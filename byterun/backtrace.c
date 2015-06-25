/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Stack backtrace for uncaught exceptions */

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "caml/config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/io.h"
#include "caml/instruct.h"
#include "caml/intext.h"
#include "caml/exec.h"
#include "caml/fix_code.h"
#include "caml/memory.h"
#include "caml/startup.h"
#include "caml/stacks.h"
#include "caml/sys.h"
#include "caml/backtrace.h"
#include "caml/fail.h"

CAMLexport int caml_backtrace_active = 0;
CAMLexport int caml_backtrace_pos = 0;
CAMLexport code_t * caml_backtrace_buffer = NULL;
value caml_backtrace_exns = Val_unit;
int * caml_backtrace_exns_pos = NULL;
int caml_backtrace_exns_cur = 0;
CAMLexport char * caml_cds_file = NULL;
#define BACKTRACE_BUFFER_SIZE 1024
#define BACKTRACE_EXN_LENGTH 16

/* Location of fields in the Instruct.debug_event record */
enum {
  EV_POS = 0,
  EV_MODULE = 1,
  EV_LOC = 2,
  EV_KIND = 3
};

/* Location of fields in the Location.t record. */
enum {
  LOC_START = 0,
  LOC_END = 1,
  LOC_GHOST = 2
};

/* Location of fields in the Lexing.position record. */
enum {
  POS_FNAME = 0,
  POS_LNUM = 1,
  POS_BOL = 2,
  POS_CNUM = 3
};

/* Start or stop the backtrace machinery */

CAMLprim value caml_record_backtrace(value vflag)
{
  int flag = Int_val(vflag);

  if (flag != caml_backtrace_active) {
    caml_backtrace_active = flag;
    caml_backtrace_pos = 0;
    if (flag) {
      caml_backtrace_exns_cur = 0;
      caml_backtrace_exns = caml_alloc((mlsize_t) BACKTRACE_EXN_LENGTH, 0);
      caml_register_global_root(&caml_backtrace_exns);
    } else {
      caml_remove_global_root(&caml_backtrace_exns);
    }
    /* Note: lazy initialization of caml_backtrace_buffer in
       caml_stash_backtrace to simplify the interface with the thread
       libraries */
  }
  return Val_unit;
}

/* Return the status of the backtrace machinery */

CAMLprim value caml_backtrace_status(value vunit)
{
  return Val_bool(caml_backtrace_active);
}

/* Store the return addresses contained in the given stack fragment
   into the backtrace array */

void caml_stash_backtrace(value exn, code_t pc, value * sp, int reraise)
{
  code_t end_code = (code_t) ((char *) caml_start_code + caml_code_size);
  if (pc != NULL) pc = pc - 1;
  if (!reraise) {
    caml_backtrace_pos = 0;
  }
  if (caml_backtrace_buffer == NULL) {
    Assert(caml_backtrace_pos == 0);
    caml_backtrace_buffer = malloc(BACKTRACE_BUFFER_SIZE * sizeof(code_t));
    caml_backtrace_exns_pos = malloc(BACKTRACE_EXN_LENGTH * sizeof(int));
    if (caml_backtrace_buffer == NULL ||
        caml_backtrace_exns_pos == NULL)
      return;
  }

  if (caml_backtrace_pos == 0) {
    int i;
    for (i = 0; i < caml_backtrace_exns_cur; ++i) {
      Field(caml_backtrace_exns, i) = Val_unit;
    }
    caml_backtrace_exns_cur = 0;
  }

  if (/* There is still room for exceptions: */
      (caml_backtrace_exns_cur < BACKTRACE_EXN_LENGTH) &&
      /* And the current exception is different from last stored: */
      (caml_backtrace_exns_cur == 0 ||
       Field(caml_backtrace_exns, caml_backtrace_exns_cur - 1) != exn))
  {
    Store_field(caml_backtrace_exns, caml_backtrace_exns_cur, exn);
    caml_backtrace_exns_pos[caml_backtrace_exns_cur] = caml_backtrace_pos;
    caml_backtrace_exns_cur += 1;
  }

  if (caml_backtrace_pos >= BACKTRACE_BUFFER_SIZE) return;
  if (pc >= caml_start_code && pc < end_code){
    /* testing the code region is needed: PR#1554 */
    caml_backtrace_buffer[caml_backtrace_pos++] = pc;
  }
  for (/*nothing*/; sp < caml_trapsp; sp++) {
    code_t p = (code_t) *sp;
    if (p >= caml_start_code && p < end_code) {
      if (caml_backtrace_pos >= BACKTRACE_BUFFER_SIZE) break;
      caml_backtrace_buffer[caml_backtrace_pos++] = p;
    }
  }
}

/* In order to prevent the GC from walking through the debug
   information (which have no headers), we transform code pointers to
   31/63 bits ocaml integers by shifting them by 1 to the right. We do
   not lose information as code pointers are aligned.

   In particular, we do not need to use [caml_initialize] when setting
   an array element with such a value.
*/
#define Val_Codet(p) Val_long((uintnat)p>>1)
#define Codet_Val(v) ((code_t)(Long_val(v)<<1))

/* returns the next frame pointer (or NULL if none is available);
   updates *sp to point to the following one, and *trsp to the next
   trap frame, which we will skip when we reach it  */

code_t caml_next_frame_pointer(value ** sp, value ** trsp)
{
  code_t end_code = (code_t) ((char *) caml_start_code + caml_code_size);

  while (*sp < caml_stack_high) {
    code_t *p = (code_t*) (*sp)++;
    if(&Trap_pc(*trsp) == p) {
      *trsp = Trap_link(*trsp);
      continue;
    }
    if (*p >= caml_start_code && *p < end_code) return *p;
  }
  return NULL;
}

/* Stores upto [max_frames_value] frames of the current call stack to
   return to the user. This is used not in an exception-raising
   context, but only when the user requests to save the trace
   (hopefully less often). Instead of using a bounded buffer as
   [caml_stash_backtrace], we first traverse the stack to compute the
   right size, then allocate space for the trace. */

CAMLprim value caml_get_current_callstack(value max_frames_value) {
  CAMLparam1(max_frames_value);
  CAMLlocal1(trace);

  /* we use `intnat` here because, were it only `int`, passing `max_int`
     from the OCaml side would overflow on 64bits machines. */
  intnat max_frames = Long_val(max_frames_value);
  intnat trace_size;

  /* first compute the size of the trace */
  {
    value * sp = caml_extern_sp;
    value * trsp = caml_trapsp;

    for (trace_size = 0; trace_size < max_frames; trace_size++) {
      code_t p = caml_next_frame_pointer(&sp, &trsp);
      if (p == NULL) break;
    }
  }

  trace = caml_alloc(trace_size, 0);

  /* then collect the trace */
  {
    value * sp = caml_extern_sp;
    value * trsp = caml_trapsp;
    uintnat trace_pos;

    for (trace_pos = 0; trace_pos < trace_size; trace_pos++) {
      code_t p = caml_next_frame_pointer(&sp, &trsp);
      Assert(p != NULL);
      Field(trace, trace_pos) = Val_Codet(p);
    }
  }

  CAMLreturn(trace);
}

/* Read the debugging info contained in the current bytecode executable. */

#ifndef O_BINARY
#define O_BINARY 0
#endif

struct ev_info {
  code_t ev_pc;
  char * ev_filename;
  int ev_lnum;
  int ev_startchr;
  int ev_endchr;
};

static int cmp_ev_info(const void *a, const void *b) {
  code_t pc_a = ((const struct ev_info*)a)->ev_pc;
  code_t pc_b = ((const struct ev_info*)b)->ev_pc;
  if (pc_a > pc_b) return 1;
  if (pc_a < pc_b) return -1;
  return 0;
}

static char *read_debug_info_error = "";
static uintnat n_events;
static struct ev_info *events = NULL;
static void read_debug_info(void)
{
  CAMLparam0();
  CAMLlocal1(events_heap);
  char * exec_name;
  int fd;
  struct exec_trailer trail;
  struct channel * chan;
  uint32 num_events, orig, i;
  intnat j;
  value evl, l, ev_start;

  if(events != NULL)
    CAMLreturn0;

  if (caml_cds_file != NULL) {
    exec_name = caml_cds_file;
  } else {
    exec_name = caml_exe_name;
  }
  fd = caml_attempt_open(&exec_name, &trail, 1);
  if (fd < 0){
    read_debug_info_error = "executable program file not found";
    CAMLreturn0;
  }
  caml_read_section_descriptors(fd, &trail);
  if (caml_seek_optional_section(fd, &trail, "DBUG") == -1) {
    close(fd);
    read_debug_info_error = "program not linked with -g";
    CAMLreturn0;
  }
  chan = caml_open_descriptor_in(fd);
  num_events = caml_getword(chan);
  n_events = 0;
  events_heap = caml_alloc(num_events, 0);
  for (i = 0; i < num_events; i++) {
    orig = caml_getword(chan);
    evl = caml_input_val(chan);
    caml_input_val(chan); // Skip the list of absolute directory names
    /* Relocate events in event list */
    for (l = evl; l != Val_int(0); l = Field(l, 1)) {
      value ev = Field(l, 0);
      Field(ev, EV_POS) = Val_long(Long_val(Field(ev, EV_POS)) + orig);
      n_events++;
    }
    /* Record event list */
    Store_field(events_heap, i, evl);
  }
  caml_close_channel(chan);

  events = (struct ev_info*)malloc(n_events * sizeof(struct ev_info));
  if(events == NULL) {
    read_debug_info_error = "out of memory";
    CAMLreturn0;
  }

  j = 0;
  for (i = 0; i < num_events; i++) {
    for (l = Field(events_heap, i); l != Val_int(0); l = Field(l, 1)) {
      uintnat fnsz;
      value ev = Field(l, 0);

      events[j].ev_pc =
        (code_t)((char*)caml_start_code + Long_val(Field(ev, EV_POS)));

      ev_start = Field (Field (ev, EV_LOC), LOC_START);

      fnsz = caml_string_length(Field (ev_start, POS_FNAME))+1;
      events[j].ev_filename = (char*)malloc(fnsz);
      if(events[j].ev_filename == NULL) {
        for(j--; j >= 0; j--)
          free(events[j].ev_filename);
        free(events);
        events = NULL;
        read_debug_info_error = "out of memory";
        CAMLreturn0;
      }
      memcpy(events[j].ev_filename, String_val (Field (ev_start, POS_FNAME)),
             fnsz);

      events[j].ev_lnum = Int_val (Field (ev_start, POS_LNUM));
      events[j].ev_startchr =
        Int_val (Field (ev_start, POS_CNUM))
        - Int_val (Field (ev_start, POS_BOL));
      events[j].ev_endchr =
        Int_val (Field (Field (Field (ev, EV_LOC), LOC_END), POS_CNUM))
        - Int_val (Field (ev_start, POS_BOL));

      j++;
    }
  }

  Assert(j == n_events);

  qsort(events, n_events, sizeof(struct ev_info), cmp_ev_info);

  CAMLreturn0;
}

/* Search the event index for the given PC.  Return -1 if not found. */

static intnat event_for_location(code_t pc)
{
  uintnat low = 0, high = n_events;
  Assert(pc >= caml_start_code && pc < caml_start_code + caml_code_size);
  Assert(events != NULL);
  while(low+1 < high) {
    uintnat m = (low+high)/2;
    if(pc < events[m].ev_pc) high = m;
    else low = m;
  }
  if(events[low].ev_pc == pc)
    return low;
  /* ocamlc sometimes moves an event past a following PUSH instruction;
     allow mismatch by 1 instruction. */
  if(events[low].ev_pc == pc + 1)
    return low;
  if(low+1 < n_events && events[low+1].ev_pc == pc + 1)
    return low+1;
  return -1;
}

/* Extract location information for the given PC */

struct loc_info {
  int loc_valid;
  int loc_is_raise;
  char * loc_filename;
  int loc_lnum;
  int loc_startchr;
  int loc_endchr;
};

static void extract_location_info(code_t pc,
                                  /*out*/ struct loc_info * li)
{
  intnat ev = event_for_location(pc);
  li->loc_is_raise = caml_is_instruction(*pc, RAISE) ||
    caml_is_instruction(*pc, RERAISE);
  if (ev == -1) {
    li->loc_valid = 0;
    return;
  }
  li->loc_valid = 1;
  li->loc_filename = events[ev].ev_filename;
  li->loc_lnum = events[ev].ev_lnum;
  li->loc_startchr = events[ev].ev_startchr;
  li->loc_endchr = events[ev].ev_endchr;
}

/* Print location information -- same behavior as in Printexc */

static void print_location(struct loc_info * li, int index)
{
  char * info;

  /* Ignore compiler-inserted raise */
  if (!li->loc_valid && li->loc_is_raise) return;

  if (li->loc_is_raise) {
    /* Initial raise if index == 0, re-raise otherwise */
    if (index == 0)
      info = "Raised at";
    else
      info = "Re-raised at";
  } else {
    if (index == 0)
      info = "Raised by primitive operation at";
    else
      info = "Called from";
  }
  if (! li->loc_valid) {
    fprintf(stderr, "%s unknown location\n", info);
  } else {
    fprintf (stderr, "%s file \"%s\", line %d, characters %d-%d\n",
             info, li->loc_filename, li->loc_lnum,
             li->loc_startchr, li->loc_endchr);
  }
}

/* Print a backtrace */

CAMLexport void caml_print_exception_backtrace(void)
{
  int i, exn_pos;
  struct loc_info li;
  value exn = Val_unit;

  read_debug_info();
  if (events == NULL) {
    fprintf(stderr, "(Cannot print stack backtrace: %s)\n",
            read_debug_info_error);
    return;
  }

  for (i = 0, exn_pos = 0; i < caml_backtrace_pos; i++) {
    extract_location_info(caml_backtrace_buffer[i], &li);
    if (exn_pos < caml_backtrace_exns_cur && caml_backtrace_exns_pos[exn_pos] == i) {
      exn = Field(caml_backtrace_exns, exn_pos);
      exn_pos += 1;
    } else {
      exn = Val_unit;
    }
    print_location(&li, i);
  }
}

/* Convert the backtrace to a data structure usable from OCaml */

CAMLprim value caml_convert_raw_backtrace_slot(value backtrace_slot) {
  CAMLparam1(backtrace_slot);
  CAMLlocal4(p, fname, codeptr, exn);
  struct loc_info li;

  read_debug_info();
  if (events == NULL)
    caml_failwith(read_debug_info_error);

  if (Is_long(backtrace_slot)) {
    codeptr = backtrace_slot;
    exn = Val_unit; /* None */
  } else {
    codeptr = Field(backtrace_slot, 0);
    exn = caml_alloc_small(1, 0); /* Some */
    Store_field(exn, 0, Field(backtrace_slot, 1));
  }

  extract_location_info(Codet_Val(codeptr), &li);

  if (li.loc_valid) {
    fname = caml_copy_string(li.loc_filename);
    p = caml_alloc_small(6, 0);
    Field(p, 0) = Val_bool(li.loc_is_raise);
    Field(p, 1) = fname;
    Field(p, 2) = Val_int(li.loc_lnum);
    Field(p, 3) = Val_int(li.loc_startchr);
    Field(p, 4) = Val_int(li.loc_endchr);
    Field(p, 5) = exn;
  } else {
    p = caml_alloc_small(2, 1);
    Field(p, 0) = Val_bool(li.loc_is_raise);
    Field(p, 1) = exn;
  }

  CAMLreturn(p);
}

/* Get a copy of the latest backtrace */

CAMLprim value caml_get_exception_raw_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal2(res, pair);
  CAMLlocalN(saved_exns, BACKTRACE_EXN_LENGTH);

  int i, saved_pos, saved_exns_cur, exn_pos;
  code_t saved_buffer[BACKTRACE_BUFFER_SIZE];
  int saved_exns_pos[BACKTRACE_EXN_LENGTH];

  /* Beware: the allocations below may cause finalizers to be run, and another
     backtrace---possibly of a different length---to be stashed (for example
     if the finalizer raises then catches an exception).  We choose to ignore
     any such finalizer backtraces and return the original one. */
  saved_pos = caml_backtrace_pos;
  if (saved_pos > BACKTRACE_BUFFER_SIZE) {
    saved_pos = BACKTRACE_BUFFER_SIZE;
  }

  /* exceptions are ocaml values: if backtrace is no longer active, they may have get
   * GCed (while code pointers are still valid).
   */
  saved_exns_cur = caml_backtrace_active ? caml_backtrace_exns_cur : 0;

  for (i = 0; i < saved_pos; i++) {
    saved_buffer[i] = caml_backtrace_buffer[i];
  }
  for (i = 0; i < saved_exns_cur; i++) {
    saved_exns_pos[i] = caml_backtrace_exns_pos[i];
    saved_exns[i] = Field(caml_backtrace_exns, i);
  }

  res = caml_alloc(saved_pos, 0);
  for(i = 0, exn_pos = 0; i < saved_pos; i++) {
    if (exn_pos < saved_exns_cur && saved_exns_pos[exn_pos] == i) {
      pair = caml_alloc_small(2, 0);
      Field(pair, 0) = Val_Codet(saved_buffer[i]);
      Field(pair, 1) = Field(saved_exns, exn_pos);
      exn_pos += 1;
      Store_field(res, i, pair);
    } else {
      Field(res, i) = Val_Codet(saved_buffer[i]);
    }
  }

  CAMLreturn(res);
}

/* the function below is deprecated: we previously returned directly
   the OCaml-usable representation, instead of the raw backtrace as an
   abstract type, but this has a large performance overhead if you
   store a lot of backtraces and print only some of them.

   It is not used by the Printexc library anymore, or anywhere else in
   the compiler, but we have kept it in case some user still depends
   on it as an external.
*/

CAMLprim value caml_get_exception_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal4(arr, raw_slot, slot, res);

  read_debug_info();
  if (events == NULL) {
      res = Val_int(0); /* None */
  } else {
      arr = caml_alloc(caml_backtrace_pos, 0);
      if(caml_backtrace_buffer == NULL) {
          Assert(caml_backtrace_pos == 0);
      } else {
          intnat i;
          for(i = 0; i < caml_backtrace_pos; i++) {
              raw_slot = Val_Codet(caml_backtrace_buffer[i]);
              /* caml_convert_raw_backtrace_slot will not fail with
               caml_failwith as we checked (events != NULL) already */
              slot = caml_convert_raw_backtrace_slot(raw_slot);
              caml_modify(&Field(arr, i), slot);
          }
      }
      res = caml_alloc_small(1, 0);
      Field(res, 0) = arr; /* Some */
  }
  CAMLreturn(res);
}
