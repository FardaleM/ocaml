#include "caml/mlvalues.h"
#define CAML_INTERNALS

#include "caml/compact.h"
#include "caml/dump.h"
#include "caml/eventlog.h"
#include "caml/fail.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/roots.h"
#include "caml/signals.h"

/*
 * Useful stuff:
 *  - caml/major_gc.h: caml_heap_start seems to be the entry point to have the
 * heap chunk
 *  - caml/major_gc.h Chunk_ seems to be the the macro to deals with the chunk
 * in the major heap
 */

/* TODO use the function to darken all the reachable value in the dump */
/* Take a ocaml string as input and dump heap in it */
CAMLprim value caml_dump_chunks(value value_filename) {
  CAMLparam1(value_filename);

  // Compact memory first
  gc_compaction();

  FILE *fp; // File of the dump
  char *dump_chunk; // This will hold the pointer to a chunk of memory

  const char *filename = String_val(value_filename);
  fp = fopen(filename, "w");

  dump_chunk = caml_heap_start; // The pointer to the firts chunk is caml_heap_start
  if (fp == NULL) {
    fprintf(stderr, "File can not be opened\n");
    return Val_unit;
  }

  while (dump_chunk != NULL) {
    /* Dump from the start of the chunk using Chunk_block
     * Chunk_size return the byte for the given chunk
     */
    printf("Printing a chunk\n");
    fwrite(Chunk_block(dump_chunk), 1, Chunk_size(dump_chunk), fp);
    dump_chunk = Chunk_next(dump_chunk);
  }

  fclose(fp);
  return Val_unit;
}

// Copied from gc_ctrl.c
void gc_compaction() {
  value exn;

  CAML_EV_BEGIN(EV_EXPLICIT_GC_COMPACT);
  caml_gc_message(0x10, "Heap dump requested\n");
  caml_empty_minor_heap();
  caml_gc_message(0x1, "Full major GC cycle (dump)\n");
  caml_finish_major_cycle();
  // call finalisers
  exn = caml_process_pending_actions_exn();
  if (Is_exception_result(exn))
    goto cleanup;
  caml_empty_minor_heap();
  caml_finish_major_cycle();
  ++Caml_state->stat_forced_major_collections;
  caml_gc_message(0x1, "Compaction (dump)\n");
  caml_compact_heap(-1);
  // call finalisers
  exn = caml_process_pending_actions_exn();

cleanup:
  CAML_EV_END(EV_EXPLICIT_GC_COMPACT);
  caml_raise_if_exception(exn);
}
