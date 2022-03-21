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

CAMLprim value caml_dump_chunks(value value_filename) {
  CAMLparam1(value_filename);

  // Compact memory first
  //gc_compaction();

  FILE *fp;
  char *dump_chunk;

  const char *filename = String_val(value_filename);
  fp = fopen(filename, "w");

  dump_chunk = caml_heap_start;
  if (fp == NULL) {
    fprintf(stderr, "File can not be opened\n");
    return Val_unit;
  }

  while (dump_chunk != NULL) {
    fwrite(Chunk_block(dump_chunk), (size_t)Bsize_wsize(1),
           Chunk_size(dump_chunk), fp);
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
  caml_gc_message(0x1, "Full major GC cycle (compaction)\n");
  caml_finish_major_cycle();
  // call finalisers
  exn = caml_process_pending_actions_exn();
  if (Is_exception_result(exn))
    goto cleanup;
  caml_empty_minor_heap();
  caml_finish_major_cycle();
  ++Caml_state->stat_forced_major_collections;
  caml_compact_heap(-1);
  // call finalisers
  exn = caml_process_pending_actions_exn();

cleanup:
  CAML_EV_END(EV_EXPLICIT_GC_COMPACT);
  caml_raise_if_exception(exn);
}
