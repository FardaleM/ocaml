#include "caml/mlvalues.h"
#define CAML_INTERNALS

#include "caml/compact.h"
#include "caml/eventlog.h"
#include "caml/fail.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/roots.h"
#include "caml/signals.h"

void gc_compaction();
void dump_roots(value root, value *dummy);
void dump_chunks();

static FILE *fp; // File of the dump

/*
 * Useful stuff:
 *  - caml/major_gc.h: caml_heap_start seems to be the entry point to have the
 * heap chunk
 *  - caml/major_gc.h Chunk_ seems to be the the macro to deals with the chunk
 * in the major heap
 */

/* TODO use the function to darken all the reachable value in the dump */
/* Take a ocaml string as input and dump the memory in it */
CAMLprim value caml_full_dump(value value_filename) {
  CAMLparam1(value_filename);

  // Opening the file for the dump
  const char *filename = String_val(value_filename);
  fp = fopen(filename, "w");

  // Check for error for the file
  if (fp == NULL) {
    fprintf(stderr, "File can not be opened\n");
    return Val_unit;
  }

  // Compact memory first
  gc_compaction();

  // Dump the root
  caml_do_roots(dump_roots, 1);

  // NULL word to mark the end of the roots
  value null = 0;
  fwrite(&null, Bsize_wsize(1), 1, fp);

  dump_chunks();

  fclose(fp);
  return Val_unit;
}

// TODO dump address and length first
void dump_chunks() {
  char *dump_chunk; // This will hold the pointer to a chunk of memory

  dump_chunk =
      caml_heap_start; // The pointer to the firts chunk is caml_heap_start

  while (dump_chunk != NULL) {
    /* Dump from the start of the chunk using Chunk_block
     * Chunk_size return the size in byte for the given chunk
     * Dump address, size and content
     */
    fwrite(&Chunk_block(dump_chunk), Bsize_wsize(1), 1, fp);
    fwrite(&Chunk_size(dump_chunk), Bsize_wsize(1), 1, fp);
    fwrite(Chunk_block(dump_chunk), 1, Chunk_size(dump_chunk), fp);
    dump_chunk = Chunk_next(dump_chunk);
  }
}

void dump_roots(value root, value *dummy) {
#ifdef NO_NAKED_POINTERS
  if (Is_block(root) && !Is_young(root)) {
#else
  if (Is_block(root) && Is_in_heap(root)) {
#endif
    fwrite(&root, Bsize_wsize(1), 1, fp);
  }
}

// Look at sweep_slice in major_gc.c to know how to do it
CAMLprim value caml_show_tag(value v) {
  CAMLassert(v == Val_unit);
  char *chunk, *hp;
  chunk = caml_heap_start;

  while (chunk != NULL) {
    hp = chunk;
    chunk = Chunk_next(chunk);
  }
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
