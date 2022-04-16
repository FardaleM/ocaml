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

void dump_roots(value root, value *dummy);
void dump_chunks();

uintnat caml_dump_after_compact = 0;
/* Control the dump after a compaction.
   This is set when parsing [OCAMLRUNPARAM]
*/

static FILE *fp; // File of the dump

/*
 * Useful stuff:
 *  - caml/major_gc.h: caml_heap_start seems to be the entry point to have the
 * heap chunk
 *  - caml/major_gc.h Chunk_ seems to be the the macro to deals with the chunk
 * in the major heap
 */

void caml_do_full_dump(const char *filename) {
  // Opening the file for the dump
  fp = fopen(filename, "w");

  // Check for error for the file
  if (fp == NULL) {
    fprintf(stderr, "File can not be opened\n");
    return;
  }

  // Dump the root
  caml_do_roots(dump_roots, 1);

  // NULL word to mark the end of the roots
  value null = 0;
  fwrite(&null, Bsize_wsize(1), 1, fp);

  dump_chunks();

  fclose(fp);
  return;
}

/* Take a ocaml string as input and dump the memory in it */
CAMLprim value caml_full_dump(value value_filename) {
  CAMLparam1(value_filename);

  // Opening the file for the dump
  const char *filename = String_val(value_filename);

  caml_do_full_dump(filename);

  CAMLreturn(Val_unit);
}

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
