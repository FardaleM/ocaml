/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      Emmanuel Arrighi, Nomadic Labs                                    */
/*                                                                        */
/*   Copyright 2022 Nomadic Labs <contact@nomadic-labs.com>               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/mlvalues.h"

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
void dump_minor_heap();
void dump_data();

extern char etext, edata,
    end; // Magic:https://stackoverflow.com/questions/1765969/where-are-the-symbols-etext-edata-and-end-defined

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

#ifdef NATIVE_CODE

void caml_do_full_dump(const char *filename) {
  value null, taglib;
  null = 0;

  // Opening the file for the dump
  fp = fopen(filename, "w");

  // Check for error for the file
  if (fp == NULL) {
    fprintf(stderr, "File can not be opened\n");
    return;
  }

  taglib = caml_read_tag_section(Val_unit);
  fwrite(&taglib, Bsize_wsize(1), 1, fp);

  // Dump the root
  caml_do_roots(dump_roots, 1);

  // NULL word to mark the end of the roots
  fwrite(&null, Bsize_wsize(1), 1, fp);

  dump_minor_heap();

  dump_chunks();

  dump_data();

  fclose(fp);
  return;
}

#else

void caml_do_full_dump(const char *filename) { return; }

#endif

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

void dump_minor_heap() {
  value size = caml_young_end - caml_young_start;
  fwrite(&caml_young_start, Bsize_wsize(1), 1, fp);
  fwrite(&size, Bsize_wsize(1), 1, fp);
  fwrite(caml_young_start, 1, size, fp);
}

void dump_roots(value root, value *dummy) {
#ifdef NO_NAKED_POINTERS
  if (Is_block(root) &&
      !Is_young(root)) { // TODO: Why do we not scan young root?
#else
  if (Is_block(root) && Is_in_heap(root)) {
#endif
    fwrite(&root, Bsize_wsize(1), 1, fp);
  }
}

void dump_data() {
  char *data_start;
  char *data_end;
  data_start = &etext;
  data_end = &end;
  value size = data_end - data_start;
  fwrite(&data_start, Bsize_wsize(1), 1, fp);
  fwrite(&size, Bsize_wsize(1), 1, fp);
  fwrite(data_start, 1, size, fp);
}
