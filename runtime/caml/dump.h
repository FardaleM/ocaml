#ifndef CAML_DUMP_H
#define CAML_DUMP_H

#ifdef CAML_INTERNALS

extern uintnat caml_dump_after_compact;

void caml_do_full_dump(const char* filename);

#endif /* CAML_INTERNALS */

#endif /* CAML_DUMP_H */
