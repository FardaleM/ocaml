/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"
#include "socketaddr.h"

static int msg_flag_table[] = {
  MSG_OOB, MSG_DONTROUTE, MSG_PEEK
};

CAMLprim value unix_recv(value sock, value buff, value ofs, value len,
                         value flags)
{
  CAMLparam1(buff);
  SOCKET s = Socket_val(sock);
  int flg = caml_convert_flag_list(flags, msg_flag_table);
  int ret;
  intnat numbytes;
  char iobuf[UNIX_BUFFER_SIZE];
  DWORD err = 0;

  numbytes = Long_val(len);
  if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
  caml_enter_blocking_section();
  ret = recv(s, iobuf, (int) numbytes, flg);
  if (ret == -1) err = WSAGetLastError();
  caml_leave_blocking_section();
  if (ret == -1) {
    win32_maperr(err);
    uerror("recv", Nothing);
  }
  memmove (&Byte(buff, Long_val(ofs)), iobuf, ret);
  CAMLreturn(Val_int(ret));
}

CAMLprim value unix_recvfrom(value sock, value buff, value ofs, value len,
                             value flags)
{
  CAMLparam1(buff);
  CAMLlocal1(adr);
  SOCKET s = Socket_val(sock);
  int flg = caml_convert_flag_list(flags, msg_flag_table);
  int ret;
  intnat numbytes;
  char iobuf[UNIX_BUFFER_SIZE];
  value res;
  union sock_addr_union addr;
  socklen_param_type addr_len;
  DWORD err = 0;

  numbytes = Long_val(len);
  if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
  addr_len = sizeof(addr);
  caml_enter_blocking_section();
  ret = recvfrom(s, iobuf, (int) numbytes, flg, &addr.s_gen, &addr_len);
  if (ret == -1) err = WSAGetLastError();
  caml_leave_blocking_section();
  if (ret == -1) {
    win32_maperr(err);
    uerror("recvfrom", Nothing);
  }
  memmove (&Byte(buff, Long_val(ofs)), iobuf, ret);
  adr = alloc_sockaddr(&addr, addr_len, -1);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_int(ret);
  Field(res, 1) = adr;
  CAMLreturn(res);
}

CAMLprim value unix_send(value sock, value buff, value ofs, value len,
                         value flags)
{
  SOCKET s = Socket_val(sock);
  int flg = caml_convert_flag_list(flags, msg_flag_table);
  int ret;
  intnat numbytes;
  char iobuf[UNIX_BUFFER_SIZE];
  DWORD err = 0;

  numbytes = Long_val(len);
  if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
  memmove (iobuf, &Byte(buff, Long_val(ofs)), numbytes);
  caml_enter_blocking_section();
  ret = send(s, iobuf, (int) numbytes, flg);
  if (ret == -1) err = WSAGetLastError();
  caml_leave_blocking_section();
  if (ret == -1) {
    win32_maperr(err);
    uerror("send", Nothing);
  }
  return Val_int(ret);
}

value unix_sendto_native(value sock, value buff, value ofs, value len,
                         value flags, value dest)
{
  SOCKET s = Socket_val(sock);
  int flg = caml_convert_flag_list(flags, msg_flag_table);
  int ret;
  intnat numbytes;
  char iobuf[UNIX_BUFFER_SIZE];
  union sock_addr_union addr;
  socklen_param_type addr_len;
  DWORD err = 0;

  get_sockaddr(dest, &addr, &addr_len);
  numbytes = Long_val(len);
  if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
  memmove (iobuf, &Byte(buff, Long_val(ofs)), numbytes);
  caml_enter_blocking_section();
  ret = sendto(s, iobuf, (int) numbytes, flg, &addr.s_gen, addr_len);
  if (ret == -1) err = WSAGetLastError();
  caml_leave_blocking_section();
  if (ret == -1) {
    win32_maperr(err);
    uerror("sendto", Nothing);
  }
  return Val_int(ret);
}

CAMLprim value unix_sendto(value * argv, int argc)
{
  return unix_sendto_native
           (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}
