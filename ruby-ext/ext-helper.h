/* ext-helper.h
**
** Author:  Yoshinari Nomura <nom@quickhack.net>
**
** Created: 1999/09/01
** Revised: $Date: 2000/05/29 14:59:25 $
**
*/

#ifndef EXT_HELPER_H
#define EXT_HELPER_H

#include "ruby.h"
#include <time.h>

#ifdef NEW_NAMING
#define cObject    rb_cObject
#define TRUE       Qtrue
#define FALSE      Qfalse
#define time_new   rb_time_new
#define str_new    rb_str_new
#define str_new2   rb_str_new2
#define ary_new    rb_ary_new
#define ary_new2   rb_ary_new2
#define ary_new3   rb_ary_new3
#define ary_push   rb_ary_push
#define ary_shift  rb_ary_shift
#define obj_call_init rb_obj_call_init
#define Fail(x)    rb_raise(rb_eRuntimeError, x)
#define TypeError(x)  rb_raise(rb_eTypeError, x)
#endif

#if 0
#  define dprintf(x)  printf x
#else
#  define dprintf(x)
#endif

#define ARRAY_LEN(x)     (sizeof(x)/sizeof(x[0]))

/****************************************************************/
/**********  set C values to ruby object     ********************/
/****************************************************************/

#define iv_set1(obj, fmt, name)     cp_set1(obj, fmt, #name, &(name))
#define iv_set2(obj, fmt, name, n)  cp_set2(obj, fmt, #name, name, n)

#define ar_set1(ary, fmt, name)     cp_set1(ary, fmt, NULL, &(name))
#define ar_set2(ary, fmt, name, n)  cp_set2(ary, fmt, NULL, name, n)

#define iv_get1(obj, fmt, name)     cp_get1(obj, fmt, #name, &(name))
#define iv_get2(obj, fmt, name, n)  cp_get2(obj, fmt, #name, name, n)

#define ar_get1(ary, fmt, name)     cp_get1(ary, fmt, NULL, &(name))
#define ar_get2(ary, fmt, name, n)  cp_get2(ary, fmt, NULL, name, n)

extern int STRING_LENGTH;
extern int IS_CLASS_OF(VALUE obj, char *class_name);

extern VALUE TM2TIME(struct tm *tp);
extern void  TIME2TM(VALUE obj, struct tm* dst);

extern VALUE ary_copy(VALUE dst, VALUE src);

extern char *iv_conv_name(char *name, char *ret);
extern int  cp_set1(VALUE obj, char *fmt, char *ivname, void *cval);
extern void cp_set2(VALUE obj, char *fmt, char *ivname, void *cval, int len);
extern int  cp_get1(VALUE obj, char *fmt, char *ivname, void *cval);
extern void cp_get2(VALUE obj, char *fmt, char *ivname, void *cval, int len);

#endif /* ifndef EXT_HELPER_H */

/*
*** Copyright Notice:
**
** Copyright (C) 1999, 2000 Yoshinari Nomura. All rights reserved.
** Copyright (C) 2000 MHC developing team. All rights reserved.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions
** are met:
**
** 1. Redistributions of source code must retain the above copyright
**    notice, this list of conditions and the following disclaimer.
** 2. Redistributions in binary form must reproduce the above copyright
**    notice, this list of conditions and the following disclaimer in the
**    documentation and/or other materials provided with the distribution.
** 3. Neither the name of the team nor the names of its contributors
**    may be used to endorse or promote products derived from this software
**    without specific prior written permission.
**
** THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS''
** AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
** LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
** FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
** THE TEAM OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
** INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
** (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
** SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
** HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
** STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
** ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
** OF THE POSSIBILITY OF SUCH DAMAGE.
**
*** ext-helper.h ends here
*/
