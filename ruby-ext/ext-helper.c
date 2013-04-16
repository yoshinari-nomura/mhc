/* ext-helper.c
**
** Author:  Yoshinari Nomura <nom@quickhack.net>
**
** Created: 1999/09/01
** Revised: $Date: 2000/05/29 14:59:25 $
**
**
*/

#include "ruby.h"
#include "ext-helper.h"

#include <time.h>
#include <stdarg.h>
#include <ctype.h>

int STRING_LENGTH = 0;

#define cp_Check_Type(c, c_type_id, message) {\
  if (c != c_type_id) TypeError(message); \
}

/******************************************************************/
/******************** Common definitions   ************************/
/******************************************************************/

int IS_CLASS_OF(VALUE obj, char *class_name)
{
  char *class_name2 = rb_class2name(CLASS_OF(obj));

  return (strcmp(class_name2, class_name) == 0) ? 1 : 0;
}

/******************************************************************/
/******************** Time <-> struct tm   ************************/
/******************************************************************/

VALUE TM2TIME(struct tm *tp)
{
  time_t sec;

  dprintf(("%d-%d-%d  %d:%d:%d\n", tp->tm_year, tp->tm_mon, tp->tm_mday,
         tp->tm_hour, tp->tm_min, tp->tm_sec));

  /* check -- sometimes, mktime() takes long time with a messy tm */
  if ((0  <= tp->tm_sec  && tp->tm_sec  <= 60) &&
      (0  <= tp->tm_min  && tp->tm_min  <  60) &&
      (0  <= tp->tm_hour && tp->tm_hour <  24) &&
      (1  <= tp->tm_mday && tp->tm_mday <= 31) &&
      (0  <= tp->tm_mon  && tp->tm_mon  <= 11) &&
      (70 <= tp->tm_year && tp->tm_year <= 137)){

    sec = mktime(tp);
  } else {
    sec = 0;  /* xxx */
  }
  if (sec < 0){
    sec = 0; /* xxx: ruby Time class does not deal with a minus sec. */
  }
  dprintf(("sec: %d\n", sec));
  return time_new(sec, 0);
}

void TIME2TM(VALUE obj, struct tm* dst)
{
  time_t sec;
  struct tm *tmp;

  sec = NUM2ULONG(rb_funcall(obj, rb_intern("tv_sec"), 0));
  tmp = localtime(&sec);
  dprintf(("before memcpy dst: %d\n", dst));
  memcpy(dst, tmp, sizeof(struct tm));
  dprintf(("after memcpy\n"));
}

#define IS_TIME(obj)     IS_CLASS_OF(obj, "Time")

/****************************************************************/
/*************** array operations      **************************/
/****************************************************************/

VALUE ary_copy(VALUE dst, VALUE src)
{
  int i;

  Check_Type(src, T_ARRAY);
  Check_Type(dst, T_ARRAY);

  for (i = 0; i < RARRAY(src)->len; i++){
    ary_push(dst, RARRAY(src)->ptr[i]);
  }
  return dst;
}

/****************************************************************/
/**********  set C values to ruby object     ********************/
/****************************************************************/

/****************************************************************
  "app.repeatType" のような C 構造体の参照名から、
  "@repeatType" のような ruby のインスタンス変数名を得る。
****************************************************************/
char *iv_conv_name(char *name, char *ret)
{
  int c, i, len, pos = 0;

  if (name == NULL) return NULL;

  len = strlen(name);
  for (i = 0; i < len; i++){
    c =  *(name + i);
    if (!isalpha(c) && !isdigit(c) && c != '_'){
      pos = i + 1;
    }
  }
  *ret = '@';
  strcpy(ret + 1, name + pos);
  return ret;
}

/****************************************************************
  obj の ivname というインスタンス変数に *cval を代入する
  ivname が NULL の場合は、obj を配列とみなし、push する
  cval から ruby VALUE への変換ヒントは fmt で与える。
****************************************************************/
int cp_set1(VALUE obj, char *fmt, char *ivname, void *cval)
{
  char at_name[100];
  int len, c;
  VALUE val;

  switch (*fmt){
  case 'b':
    dprintf(("converting %d into BOOL\n", *(int*)cval));
    val = (*(int*)cval ? TRUE : FALSE);
    len = sizeof(int);
    break;
  case 'c':
    dprintf(("converting %d into uchar\n", *(unsigned char*)cval));
    val = INT2FIX(*(unsigned char *)cval);
    len = sizeof(unsigned char);
    break;
  case 'i':
    dprintf(("converting %d into FIXNUM\n", *(int*)cval));
    val = INT2FIX(*(int*)cval);
    len = sizeof(int);
    break;
  case 't':
    dprintf(("converting %d into Time\n", ((struct tm*)cval)->tm_year));
    val = TM2TIME((struct tm*)cval);
    len = sizeof(struct tm);
    break;
  case 's':
    dprintf(("converting into String\n"));
    if ((c = atoi(fmt + 1)) > 0){
      val = str_new2((char*)cval != NULL ? (char*)cval : "");
      len = sizeof(char) * c;
    } else {
      val = str_new2(*(char**)cval != NULL ? *(char**)cval : "");
      len = sizeof(char*);
    }
    break;
  case 'v':
    dprintf(("converting %d into VALUE\n", (VALUE*)cval));
    val = *(VALUE*)cval;
    len = sizeof(VALUE);
    break;
  default:
    TypeError(ivname != NULL ? ivname : "???");
  }

  if (ivname == NULL) {
    Check_Type(obj, T_ARRAY);
    ary_push(obj, val);
  } else {
    iv_conv_name(ivname, at_name);
    rb_iv_set(obj, at_name, val);
  }

  return len;
}

/****************************************************************
  新しく ruby 配列 x を作る。cval を C の配列(0番目を指すポインタ)
  だと思って cp_set1 でx に対してどんどん push。
  obj の インスタンス変数 ivname に x をセット。
  ivname が NULL の場合は、obj を配列とみなし obj に x を push。
****************************************************************/
void cp_set2(VALUE obj, char *fmt, char *ivname, void *cval, int len)
{
  int i, s;
  VALUE ary = ary_new();

  for (i = 0; i < len; i++){
    cval += cp_set1(ary, fmt, NULL, cval);
  }
  cp_set1(obj, "v", ivname, &ary);
}

/****************************************************************/
/********** set ruby obects to C variables   ********************/
/****************************************************************/

/****************************************************************
****************************************************************/

int cp_get1(VALUE obj, char *fmt, char *ivname, void *cval)
{
  int len;
  char at_name[100];
  VALUE val;

  at_name[0] = '\0';

  if (ivname == NULL){
    Check_Type(obj, T_ARRAY);
    val = ary_shift(obj);
  } else {
    iv_conv_name(ivname, at_name);
    val = rb_iv_get(obj, at_name);
  }

  dprintf(("converting name:%s class:%s\n",
           at_name == NULL ? "???" : at_name,
           rb_class2name(CLASS_OF(val))));

  switch (TYPE(val)){
  case T_TRUE:
  case T_FALSE:
    cp_Check_Type(*fmt, 'b', at_name);
    *(int*)cval = (TYPE(val) == T_TRUE ? 1 : 0);
    len = sizeof(int);
    break;
  case T_FIXNUM:
    if (*fmt == 'i'){
      cp_Check_Type(*fmt, 'i', at_name);
      *(int*)cval = FIX2INT(val);
      len = sizeof(int);
    } else if (*fmt == 'c'){
      *(unsigned char *)cval = FIX2INT(val);
      len = sizeof(unsigned char);
    } else {
      TypeError(at_name);
    }
    break;
  case T_STRING:
    cp_Check_Type(*fmt, 's', at_name);
    STRING_LENGTH = RSTRING(val)->len;
    if (STRING_LENGTH > 0){
      *(char**)cval = STR2CSTR(val);
    } else {
      *(char**)cval = NULL;
    }
    len = sizeof(char*);
    break;
  case T_ARRAY:
    cp_Check_Type(*fmt, 'v', at_name);
    *(VALUE*)cval = val;
    len = sizeof(VALUE);
    break;
  default:
    dprintf(("converted Time\n"));
    cp_Check_Type(*fmt, 't', at_name);
    dprintf(("converted Time\n"));
    if (!IS_TIME(val)){TypeError("Time required");}
    dprintf(("converted Time cval: %d\n", cval));
    TIME2TM(val, (struct tm*)cval);
    dprintf(("converted Time\n"));
    len = sizeof(struct tm);
    dprintf(("converted Time\n"));
    break;
  }
  return len;
}

void cp_get2(VALUE obj, char *fmt, char *ivname, void *cval, int len)
{
  int i;
  VALUE val, ary;
  char at_name[100];
  ary = ary_new();

  cp_get1(obj, "v", ivname, &val);
  Check_Type(val, T_ARRAY);
  ary_copy(ary, val);

  dprintf(("cp_get2: %d(length) cval:%d\n", len, cval));
  for (i = 0; i < len; i++){
    cval += cp_get1(ary, fmt, NULL, cval);
  }
}

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
*** ext-helper.c ends here
*/
