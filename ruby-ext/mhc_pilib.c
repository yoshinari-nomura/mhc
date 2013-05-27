/* mhc_pilib.c
**
** Author:  Yoshinari Nomura <nom@quickhack.net>
**
** Created: 1999/09/01
** Revised: $Date: 2008/09/29 00:39:11 $
**
*/

/*************************************************************/
/********************* PiLib Module **************************/
/*************************************************************/

#include "ruby.h"
#include "ext-helper.h"

#include "pi-source.h"
#include "pi-socket.h"
#include "pi-file.h"
#include "pi-dlp.h"
#include "pi-todo.h"
#include "pi-datebook.h"
#include "pi-version.h"
#include "pi-address.h"
#include "pi-appinfo.h"

VALUE mPiLib;

/*************************************************************/
/******************** connection management ******************/
/*************************************************************/

/* open socket, return descriptor or nil */
static VALUE rpi_sock_open(VALUE obj, VALUE dev)
{
  int sd, ret;

  sd = pi_socket(0, PI_SOCK_STREAM, 0);
  if (sd == -1)return Qnil;

  ret = pi_bind(sd, StringValuePtr(dev));
  if (ret < 0) return Qnil;

  return INT2FIX(sd);
}

/* listen socket, return new descriptor or nil */
static VALUE rpi_sock_listen(VALUE obj, VALUE rb_sd)
{
  int sd = FIX2INT(rb_sd);
  struct SysInfo     sys_info;
  struct PilotUser   user;

  if (pi_listen(sd, 1) < 0)              return Qnil;
  if ((sd = pi_accept(sd, 0, 0)) < 0)    return Qnil;

  /* We must do this to take care of the password being required to sync
   * on Palm OS 4.x */
  if (dlp_ReadSysInfo(sd, &sys_info) < 0)return Qnil;

  if (dlp_ReadUserInfo(sd, &user) < 0)   return Qnil;
  if (dlp_OpenConduit(sd) < 0)           return Qnil;

  return INT2FIX(sd);
}

/* close socket, return nil or breakup */
static VALUE rpi_sock_close(VALUE obj, VALUE rb_sd)
{
  int sd = FIX2INT(rb_sd);

  dlp_CloseDB_All(sd);
  dlp_EndOfSync(sd, 0);
  pi_close(sd);

  return Qnil;
}

/* Add an entry into the HotSync log on the Pilot.
   \n is OK, as usual. You may invoke this command once or more before
   calling EndOfSync (sockClose), but it is not required. */
static VALUE rdlp_AddSyncLogEntry(VALUE obj, VALUE sd, VALUE str)
{
  Check_Type(str, T_STRING);

  if (dlp_AddSyncLogEntry(INT2FIX(sd), RSTRING_PTR(str)) < 0)
    return Qnil;

  return Qtrue;
}

/* Convenience function to reset lastSyncPC in the UserInfo to 0 */
static VALUE rdlp_ResetLastSyncPC(VALUE obj, VALUE sd)
{
  if (dlp_ResetLastSyncPC(FIX2INT(sd)) < 0)  return Qnil;
  return Qtrue;
}

/****************************************************************/
/********************* System Information ***********************/
/****************************************************************/

/* Get the time on the Pilot and return it as a local Time value. */
static VALUE rdlp_GetSysDateTime(VALUE obj, VALUE sd)
{
  time_t time;

  if (dlp_GetSysDateTime(FIX2INT(sd), &time) < 0)
    return Qnil;

  return time_new(time, 0);
}

/* Set the time on the Pilot using a local Time value. */
static VALUE rdlp_SetSysDateTime(VALUE obj, VALUE sd, VALUE time)
{
  time_t sec = NUM2ULONG(rb_funcall(time, rb_intern("tv_sec"), 0));

  if (dlp_SetSysDateTime(FIX2INT(sd), sec) < 0)
    return Qnil;

  return Qtrue;
}

/* Ask the pilot who it is. */
static VALUE rdlp_ReadUserInfo(VALUE obj, VALUE sd, VALUE ary)
{
  return Qnil;
}

/* Tell the pilot who it is. */
static VALUE rdlp_WriteUserInfo(VALUE obj, VALUE sd, VALUE ary)
{
  return Qnil;
}

/****************************************************************/
/******************** open, close DB ****************************/
/****************************************************************/

/* Open a database on the Pilot, return db handler or nil.
   name is the ASCII name of the DB.
   access mode is always Read/Write for now.
   Mode:  Read = 0x80, Write = 0x40, Exclusive = 0x20, ShowSecret = 0x10
   */
static VALUE rdlp_OpenDB(VALUE obj, VALUE sd, VALUE name)
{
  int db;

  Check_Type(name, T_STRING);

  /* xxx: cardno is always zero for now. */
  if (dlp_OpenDB(FIX2INT(sd), 0, 0x80|0x40, StringValuePtr(name), &db) < 0)
    return Qnil;

  return INT2FIX(db);
}

/* close DB, return nil or breakup */
static VALUE rdlp_CloseDB(VALUE obj, VALUE sd, VALUE db)
{
  dlp_CloseDB(FIX2INT(sd), FIX2INT(db));
  return Qnil;
}

/****************************************************************/
/******************** Application Info       ********************/
/****************************************************************/

static VALUE rdlp_ReadAppBlock(VALUE obj, VALUE sd, VALUE db)
{
  int result;
  pi_buffer_t *buffer;
  VALUE ret;

  buffer = pi_buffer_new(0xffff);
  if (!buffer) return Qnil;

  result = dlp_ReadAppBlock(FIX2INT(sd), FIX2INT(db), 0, 0xffff, buffer);

  if (result <= 0)
    ret = Qnil;
  else
    ret = str_new(buffer->data, buffer->used);

  pi_buffer_free (buffer);
  return ret;
}

/****************************************************************/
/******************** DB Record Manipulation ********************/
/****************************************************************/

/* Read all Record in DB, even if marked as deleted or archived.
   return array of [id, attr, category, data] or nil.
   id, attr, category :  integer.
   data : String which contains DB specific data,
   it would be transformed by pack_* or unpack_* functions. */


static VALUE rdlp_ReadRecordByIndex(VALUE obj, VALUE sd, VALUE db, VALUE i)
{
  VALUE ary;
  int attr, category, len;
  recordid_t id;
  pi_buffer_t *buffer;

  buffer = pi_buffer_new(0xffff);
  if (!buffer) return Qnil;

  ary = ary_new();
  len = dlp_ReadRecordByIndex(FIX2INT(sd), FIX2INT(db), FIX2INT(i),
                              buffer, &id, &attr, &category);

  if (len <= 0) {
    pi_buffer_free (buffer);
    return Qnil;
  }

  dprintf(("id:%d atr:%d cat:%d bp:%d len:%d\n",id,attr,category,buffer,len));
  dprintf(("readrecordbyindex 0\n"));
  ar_set1(ary, "i", id);
  dprintf(("readrecordbyindex 1\n"));
  ar_set1(ary, "i", attr);
  dprintf(("readrecordbyindex 2\n"));
  ar_set1(ary, "i", category);
  dprintf(("readrecordbyindex 3\n"));
  ary_push(ary, str_new(buffer->data, len));
  dprintf(("readrecordbyindex 4\n"));

  pi_buffer_free (buffer);
  return ary;
}

static VALUE rdlp_ReadRecordById(VALUE obj, VALUE sd, VALUE db, VALUE vid)
{
  VALUE ary;
  int attr, category, len;
  int index;
  pi_buffer_t *buffer;
  recordid_t id = FIX2INT(vid);

  buffer = pi_buffer_new(0xffff);
  if (!buffer) return Qnil;

  ary = ary_new();
  len = dlp_ReadRecordById(FIX2INT(sd), FIX2INT(db), id,
                           buffer, &index, &attr, &category);

  if (len <= 0) {
    pi_buffer_free (buffer);
    return Qnil;
  }

  dprintf(("id:%d atr:%d cat:%d bp:%d len:%d\n",id,attr,category,buffer,len));
  ar_set1(ary, "i", id);
  ar_set1(ary, "i", attr);
  ar_set1(ary, "i", category);
  ary_push(ary, str_new(buffer->data, len));

  pi_buffer_free (buffer);
  return ary;
}

/* Write a record to an open database, return a new record id or nil.
   ary is a array of [id, attr, category, data]
   data : String which contains DB specific data,
   it would be transformed by pack_* or unpack_* functions. */
static VALUE rdlp_WriteRecord(VALUE obj, VALUE sd, VALUE db, VALUE ary)
{
  recordid_t id, new_id;
  int        attr, category, ret, len;
  char       *ptr;
  struct Appointment app;
  int i;
  VALUE      a = ary_new();
  pi_buffer_t *buffer;

  ary_copy(a, ary);
  ar_get1(a, "i", id);
  ar_get1(a, "i", attr);
  ar_get1(a, "i", category);
  ar_get1(a, "s", ptr);
  len = STRING_LENGTH;

  buffer = pi_buffer_new(len);
  if (!buffer) return Qnil;

  if(!pi_buffer_append(buffer, ptr, len)) {
    pi_buffer_free(buffer);
    return Qnil;
  }

  dprintf(("buf: %s\n", buffer->data));

  unpack_Appointment(&app, buffer, datebook_v1);
  dprintf(("event: %d\n", app.event));
  dprintf(("beg_year %d\n", app.begin.tm_year));
  dprintf(("Subject: %s\n", app.description));
  dprintf(("id: %d  attr: %d  cat: %d  buf_len: %d\n",
           id, attr, category, len));

  ret = dlp_WriteRecord(FIX2INT(sd), FIX2INT(db), attr,
                        id, category, buffer->data, len, &new_id);
  pi_buffer_free(buffer);

  if (ret < 0){
    dprintf(("%s\n", dlp_strerror(ret)));
    return Qnil;
  }

  dprintf(("new_id::::::: %d\n", new_id));
  return INT2FIX(new_id);
}

/* delete a record specified by the record id. */
VALUE rdlp_DeleteRecord(VALUE obj, VALUE sd, VALUE db, VALUE all, VALUE id)
{
  int all_cval;

  switch (all){
  case Qfalse:
    all_cval = 0;
    break;
  case Qtrue:
    all_cval = 1;
    break;
  default:
    rb_raise(rb_eTypeError, "`all' must be true or false");
  }
  if (dlp_DeleteRecord(FIX2INT(sd), FIX2INT(db), all_cval,  FIX2INT(id)) < 0)
    return Qnil;

  return Qtrue;
}

/* Deletes all records in the opened database which are marked as archived
   or deleted. */
static VALUE rdlp_CleanUpDatabase(VALUE obj, VALUE sd, VALUE db)
{
  if (dlp_CleanUpDatabase(FIX2INT(sd), FIX2INT(db)) < 0)
    return Qnil;

  return Qtrue;
}

/* For record databases, reset all dirty flags. For both record and
   resource databases, set the last sync time to now. */
static VALUE rdlp_ResetSyncFlags(VALUE obj, VALUE sd, VALUE db)
{
  if (dlp_ResetSyncFlags(FIX2INT(sd), FIX2INT(db)) < 0)
    return Qnil;

  return Qtrue;
}

/****************************************************************/
/******************* For Datebook Record ************************/
/****************************************************************/

static VALUE rpack_Appointment(VALUE x, VALUE ary1)
{
  struct Appointment app;
  int len, i = 0;
  VALUE ary = ary_new();
  VALUE ret;
  ary_copy(ary, ary1);
  pi_buffer_t * buffer;

  buffer = pi_buffer_new(0xffff);
  if (!buffer) return Qnil;

  ar_get1(ary, "b", app.event);
  ar_get1(ary, "t", app.begin);
  ar_get1(ary, "t", app.end);
  ar_get1(ary, "b", app.alarm);
  ar_get1(ary, "i", app.advance);
  ar_get1(ary, "i", app.advanceUnits);
  ar_get1(ary, "i", app.repeatType);
  ar_get1(ary, "b", app.repeatForever);
  ar_get1(ary, "t", app.repeatEnd);
  ar_get1(ary, "i", app.repeatFrequency);
  ar_get1(ary, "i", app.repeatDay);
  ar_get2(ary, "b", app.repeatDays, 7);
  ar_get1(ary, "i", app.repeatWeekstart);
  ar_get1(ary, "i", app.exceptions);
  app.exception = (struct tm*)malloc(app.exceptions * sizeof(struct tm));
  ar_get2(ary, "t", app.exception, app.exceptions);
  ar_get1(ary, "s", app.description);
  ar_get1(ary, "s", app.note);

  len = pack_Appointment(&app, buffer, datebook_v1);
  dprintf(("pack_Appointment: length: %d\n", len));
  free(app.exception);

  ret = str_new(buffer->data, buffer->used);
  pi_buffer_free(buffer);

  return ret;
}

static VALUE runpack_Appointment(VALUE x, VALUE raw_str)
{
  struct Appointment app;
  VALUE ary = ary_new();
  pi_buffer_t *buffer;

  buffer = pi_buffer_new(0xffff);
  if (!buffer) return Qnil;

  Check_Type(raw_str, T_STRING);

  if (!pi_buffer_append(buffer, RSTRING_PTR(raw_str), RSTRING_LEN(raw_str))) {
    pi_buffer_free(buffer);
    return Qnil;
  }
  unpack_Appointment(&app, buffer, datebook_v1);

  ar_set1(ary, "b", app.event);
  dprintf(("runpack_Appointment: 0\n"));
  ar_set1(ary, "t", app.begin);
  dprintf(("runpack_Appointment: 1\n"));
  ar_set1(ary, "t", app.end);
  dprintf(("runpack_Appointment: 2\n"));
  ar_set1(ary, "b", app.alarm);
  ar_set1(ary, "i", app.advance);
  ar_set1(ary, "i", app.advanceUnits);
  ar_set1(ary, "i", app.repeatType);
  ar_set1(ary, "b", app.repeatForever);
  ar_set1(ary, "t", app.repeatEnd);
  dprintf(("runpack_Appointment: 3\n"));
  ar_set1(ary, "i", app.repeatFrequency);
  ar_set1(ary, "i", app.repeatDay);
  ar_set2(ary, "b", app.repeatDays, 7);
  ar_set1(ary, "i", app.repeatWeekstart);
  ar_set1(ary, "i", app.exceptions);
  ar_set2(ary, "t", app.exception, app.exceptions);
  dprintf(("runpack_Appointment: 4\n"));
  ar_set1(ary, "s", app.description);
  ar_set1(ary, "s", app.note);
  dprintf(("Subject: %s\n", app.description));

  free_Appointment(&app);
  pi_buffer_free (buffer);
  return ary;
}

/****************************************************************/
/********* For Address Records **********************************/
/****************************************************************/

static VALUE rpack_Address(VALUE x, VALUE ary1)
{
  struct Address add;
  VALUE ret;
  pi_buffer_t *buffer;

  buffer = pi_buffer_new(0xffff);
  if (!buffer) return Qnil;

  VALUE ary = ary_new();
  ary_copy(ary, ary1);

  ar_get2(ary, "i", add.phoneLabel, 5);
  ar_get1(ary, "i", add.showPhone);
  ar_get2(ary, "s", add.entry, 19);

  if (pack_Address(&add, buffer, address_v1) < 0)
    ret = Qnil;
  else
    ret = str_new(buffer->data, buffer->used);

  pi_buffer_free (buffer);
  return ret;
}

static VALUE runpack_Address(VALUE x, VALUE raw_str)
{
  struct Address add;
  VALUE ary = ary_new();
  pi_buffer_t *buffer;

  buffer = pi_buffer_new(0xffff);
  if (!buffer) return Qnil;

  Check_Type(raw_str, T_STRING);
  if (!pi_buffer_append(buffer, RSTRING_PTR(raw_str), RSTRING_LEN(raw_str))) {
    pi_buffer_free(buffer);
    return Qnil;
  }
  unpack_Address(&add, buffer, address_v1);

  ar_set2(ary, "i", add.phoneLabel, 5);
  ar_set1(ary, "i", add.showPhone);
  ar_set2(ary, "s", add.entry, 19);

  free_Address(&add);
  pi_buffer_free(buffer);
  return ary;
}

static VALUE rpack_AddressAppInfo(VALUE o, VALUE ary1)
{
  struct AddressAppInfo ai;
  unsigned char buf[0xffff];
  int len;
  VALUE ary = ary_new();
  ary_copy(ary, ary1);

  ar_get2(ary, "b", ai.category.renamed, 16);
  ar_get2(ary, "c", ai.category.ID, 16);
  ar_get1(ary, "c", ai.category.lastUniqueID);

  ar_get2(ary, "s16", ai.labels, 22);
  ar_get2(ary, "s16", ai.phoneLabels, 8);
  ar_get1(ary, "i", ai.country);
  ar_get1(ary, "b", ai.sortByCompany);

  len = pack_AddressAppInfo(&ai, buf, sizeof(buf));
  return str_new(buf, len);
}


static VALUE runpack_AddressAppInfo(VALUE o, VALUE raw_str)
{
  struct AddressAppInfo ai;
  VALUE ary = ary_new();

  Check_Type(raw_str, T_STRING);
  unpack_AddressAppInfo(&ai, RSTRING_PTR(raw_str), RSTRING_LEN(raw_str));

  ar_set2(ary, "b", ai.category.renamed, 16);
  ar_set2(ary, "s16", ai.category.name, 16);
  ar_set2(ary, "c", ai.category.ID, 16);
  ar_set1(ary, "c", ai.category.lastUniqueID);

  ar_set2(ary, "s16", ai.labels, 22);
  ar_set2(ary, "b",   ai.labelRenamed, 22);
  ar_set2(ary, "s16", ai.phoneLabels, 8);
  ar_set1(ary, "i", ai.country);
  ar_set1(ary, "b", ai.sortByCompany);

  return ary;
}

/****************************************************************/
/********************* dump DB file *****************************/
/****************************************************************/

static VALUE cPilotFile;

/* singleton method */
static VALUE rpi_file_open(int argc, VALUE *argv, VALUE klass)
{
  struct pi_file *pf;
  VALUE name, obj, rec_klass;

  rb_scan_args(argc, argv, "11", &name, &rec_klass);
  Check_Type(name, T_STRING);

  if ((pf = pi_file_open(RSTRING_PTR(name))) == NULL){
    Fail("pi_file_open");
  }
  obj = Data_Wrap_Struct(cPilotFile, 0, (void *)pi_file_close, pf);

  rb_obj_call_init(obj, argc, argv);
  return obj;
}

static VALUE rpi_file_get_app_info(VALUE obj)
{
  struct pi_file *pf;
  size_t len;
  char *ptr;

  Data_Get_Struct(obj, struct pi_file, pf);

  pi_file_get_app_info(pf, (void *) &ptr, &len);

  return str_new(ptr, len);
}

static VALUE rpi_file_read_record(VALUE obj, VALUE i)
{
  struct pi_file *pf;
  size_t len;
  int attr, category;
  recordid_t id;
  void *ptr;
  VALUE ary = ary_new();

  Data_Get_Struct(obj, struct pi_file, pf);

  if (pi_file_read_record(pf, FIX2INT(i),
                          &ptr, &len, &attr, &category, &id) < 0){
    return Qnil;
  }

  dprintf(("id:%d atr:%d cat:%d bp:%d len:%d\n",id,attr,category,ptr,len));
  ar_set1(ary, "i", id);
  ar_set1(ary, "i", attr);
  ar_set1(ary, "i", category);
  ary_push(ary, str_new((char*)ptr, len));

  return ary;
}

static VALUE rpi_file_close(VALUE obj)
{
  struct pi_file *pf;

  Data_Get_Struct(obj, struct pi_file, pf);
  pi_file_close(pf);

  return Qnil;
}

/****************************************************************/
/************************* Init *********************************/
/****************************************************************/
void Init_mhc_pilib()
{
  mPiLib = rb_define_module("PiLib");

#define mfunc rb_define_module_function
  /* connection management */
  mfunc(mPiLib, "openSock",             rpi_sock_open,         1);
  mfunc(mPiLib, "listenSock",           rpi_sock_listen,       1);
  mfunc(mPiLib, "closeSock",            rpi_sock_close,        1);
  mfunc(mPiLib, "dlp_AddSyncLogEntry",  rdlp_AddSyncLogEntry,  2);
  mfunc(mPiLib, "dlp_ResetLastSyncPC",  rdlp_ResetLastSyncPC,  1);

  /* system information */
  mfunc(mPiLib, "dlp_GetSysDateTime",    rdlp_GetSysDateTime,  1);
  mfunc(mPiLib, "dlp_SetSysDateTime",    rdlp_SetSysDateTime,  2);
  mfunc(mPiLib, "dlp_ReadUserInfo",      rdlp_ReadUserInfo,    2);
  mfunc(mPiLib, "dlp_WriteUserInfo",     rdlp_WriteUserInfo,   2);

  /* open, close DB */
  mfunc(mPiLib, "dlp_OpenDB",           rdlp_OpenDB,  2);
  mfunc(mPiLib, "dlp_CloseDB",          rdlp_CloseDB, 2);

  /* App info */
  mfunc(mPiLib, "dlp_ReadAppBlock",      rdlp_ReadAppBlock,      2);

  /* record manipulation */
  mfunc(mPiLib, "dlp_ReadRecordByIndex", rdlp_ReadRecordByIndex, 3);
  mfunc(mPiLib, "dlp_ReadRecordById",    rdlp_ReadRecordById,    3);

  mfunc(mPiLib, "dlp_WriteRecord",       rdlp_WriteRecord,       3);
  mfunc(mPiLib, "dlp_DeleteRecord",      rdlp_DeleteRecord,      4);
  mfunc(mPiLib, "dlp_CleanUpDatabase",   rdlp_CleanUpDatabase,   2);
  mfunc(mPiLib, "dlp_ResetSyncFlags",    rdlp_ResetSyncFlags,    2);

  /* for datebook */
  mfunc(mPiLib, "pack_Appointment",      rpack_Appointment,      1);
  mfunc(mPiLib, "unpack_Appointment",    runpack_Appointment,    1);

  /* for address */
  mfunc(mPiLib, "pack_Address",          rpack_Address,          1);
  mfunc(mPiLib, "unpack_Address",        runpack_Address,        1);
  mfunc(mPiLib, "pack_AddressAppInfo",   rpack_AddressAppInfo,   1);
  mfunc(mPiLib, "unpack_AddressAppInfo", runpack_AddressAppInfo, 1);

  /* dump db file */
  cPilotFile = rb_define_class("PilotFile", cObject);
  rb_define_singleton_method(cPilotFile, "new",   rpi_file_open, -1);
  rb_define_singleton_method(cPilotFile, "open",  rpi_file_open, -1);

  rb_define_method(cPilotFile, "get_app_info", rpi_file_get_app_info, 0);
  rb_define_method(cPilotFile, "read_record",  rpi_file_read_record,  1);
  rb_define_method(cPilotFile, "close",        rpi_file_close,        1);
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
*** mhc_pilib.c ends here
*/
