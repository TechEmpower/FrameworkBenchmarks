/*************************************************************************************************
 * The fixed-length database API of Tokyo Cabinet
 *                                                               Copyright (C) 2006-2010 FAL Labs
 * This file is part of Tokyo Cabinet.
 * Tokyo Cabinet is free software; you can redistribute it and/or modify it under the terms of
 * the GNU Lesser General Public License as published by the Free Software Foundation; either
 * version 2.1 of the License or any later version.  Tokyo Cabinet is distributed in the hope
 * that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 * You should have received a copy of the GNU Lesser General Public License along with Tokyo
 * Cabinet; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA.
 *************************************************************************************************/


#include "tcutil.h"
#include "tcfdb.h"
#include "myconf.h"

#define FDBFILEMODE    00644             // permission of created files
#define FDBIOBUFSIZ    8192              // size of an I/O buffer

#define FDBMAGICDATA   "ToKyO CaBiNeT"   // magic data for identification
#define FDBHEADSIZ     256               // size of the reagion of the header
#define FDBTYPEOFF     32                // offset of the region for the database type
#define FDBFLAGSOFF    33                // offset of the region for the additional flags
#define FDBRNUMOFF     48                // offset of the region for the record number
#define FDBFSIZOFF     56                // offset of the region for the file size
#define FDBWIDTHOFF    64                // offset of the region for the record width
#define FDBLIMSIZOFF   72                // offset of the region for the limit size
#define FDBMINOFF      80                // offset of the region for the minimum ID offset
#define FDBMAXOFF      88                // offset of the region for the maximum ID offset
#define FDBOPAQUEOFF   128               // offset of the region for the opaque field

#define FDBDEFWIDTH    255               // default value width
#define FDBDEFLIMSIZ   (256LL<<20)       // default limit size
#define FDBRMTXNUM     127               // number of record mutexes
#define FDBTRUNCALW    256               // number of record for truncate allowance
#define FDBIDARYUNIT   2048              // size of ID array allocation unit
#define FDBWALSUFFIX   "wal"             // suffix of write ahead logging file

enum {                                   // enumeration for duplication behavior
  FDBPDOVER,                             // overwrite an existing value
  FDBPDKEEP,                             // keep the existing value
  FDBPDCAT,                              // concatenate values
  FDBPDADDINT,                           // add an integer
  FDBPDADDDBL,                           // add a real number
  FDBPDPROC                              // process by a callback function
};

typedef struct {                         // type of structure for a duplication callback
  TCPDPROC proc;                         // function pointer
  void *op;                              // opaque pointer
} FDBPDPROCOP;


/* private macros */
#define FDBLOCKMETHOD(TC_fdb, TC_wr)                            \
  ((TC_fdb)->mmtx ? tcfdblockmethod((TC_fdb), (TC_wr)) : true)
#define FDBUNLOCKMETHOD(TC_fdb)                         \
  ((TC_fdb)->mmtx ? tcfdbunlockmethod(TC_fdb) : true)
#define FDBLOCKATTR(TC_fdb)                             \
  ((TC_fdb)->mmtx ? tcfdblockattr(TC_fdb) : true)
#define FDBUNLOCKATTR(TC_fdb)                           \
  ((TC_fdb)->mmtx ? tcfdbunlockattr(TC_fdb) : true)
#define FDBLOCKRECORD(TC_fdb, TC_wr, TC_id)                             \
  ((TC_fdb)->mmtx ? tcfdblockrecord((TC_fdb), (TC_wr), (TC_id)) : true)
#define FDBUNLOCKRECORD(TC_fdb, TC_id)                                  \
  ((TC_fdb)->mmtx ? tcfdbunlockrecord((TC_fdb), (TC_id)) : true)
#define FDBLOCKALLRECORDS(TC_fdb, TC_wr)                                \
  ((TC_fdb)->mmtx ? tcfdblockallrecords((TC_fdb), (TC_wr)) : true)
#define FDBUNLOCKALLRECORDS(TC_fdb)                             \
  ((TC_fdb)->mmtx ? tcfdbunlockallrecords(TC_fdb) : true)
#define FDBLOCKWAL(TC_fdb)                              \
  ((TC_fdb)->mmtx ? tcfdblockwal(TC_fdb) : true)
#define FDBUNLOCKWAL(TC_fdb)                            \
  ((TC_fdb)->mmtx ? tcfdbunlockwal(TC_fdb) : true)
#define FDBTHREADYIELD(TC_fdb)                          \
  do { if((TC_fdb)->mmtx) sched_yield(); } while(false)


/* private function prototypes */
static void tcfdbdumpmeta(TCFDB *fdb, char *hbuf);
static void tcfdbloadmeta(TCFDB *fdb, const char *hbuf);
static void tcfdbclear(TCFDB *fdb);
static void tcfdbsetflag(TCFDB *fdb, int flag, bool sign);
static bool tcfdbwalinit(TCFDB *fdb);
static bool tcfdbwalwrite(TCFDB *fdb, uint64_t off, int64_t size);
static int tcfdbwalrestore(TCFDB *fdb, const char *path);
static bool tcfdbwalremove(TCFDB *fdb, const char *path);
static bool tcfdbopenimpl(TCFDB *fdb, const char *path, int omode);
static bool tcfdbcloseimpl(TCFDB *fdb);
static int64_t tcfdbprevid(TCFDB *fdb, int64_t id);
static int64_t tcfdbnextid(TCFDB *fdb, int64_t id);
static bool tcfdbputimpl(TCFDB *fdb, int64_t id, const void *vbuf, int vsiz, int dmode);
static bool tcfdboutimpl(TCFDB *fdb, int64_t id);
static const void *tcfdbgetimpl(TCFDB *fdb, int64_t id, int *sp);
static bool tcfdbiterinitimpl(TCFDB *fdb);
static uint64_t tcfdbiternextimpl(TCFDB *fdb);
static uint64_t *tcfdbrangeimpl(TCFDB *fdb, int64_t lower, int64_t upper, int max, int *np);
static bool tcfdboptimizeimpl(TCFDB *fdb, int32_t width, int64_t limsiz);
static bool tcfdbvanishimpl(TCFDB *fdb);
static bool tcfdbcopyimpl(TCFDB *fdb, const char *path);
static bool tcfdbiterjumpimpl(TCFDB *fdb, int64_t id);
static bool tcfdbforeachimpl(TCFDB *fdb, TCITER iter, void *op);
static bool tcfdblockmethod(TCFDB *fdb, bool wr);
static bool tcfdbunlockmethod(TCFDB *fdb);
static bool tcfdblockattr(TCFDB *fdb);
static bool tcfdbunlockattr(TCFDB *fdb);
static bool tcfdblockrecord(TCFDB *fdb, bool wr, uint64_t id);
static bool tcfdbunlockrecord(TCFDB *fdb, uint64_t id);
static bool tcfdblockallrecords(TCFDB *fdb, bool wr);
static bool tcfdbunlockallrecords(TCFDB *fdb);
static bool tcfdblockwal(TCFDB *fdb);
static bool tcfdbunlockwal(TCFDB *fdb);


/* debugging function prototypes */
void tcfdbprintmeta(TCFDB *fdb);



/*************************************************************************************************
 * API
 *************************************************************************************************/


/* Get the message string corresponding to an error code. */
const char *tcfdberrmsg(int ecode){
  return tcerrmsg(ecode);
}


/* Create a fixed-length database object. */
TCFDB *tcfdbnew(void){
  TCFDB *fdb;
  TCMALLOC(fdb, sizeof(*fdb));
  tcfdbclear(fdb);
  return fdb;
}


/* Delete a fixed-length database object. */
void tcfdbdel(TCFDB *fdb){
  assert(fdb);
  if(fdb->fd >= 0) tcfdbclose(fdb);
  if(fdb->mmtx){
    pthread_key_delete(*(pthread_key_t *)fdb->eckey);
    pthread_mutex_destroy(fdb->wmtx);
    pthread_mutex_destroy(fdb->tmtx);
    for(int i = FDBRMTXNUM - 1; i >= 0; i--){
      pthread_rwlock_destroy((pthread_rwlock_t *)fdb->rmtxs + i);
    }
    pthread_mutex_destroy(fdb->amtx);
    pthread_rwlock_destroy(fdb->mmtx);
    TCFREE(fdb->eckey);
    TCFREE(fdb->wmtx);
    TCFREE(fdb->tmtx);
    TCFREE(fdb->rmtxs);
    TCFREE(fdb->amtx);
    TCFREE(fdb->mmtx);
  }
  TCFREE(fdb);
}


/* Get the last happened error code of a fixed-length database object. */
int tcfdbecode(TCFDB *fdb){
  assert(fdb);
  return fdb->mmtx ?
    (int)(intptr_t)pthread_getspecific(*(pthread_key_t *)fdb->eckey) : fdb->ecode;
}


/* Set mutual exclusion control of a fixed-length database object for threading. */
bool tcfdbsetmutex(TCFDB *fdb){
  assert(fdb);
  if(!TCUSEPTHREAD) return true;
  if(fdb->mmtx || fdb->fd >= 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    return false;
  }
  TCMALLOC(fdb->mmtx, sizeof(pthread_rwlock_t));
  TCMALLOC(fdb->amtx, sizeof(pthread_mutex_t));
  TCMALLOC(fdb->rmtxs, sizeof(pthread_rwlock_t) * FDBRMTXNUM);
  TCMALLOC(fdb->tmtx, sizeof(pthread_mutex_t));
  TCMALLOC(fdb->wmtx, sizeof(pthread_mutex_t));
  TCMALLOC(fdb->eckey, sizeof(pthread_key_t));
  bool err = false;
  if(pthread_rwlock_init(fdb->mmtx, NULL) != 0) err = true;
  if(pthread_mutex_init(fdb->amtx, NULL) != 0) err = true;
  for(int i = 0; i < FDBRMTXNUM; i++){
    if(pthread_rwlock_init((pthread_rwlock_t *)fdb->rmtxs + i, NULL) != 0) err = true;
  }
  if(pthread_mutex_init(fdb->tmtx, NULL) != 0) err = true;
  if(pthread_mutex_init(fdb->wmtx, NULL) != 0) err = true;
  if(pthread_key_create(fdb->eckey, NULL) != 0) err = true;
  if(err){
    TCFREE(fdb->eckey);
    TCFREE(fdb->wmtx);
    TCFREE(fdb->tmtx);
    TCFREE(fdb->rmtxs);
    TCFREE(fdb->amtx);
    TCFREE(fdb->mmtx);
    fdb->eckey = NULL;
    fdb->wmtx = NULL;
    fdb->tmtx = NULL;
    fdb->rmtxs = NULL;
    fdb->amtx = NULL;
    fdb->mmtx = NULL;
    return false;
  }
  return true;
}


/* Set the tuning parameters of a fixed-length database object. */
bool tcfdbtune(TCFDB *fdb, int32_t width, int64_t limsiz){
  assert(fdb);
  if(fdb->fd >= 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    return false;
  }
  fdb->width = (width > 0) ? width : FDBDEFWIDTH;
  fdb->limsiz = (limsiz > 0) ? limsiz : FDBDEFLIMSIZ;
  if(fdb->limsiz < FDBHEADSIZ + fdb->width + sizeof(uint32_t))
    fdb->limsiz = FDBHEADSIZ + fdb->width + sizeof(uint32_t);
  fdb->limsiz = tcpagealign(fdb->limsiz);
  return true;
}


/* Open a database file and connect a fixed-length database object. */
bool tcfdbopen(TCFDB *fdb, const char *path, int omode){
  assert(fdb && path);
  if(!FDBLOCKMETHOD(fdb, true)) return false;
  if(fdb->fd >= 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  char *rpath = tcrealpath(path);
  if(!rpath){
    int ecode = TCEOPEN;
    switch(errno){
      case EACCES: ecode = TCENOPERM; break;
      case ENOENT: ecode = TCENOFILE; break;
      case ENOTDIR: ecode = TCENOFILE; break;
    }
    tcfdbsetecode(fdb, ecode, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(!tcpathlock(rpath)){
    tcfdbsetecode(fdb, TCETHREAD, __FILE__, __LINE__, __func__);
    TCFREE(rpath);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  bool rv = tcfdbopenimpl(fdb, path, omode);
  if(rv){
    fdb->rpath = rpath;
  } else {
    tcpathunlock(rpath);
    TCFREE(rpath);
  }
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Close a fixed-length database object. */
bool tcfdbclose(TCFDB *fdb){
  assert(fdb);
  if(!FDBLOCKMETHOD(fdb, true)) return false;
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  bool rv = tcfdbcloseimpl(fdb);
  tcpathunlock(fdb->rpath);
  TCFREE(fdb->rpath);
  fdb->rpath = NULL;
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Store a record into a fixed-length database object. */
bool tcfdbput(TCFDB *fdb, int64_t id, const void *vbuf, int vsiz){
  assert(fdb && vbuf && vsiz >= 0);
  if(!FDBLOCKMETHOD(fdb, id < 1)) return false;
  if(fdb->fd < 0 || !(fdb->omode & FDBOWRITER)){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(id == FDBIDMIN){
    id = fdb->min;
  } else if(id == FDBIDPREV){
    id = fdb->min - 1;
  } else if(id == FDBIDMAX){
    id = fdb->max;
  } else if(id == FDBIDNEXT){
    id = fdb->max + 1;
  }
  if(id < 1 || id > fdb->limid){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(!FDBLOCKRECORD(fdb, true, id)){
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  bool rv = tcfdbputimpl(fdb, id, vbuf, vsiz, FDBPDOVER);
  FDBUNLOCKRECORD(fdb, id);
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Store a record with a decimal key into a fixed-length database object. */
bool tcfdbput2(TCFDB *fdb, const void *kbuf, int ksiz, const void *vbuf, int vsiz){
  assert(fdb && kbuf && ksiz >= 0 && vbuf && vsiz >= 0);
  return tcfdbput(fdb, tcfdbkeytoid(kbuf, ksiz), vbuf, vsiz);
}


/* Store a string record with a decimal key into a fixed-length database object. */
bool tcfdbput3(TCFDB *fdb, const char *kstr, const void *vstr){
  assert(fdb && kstr && vstr);
  return tcfdbput(fdb, tcfdbkeytoid(kstr, strlen(kstr)), vstr, strlen(vstr));
}


/* Store a new record into a fixed-length database object. */
bool tcfdbputkeep(TCFDB *fdb, int64_t id, const void *vbuf, int vsiz){
  assert(fdb && vbuf && vsiz >= 0);
  if(!FDBLOCKMETHOD(fdb, id < 1)) return false;
  if(fdb->fd < 0 || !(fdb->omode & FDBOWRITER)){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(id == FDBIDMIN){
    id = fdb->min;
  } else if(id == FDBIDPREV){
    id = fdb->min - 1;
  } else if(id == FDBIDMAX){
    id = fdb->max;
  } else if(id == FDBIDNEXT){
    id = fdb->max + 1;
  }
  if(id < 1 || id > fdb->limid){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(!FDBLOCKRECORD(fdb, true, id)){
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  bool rv = tcfdbputimpl(fdb, id, vbuf, vsiz, FDBPDKEEP);
  FDBUNLOCKRECORD(fdb, id);
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Store a new record with a decimal key into a fixed-length database object. */
bool tcfdbputkeep2(TCFDB *fdb, const void *kbuf, int ksiz, const void *vbuf, int vsiz){
  assert(fdb && kbuf && ksiz >= 0 && vbuf && vsiz >= 0);
  return tcfdbputkeep(fdb, tcfdbkeytoid(kbuf, ksiz), vbuf, vsiz);
}


/* Store a new string record with a decimal key into a fixed-length database object. */
bool tcfdbputkeep3(TCFDB *fdb, const char *kstr, const void *vstr){
  assert(fdb && kstr && vstr);
  return tcfdbputkeep(fdb, tcfdbkeytoid(kstr, strlen(kstr)), vstr, strlen(vstr));
}


/* Concatenate a value at the end of the existing record in a fixed-length database object. */
bool tcfdbputcat(TCFDB *fdb, int64_t id, const void *vbuf, int vsiz){
  assert(fdb && vbuf && vsiz >= 0);
  if(!FDBLOCKMETHOD(fdb, id < 1)) return false;
  if(fdb->fd < 0 || !(fdb->omode & FDBOWRITER)){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(id == FDBIDMIN){
    id = fdb->min;
  } else if(id == FDBIDPREV){
    id = fdb->min - 1;
  } else if(id == FDBIDMAX){
    id = fdb->max;
  } else if(id == FDBIDNEXT){
    id = fdb->max + 1;
  }
  if(id < 1 || id > fdb->limid){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(!FDBLOCKRECORD(fdb, true, id)){
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  bool rv = tcfdbputimpl(fdb, id, vbuf, vsiz, FDBPDCAT);
  FDBUNLOCKRECORD(fdb, id);
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Concatenate a value with a decimal key in a fixed-length database object. */
bool tcfdbputcat2(TCFDB *fdb, const void *kbuf, int ksiz, const void *vbuf, int vsiz){
  assert(fdb && kbuf && ksiz >= 0 && vbuf && vsiz >= 0);
  return tcfdbputcat(fdb, tcfdbkeytoid(kbuf, ksiz), vbuf, vsiz);
}


/* Concatenate a string value with a decimal key in a fixed-length database object. */
bool tcfdbputcat3(TCFDB *fdb, const char *kstr, const void *vstr){
  assert(fdb && kstr && vstr);
  return tcfdbputcat(fdb, tcfdbkeytoid(kstr, strlen(kstr)), vstr, strlen(vstr));
}


/* Remove a record of a fixed-length database object. */
bool tcfdbout(TCFDB *fdb, int64_t id){
  assert(fdb);
  if(!FDBLOCKMETHOD(fdb, true)) return false;
  if(fdb->fd < 0 || !(fdb->omode & FDBOWRITER)){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(id == FDBIDMIN){
    id = fdb->min;
  } else if(id == FDBIDMAX){
    id = fdb->max;
  }
  if(id < 1 || id > fdb->limid){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(!FDBLOCKRECORD(fdb, true, id)){
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  bool rv = tcfdboutimpl(fdb, id);
  FDBUNLOCKRECORD(fdb, id);
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Remove a record with a decimal key of a fixed-length database object. */
bool tcfdbout2(TCFDB *fdb, const void *kbuf, int ksiz){
  assert(fdb && kbuf && ksiz >= 0);
  return tcfdbout(fdb, tcfdbkeytoid(kbuf, ksiz));
}


/* Remove a string record with a decimal key of a fixed-length database object. */
bool tcfdbout3(TCFDB *fdb, const char *kstr){
  assert(fdb && kstr);
  return tcfdbout(fdb, tcfdbkeytoid(kstr, strlen(kstr)));
}


/* Retrieve a record in a fixed-length database object. */
void *tcfdbget(TCFDB *fdb, int64_t id, int *sp){
  assert(fdb && sp);
  if(!FDBLOCKMETHOD(fdb, false)) return false;
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(id == FDBIDMIN){
    id = fdb->min;
  } else if(id == FDBIDMAX){
    id = fdb->max;
  }
  if(id < 1 || id > fdb->limid){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(!FDBLOCKRECORD(fdb, false, id)){
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  const void *vbuf = tcfdbgetimpl(fdb, id, sp);
  char *rv = vbuf ? tcmemdup(vbuf, *sp) : NULL;
  FDBUNLOCKRECORD(fdb, id);
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Retrieve a record with a decimal key in a fixed-length database object. */
void *tcfdbget2(TCFDB *fdb, const void *kbuf, int ksiz, int *sp){
  assert(fdb && kbuf && ksiz >= 0 && sp);
  return tcfdbget(fdb, tcfdbkeytoid(kbuf, ksiz), sp);
}


/* Retrieve a string record with a decimal key in a fixed-length database object. */
char *tcfdbget3(TCFDB *fdb, const char *kstr){
  assert(fdb && kstr);
  int vsiz;
  return tcfdbget(fdb, tcfdbkeytoid(kstr, strlen(kstr)), &vsiz);
}


/* Retrieve a record in a fixed-length database object and write the value into a buffer. */
int tcfdbget4(TCFDB *fdb, int64_t id, void *vbuf, int max){
  assert(fdb && vbuf && max >= 0);
  if(!FDBLOCKMETHOD(fdb, false)) return false;
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(id == FDBIDMIN){
    id = fdb->min;
  } else if(id == FDBIDMAX){
    id = fdb->max;
  }
  if(id < 1 || id > fdb->limid){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(!FDBLOCKRECORD(fdb, false, id)){
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  int vsiz;
  const void *rbuf = tcfdbgetimpl(fdb, id, &vsiz);
  if(rbuf){
    if(vsiz > max) vsiz = max;
    memcpy(vbuf, rbuf, vsiz);
  } else {
    vsiz = -1;
  }
  FDBUNLOCKRECORD(fdb, id);
  FDBUNLOCKMETHOD(fdb);
  return vsiz;
}


/* Get the size of the value of a record in a fixed-length database object. */
int tcfdbvsiz(TCFDB *fdb, int64_t id){
  assert(fdb);
  if(!FDBLOCKMETHOD(fdb, false)) return false;
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(id == FDBIDMIN){
    id = fdb->min;
  } else if(id == FDBIDMAX){
    id = fdb->max;
  }
  if(id < 1 || id > fdb->limid){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(!FDBLOCKRECORD(fdb, false, id)){
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  int vsiz;
  const void *vbuf = tcfdbgetimpl(fdb, id, &vsiz);
  if(!vbuf) vsiz = -1;
  FDBUNLOCKRECORD(fdb, id);
  FDBUNLOCKMETHOD(fdb);
  return vsiz;
}


/* Get the size of the value with a decimal key in a fixed-length database object. */
int tcfdbvsiz2(TCFDB *fdb, const void *kbuf, int ksiz){
  assert(fdb && kbuf && ksiz >= 0);
  return tcfdbvsiz(fdb, tcfdbkeytoid(kbuf, ksiz));
}


/* Get the size of the string value with a decimal key in a fixed-length database object. */
int tcfdbvsiz3(TCFDB *fdb, const char *kstr){
  assert(fdb && kstr);
  return tcfdbvsiz(fdb, tcfdbkeytoid(kstr, strlen(kstr)));
}


/* Initialize the iterator of a fixed-length database object. */
bool tcfdbiterinit(TCFDB *fdb){
  assert(fdb);
  if(!FDBLOCKMETHOD(fdb, true)) return false;
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  bool rv = tcfdbiterinitimpl(fdb);
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Get the next ID number of the iterator of a fixed-length database object. */
uint64_t tcfdbiternext(TCFDB *fdb){
  assert(fdb);
  if(!FDBLOCKMETHOD(fdb, true)) return false;
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  uint64_t rv = tcfdbiternextimpl(fdb);
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Get the next decimay key of the iterator of a fixed-length database object. */
void *tcfdbiternext2(TCFDB *fdb, int *sp){
  assert(fdb && sp);
  uint64_t id = tcfdbiternextimpl(fdb);
  if(id < 1) return NULL;
  char kbuf[TCNUMBUFSIZ];
  int ksiz = sprintf(kbuf, "%llu", (unsigned long long)id);
  *sp = ksiz;
  return tcmemdup(kbuf, ksiz);
}


/* Get the next decimay key string of the iterator of a fixed-length database object. */
char *tcfdbiternext3(TCFDB *fdb){
  assert(fdb);
  int ksiz;
  return tcfdbiternext2(fdb, &ksiz);
}


/* Get range matching decimal keys in a fixed-length database object. */
uint64_t *tcfdbrange(TCFDB *fdb, int64_t lower, int64_t upper, int max, int *np){
  assert(fdb && np);
  if(!FDBLOCKMETHOD(fdb, true)) return false;
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    *np = 0;
    return tcmalloc(1);
  }
  if(lower == FDBIDMIN) lower = fdb->min;
  if(upper == FDBIDMAX) upper = fdb->max;
  if(lower < 1 || lower > fdb->limid || upper < 1 || upper > fdb->limid){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    *np = 0;
    return tcmalloc(1);
  }
  uint64_t *rv = tcfdbrangeimpl(fdb, lower, upper, max, np);
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Get range matching decimal keys in a fixed-length database object. */
TCLIST *tcfdbrange2(TCFDB *fdb, const void *lbuf, int lsiz, const void *ubuf, int usiz, int max){
  assert(fdb && lbuf && lsiz >= 0 && ubuf && usiz >= 0);
  int num;
  uint64_t *ids = tcfdbrange(fdb, tcfdbkeytoid(lbuf, lsiz), tcfdbkeytoid(ubuf, usiz), max, &num);
  TCLIST *keys = tclistnew2(num);
  for(int i = 0; i < num; i++){
    char kbuf[TCNUMBUFSIZ];
    int ksiz = sprintf(kbuf, "%llu", (unsigned long long)ids[i]);
    TCLISTPUSH(keys, kbuf, ksiz);
  }
  TCFREE(ids);
  return keys;
}


/* Get range matching decimal keys with strings in a fixed-length database object. */
TCLIST *tcfdbrange3(TCFDB *fdb, const char *lstr, const char *ustr, int max){
  assert(fdb && lstr && ustr);
  return tcfdbrange2(fdb, lstr, strlen(lstr), ustr, strlen(ustr), max);
}


/* Get keys with an interval notation in a fixed-length database object. */
TCLIST *tcfdbrange4(TCFDB *fdb, const void *ibuf, int isiz, int max){
  assert(fdb && ibuf && isiz >= 0);
  char *expr;
  TCMEMDUP(expr, ibuf, isiz);
  char *pv = expr;
  while(*pv > '\0' && *pv <= ' '){
    pv++;
  }
  bool linc = false;
  if(*pv == '['){
    linc = true;
  } else if(*pv != '('){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    TCFREE(expr);
    return tclistnew();
  }
  pv++;
  char *sep = strchr(pv, ',');
  if(!sep){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    TCFREE(expr);
    return tclistnew();
  }
  *sep = '\0';
  tcstrtrim(pv);
  int64_t lower = tcfdbkeytoid(pv, strlen(pv));
  pv = sep + 1;
  bool uinc = false;
  if((sep = strchr(pv, ']')) != NULL){
    uinc = true;
    *sep = '\0';
  } else if((sep = strchr(pv, ')')) != NULL){
    *sep = '\0';
  } else {
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    TCFREE(expr);
    return tclistnew();
  }
  tcstrtrim(pv);
  int64_t upper = tcfdbkeytoid(pv, strlen(pv));
  if(lower == FDBIDMIN){
    lower = fdb->min;
  } else if(lower == FDBIDPREV){
    lower = fdb->min - 1;
  } else if(lower == FDBIDMAX){
    lower = fdb->max;
  } else if(lower == FDBIDNEXT){
    lower = fdb->max + 1;
  }
  if(!linc) lower++;
  if(upper == FDBIDMIN){
    upper = fdb->min;
  } else if(upper == FDBIDPREV){
    upper = fdb->min - 1;
  } else if(upper == FDBIDMAX){
    upper = fdb->max;
  } else if(upper == FDBIDNEXT){
    upper = fdb->max + 1;
  }
  if(!uinc) upper--;
  TCFREE(expr);
  int num;
  uint64_t *ids = tcfdbrange(fdb, lower, upper, max, &num);
  TCLIST *keys = tclistnew2(num);
  for(int i = 0; i < num; i++){
    char kbuf[TCNUMBUFSIZ];
    int ksiz = sprintf(kbuf, "%llu", (unsigned long long)ids[i]);
    TCLISTPUSH(keys, kbuf, ksiz);
  }
  TCFREE(ids);
  return keys;
}


/* Get keys with an interval notation string in a fixed-length database object. */
TCLIST *tcfdbrange5(TCFDB *fdb, const void *istr, int max){
  assert(fdb && istr);
  return tcfdbrange4(fdb, istr, strlen(istr), max);
}


/* Add an integer to a record in a fixed-length database object. */
int tcfdbaddint(TCFDB *fdb, int64_t id, int num){
  assert(fdb);
  if(!FDBLOCKMETHOD(fdb, id < 1)) return INT_MIN;
  if(fdb->fd < 0 || !(fdb->omode & FDBOWRITER)){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return INT_MIN;
  }
  if(id == FDBIDMIN){
    id = fdb->min;
  } else if(id == FDBIDPREV){
    id = fdb->min - 1;
  } else if(id == FDBIDMAX){
    id = fdb->max;
  } else if(id == FDBIDNEXT){
    id = fdb->max + 1;
  }
  if(id < 1 || id > fdb->limid){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return INT_MIN;
  }
  if(!FDBLOCKRECORD(fdb, true, id)){
    FDBUNLOCKMETHOD(fdb);
    return INT_MIN;
  }
  bool rv = tcfdbputimpl(fdb, id, (char *)&num, sizeof(num), FDBPDADDINT);
  FDBUNLOCKRECORD(fdb, id);
  FDBUNLOCKMETHOD(fdb);
  return rv ? num : INT_MIN;
}


/* Add a real number to a record in a fixed-length database object. */
double tcfdbadddouble(TCFDB *fdb, int64_t id, double num){
  assert(fdb);
  if(!FDBLOCKMETHOD(fdb, id < 1)) return nan("");
  if(fdb->fd < 0 || !(fdb->omode & FDBOWRITER)){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return nan("");
  }
  if(id == FDBIDMIN){
    id = fdb->min;
  } else if(id == FDBIDPREV){
    id = fdb->min - 1;
  } else if(id == FDBIDMAX){
    id = fdb->max;
  } else if(id == FDBIDNEXT){
    id = fdb->max + 1;
  }
  if(id < 1 || id > fdb->limid){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return nan("");
  }
  if(!FDBLOCKRECORD(fdb, true, id)){
    FDBUNLOCKMETHOD(fdb);
    return nan("");
  }
  bool rv = tcfdbputimpl(fdb, id, (char *)&num, sizeof(num), FDBPDADDDBL);
  FDBUNLOCKRECORD(fdb, id);
  FDBUNLOCKMETHOD(fdb);
  return rv ? num : nan("");
}


/* Synchronize updated contents of a fixed-length database object with the file and the device. */
bool tcfdbsync(TCFDB *fdb){
  assert(fdb);
  if(!FDBLOCKMETHOD(fdb, true)) return false;
  if(fdb->fd < 0 || !(fdb->omode & FDBOWRITER) || fdb->tran){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  bool rv = tcfdbmemsync(fdb, true);
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Optimize the file of a fixed-length database object. */
bool tcfdboptimize(TCFDB *fdb, int32_t width, int64_t limsiz){
  assert(fdb);
  if(!FDBLOCKMETHOD(fdb, true)) return false;
  if(fdb->fd < 0 || !(fdb->omode & FDBOWRITER) || fdb->tran){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  FDBTHREADYIELD(fdb);
  bool rv = tcfdboptimizeimpl(fdb, width, limsiz);
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Remove all records of a fixed-length database object. */
bool tcfdbvanish(TCFDB *fdb){
  assert(fdb);
  if(!FDBLOCKMETHOD(fdb, true)) return false;
  if(fdb->fd < 0 || !(fdb->omode & FDBOWRITER) || fdb->tran){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  FDBTHREADYIELD(fdb);
  bool rv = tcfdbvanishimpl(fdb);
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Copy the database file of a fixed-length database object. */
bool tcfdbcopy(TCFDB *fdb, const char *path){
  assert(fdb && path);
  if(!FDBLOCKMETHOD(fdb, false)) return false;
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(!FDBLOCKALLRECORDS(fdb, false)){
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  FDBTHREADYIELD(fdb);
  bool rv = tcfdbcopyimpl(fdb, path);
  FDBUNLOCKALLRECORDS(fdb);
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Begin the transaction of a fixed-length database object. */
bool tcfdbtranbegin(TCFDB *fdb){
  assert(fdb);
  for(double wsec = 1.0 / sysconf(_SC_CLK_TCK); true; wsec *= 2){
    if(!FDBLOCKMETHOD(fdb, true)) return false;
    if(fdb->fd < 0 || !(fdb->omode & FDBOWRITER) || fdb->fatal){
      tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
      FDBUNLOCKMETHOD(fdb);
      return false;
    }
    if(!fdb->tran) break;
    FDBUNLOCKMETHOD(fdb);
    if(wsec > 1.0) wsec = 1.0;
    tcsleep(wsec);
  }
  if(!tcfdbmemsync(fdb, false)){
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if((fdb->omode & FDBOTSYNC) && fsync(fdb->fd) == -1){
    tcfdbsetecode(fdb, TCESYNC, __FILE__, __LINE__, __func__);
    return false;
  }
  if(fdb->walfd < 0){
    char *tpath = tcsprintf("%s%c%s", fdb->path, MYEXTCHR, FDBWALSUFFIX);
    int walfd = open(tpath, O_RDWR | O_CREAT | O_TRUNC, FDBFILEMODE);
    TCFREE(tpath);
    if(walfd < 0){
      int ecode = TCEOPEN;
      switch(errno){
        case EACCES: ecode = TCENOPERM; break;
        case ENOENT: ecode = TCENOFILE; break;
        case ENOTDIR: ecode = TCENOFILE; break;
      }
      tcfdbsetecode(fdb, ecode, __FILE__, __LINE__, __func__);
      FDBUNLOCKMETHOD(fdb);
      return false;
    }
    fdb->walfd = walfd;
  }
  tcfdbsetflag(fdb, FDBFOPEN, false);
  if(!tcfdbwalinit(fdb)){
    tcfdbsetflag(fdb, FDBFOPEN, true);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  tcfdbsetflag(fdb, FDBFOPEN, true);
  fdb->tran = true;
  FDBUNLOCKMETHOD(fdb);
  return true;
}


/* Commit the transaction of a fixed-length database object. */
bool tcfdbtrancommit(TCFDB *fdb){
  assert(fdb);
  if(!FDBLOCKMETHOD(fdb, true)) return false;
  if(fdb->fd < 0 || !(fdb->omode & FDBOWRITER) || fdb->fatal || !fdb->tran){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  bool err = false;
  if(!tcfdbmemsync(fdb, fdb->omode & FDBOTSYNC)) err = true;
  if(!err && ftruncate(fdb->walfd, 0) == -1){
    tcfdbsetecode(fdb, TCETRUNC, __FILE__, __LINE__, __func__);
    err = true;
  }
  fdb->tran = false;
  FDBUNLOCKMETHOD(fdb);
  return !err;
}


/* Abort the transaction of a fixed-length database object. */
bool tcfdbtranabort(TCFDB *fdb){
  assert(fdb);
  if(!FDBLOCKMETHOD(fdb, true)) return false;
  if(fdb->fd < 0 || !(fdb->omode & FDBOWRITER) || !fdb->tran){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  bool err = false;
  if(!tcfdbmemsync(fdb, false)) err = true;
  if(!tcfdbwalrestore(fdb, fdb->path)) err = true;
  char hbuf[FDBHEADSIZ];
  if(lseek(fdb->fd, 0, SEEK_SET) == -1){
    tcfdbsetecode(fdb, TCESEEK, __FILE__, __LINE__, __func__);
    err = false;
  } else if(!tcread(fdb->fd, hbuf, FDBHEADSIZ)){
    tcfdbsetecode(fdb, TCEREAD, __FILE__, __LINE__, __func__);
    err = false;
  } else {
    tcfdbloadmeta(fdb, hbuf);
  }
  fdb->tran = false;
  FDBUNLOCKMETHOD(fdb);
  return !err;
}


/* Get the file path of a fixed-length database object. */
const char *tcfdbpath(TCFDB *fdb){
  assert(fdb);
  if(!FDBLOCKMETHOD(fdb, false)) return NULL;
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return NULL;
  }
  const char *rv = fdb->path;
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Get the number of records of a fixed-length database object. */
uint64_t tcfdbrnum(TCFDB *fdb){
  assert(fdb);
  if(!FDBLOCKMETHOD(fdb, false)) return 0;
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return 0;
  }
  uint64_t rv = fdb->rnum;
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Get the size of the database file of a fixed-length database object. */
uint64_t tcfdbfsiz(TCFDB *fdb){
  assert(fdb);
  if(!FDBLOCKMETHOD(fdb, false)) return 0;
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return 0;
  }
  uint64_t rv = fdb->fsiz;
  FDBUNLOCKMETHOD(fdb);
  return rv;
}



/*************************************************************************************************
 * features for experts
 *************************************************************************************************/


/* Set the error code of a fixed-length database object. */
void tcfdbsetecode(TCFDB *fdb, int ecode, const char *filename, int line, const char *func){
  assert(fdb && filename && line >= 1 && func);
  int myerrno = errno;
  if(!fdb->fatal){
    fdb->ecode = ecode;
    if(fdb->mmtx) pthread_setspecific(*(pthread_key_t *)fdb->eckey, (void *)(intptr_t)ecode);
  }
  if(ecode != TCEINVALID && ecode != TCEKEEP && ecode != TCENOREC){
    fdb->fatal = true;
    if(fdb->fd >= 0 && (fdb->omode & FDBOWRITER)) tcfdbsetflag(fdb, FDBFFATAL, true);
  }
  if(fdb->dbgfd >= 0 && (fdb->dbgfd != UINT16_MAX || fdb->fatal)){
    int dbgfd = (fdb->dbgfd == UINT16_MAX) ? 1 : fdb->dbgfd;
    char obuf[FDBIOBUFSIZ];
    int osiz = sprintf(obuf, "ERROR:%s:%d:%s:%s:%d:%s:%d:%s\n", filename, line, func,
                       fdb->path ? fdb->path : "-", ecode, tcfdberrmsg(ecode),
                       myerrno, strerror(myerrno));
    tcwrite(dbgfd, obuf, osiz);
  }
}


/* Set the file descriptor for debugging output. */
void tcfdbsetdbgfd(TCFDB *fdb, int fd){
  assert(fdb && fd >= 0);
  fdb->dbgfd = fd;
}


/* Get the file descriptor for debugging output. */
int tcfdbdbgfd(TCFDB *fdb){
  assert(fdb);
  return fdb->dbgfd;
}


/* Check whether mutual exclusion control is set to a fixed-length database object. */
bool tcfdbhasmutex(TCFDB *fdb){
  assert(fdb);
  return fdb->mmtx != NULL;
}


/* Synchronize updating contents on memory of a fixed-length database object. */
bool tcfdbmemsync(TCFDB *fdb, bool phys){
  assert(fdb);
  if(fdb->fd < 0 || !(fdb->omode & FDBOWRITER)){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    return false;
  }
  bool err = false;
  char hbuf[FDBHEADSIZ];
  tcfdbdumpmeta(fdb, hbuf);
  memcpy(fdb->map, hbuf, FDBOPAQUEOFF);
  if(phys){
    if(msync(fdb->map, fdb->limsiz, MS_SYNC) == -1){
      tcfdbsetecode(fdb, TCEMMAP, __FILE__, __LINE__, __func__);
      err = true;
    }
    if(fsync(fdb->fd) == -1){
      tcfdbsetecode(fdb, TCESYNC, __FILE__, __LINE__, __func__);
      err = true;
    }
  }
  return !err;
}


/* Get the minimum ID number of records of a fixed-length database object. */
uint64_t tcfdbmin(TCFDB *fdb){
  assert(fdb);
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    return 0;
  }
  return fdb->min;
}


/* Get the maximum ID number of records of a fixed-length database object. */
uint64_t tcfdbmax(TCFDB *fdb){
  assert(fdb);
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    return 0;
  }
  return fdb->max;
}


/* Get the width of the value of each record of a fixed-length database object. */
uint32_t tcfdbwidth(TCFDB *fdb){
  assert(fdb);
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    return 0;
  }
  return fdb->width;
}


/* Get the limit file size of a fixed-length database object. */
uint64_t tcfdblimsiz(TCFDB *fdb){
  assert(fdb);
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    return 0;
  }
  return fdb->limsiz;
}


/* Get the limit ID number of a fixed-length database object. */
uint64_t tcfdblimid(TCFDB *fdb){
  assert(fdb);
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    return 0;
  }
  return fdb->limid;
}


/* Get the inode number of the database file of a fixed-length database object. */
uint64_t tcfdbinode(TCFDB *fdb){
  assert(fdb);
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    return 0;
  }
  return fdb->inode;
}


/* Get the modification time of the database file of a fixed-length database object. */
time_t tcfdbmtime(TCFDB *fdb){
  assert(fdb);
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    return 0;
  }
  return fdb->mtime;
}


/* Get the connection mode of a fixed-length database object. */
int tcfdbomode(TCFDB *fdb){
  assert(fdb);
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    return 0;
  }
  return fdb->omode;
}


/* Get the database type of a fixed-length database object. */
uint8_t tcfdbtype(TCFDB *fdb){
  assert(fdb);
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    return 0;
  }
  return fdb->type;
}


/* Get the additional flags of a fixed-length database object. */
uint8_t tcfdbflags(TCFDB *fdb){
  assert(fdb);
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    return 0;
  }
  return fdb->flags;
}


/* Get the pointer to the opaque field of a fixed-length database object. */
char *tcfdbopaque(TCFDB *fdb){
  assert(fdb);
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    return NULL;
  }
  return fdb->map + FDBOPAQUEOFF;
}


/* Store a record into a fixed-length database object with a duplication handler. */
bool tcfdbputproc(TCFDB *fdb, int64_t id, const void *vbuf, int vsiz, TCPDPROC proc, void *op){
  assert(fdb && proc);
  if(!FDBLOCKMETHOD(fdb, id < 1)) return false;
  if(fdb->fd < 0 || !(fdb->omode & FDBOWRITER)){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(id == FDBIDMIN){
    id = fdb->min;
  } else if(id == FDBIDPREV){
    id = fdb->min - 1;
  } else if(id == FDBIDMAX){
    id = fdb->max;
  } else if(id == FDBIDNEXT){
    id = fdb->max + 1;
  }
  if(id < 1 || id > fdb->limid){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(!FDBLOCKRECORD(fdb, true, id)){
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  FDBPDPROCOP procop;
  procop.proc = proc;
  procop.op = op;
  FDBPDPROCOP *procptr = &procop;
  tcgeneric_t stack[(FDBDEFWIDTH+TCNUMBUFSIZ)/sizeof(tcgeneric_t)+1];
  char *rbuf;
  if(vbuf){
    if(vsiz <= sizeof(stack) - sizeof(procptr)){
      rbuf = (char *)stack;
    } else {
      TCMALLOC(rbuf, vsiz + sizeof(procptr));
    }
    char *wp = rbuf;
    memcpy(wp, &procptr, sizeof(procptr));
    wp += sizeof(procptr);
    memcpy(wp, vbuf, vsiz);
    vbuf = rbuf + sizeof(procptr);
  } else {
    rbuf = (char *)stack;
    memcpy(rbuf, &procptr, sizeof(procptr));
    vbuf = rbuf + sizeof(procptr);
    vsiz = -1;
  }
  bool rv = tcfdbputimpl(fdb, id, vbuf, vsiz, FDBPDPROC);
  if(rbuf != (char *)stack) TCFREE(rbuf);
  FDBUNLOCKRECORD(fdb, id);
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Move the iterator to the record corresponding a key of a fixed-length database object. */
bool tcfdbiterinit2(TCFDB *fdb, int64_t id){
  assert(fdb);
  if(!FDBLOCKMETHOD(fdb, true)) return false;
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(id == FDBIDMIN){
    id = fdb->min;
  } else if(id == FDBIDMAX){
    id = fdb->max;
  }
  if(id < 1 || id > fdb->limid){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  bool rv = tcfdbiterjumpimpl(fdb, id);
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Move the iterator to the decimal record of a fixed-length database object. */
bool tcfdbiterinit3(TCFDB *fdb, const void *kbuf, int ksiz){
  assert(fdb && kbuf && ksiz >= 0);
  return tcfdbiterinit2(fdb, tcfdbkeytoid(kbuf, ksiz));
}


/* Move the iterator to the decimal string record of a fixed-length database object. */
bool tcfdbiterinit4(TCFDB *fdb, const char *kstr){
  assert(fdb && kstr);
  return tcfdbiterinit2(fdb, tcfdbkeytoid(kstr, strlen(kstr)));
}


/* Process each record atomically of a fixed-length database object. */
bool tcfdbforeach(TCFDB *fdb, TCITER iter, void *op){
  assert(fdb && iter);
  if(!FDBLOCKMETHOD(fdb, false)) return false;
  if(fdb->fd < 0){
    tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  if(!FDBLOCKALLRECORDS(fdb, false)){
    FDBUNLOCKMETHOD(fdb);
    return false;
  }
  FDBTHREADYIELD(fdb);
  bool rv = tcfdbforeachimpl(fdb, iter, op);
  FDBUNLOCKALLRECORDS(fdb);
  FDBUNLOCKMETHOD(fdb);
  return rv;
}


/* Generate the ID number from arbitrary binary data. */
int64_t tcfdbkeytoid(const char *kbuf, int ksiz){
  assert(kbuf && ksiz >= 0);
  if(ksiz == 3 && !memcmp(kbuf, "min", 3)){
    return FDBIDMIN;
  } else if(ksiz == 4 && !memcmp(kbuf, "prev", 4)){
    return FDBIDPREV;
  } else if(ksiz == 3 && !memcmp(kbuf, "max", 3)){
    return FDBIDMAX;
  } else if(ksiz == 4 && !memcmp(kbuf, "next", 4)){
    return FDBIDNEXT;
  }
  int64_t id = 0;
  const char *end = kbuf + ksiz;
  while(kbuf < end){
    int c = *(unsigned char *)(kbuf++);
    if(c >= '0' && c <= '9') id = id * 10 + c - '0';
  }
  return id;
}



/*************************************************************************************************
 * private features
 *************************************************************************************************/


/* Serialize meta data into a buffer.
   `fdb' specifies the fixed-length database object.
   `hbuf' specifies the buffer. */
static void tcfdbdumpmeta(TCFDB *fdb, char *hbuf){
  memset(hbuf, 0, FDBHEADSIZ);
  sprintf(hbuf, "%s\n%s:%d\n", FDBMAGICDATA, _TC_FORMATVER, _TC_LIBVER);
  memcpy(hbuf + FDBTYPEOFF, &(fdb->type), sizeof(fdb->type));
  memcpy(hbuf + FDBFLAGSOFF, &(fdb->flags), sizeof(fdb->flags));
  uint64_t llnum;
  llnum = fdb->rnum;
  llnum = TCHTOILL(llnum);
  memcpy(hbuf + FDBRNUMOFF, &llnum, sizeof(llnum));
  llnum = fdb->fsiz;
  llnum = TCHTOILL(llnum);
  memcpy(hbuf + FDBFSIZOFF, &llnum, sizeof(llnum));
  llnum = fdb->width;
  llnum = TCHTOILL(llnum);
  memcpy(hbuf + FDBWIDTHOFF, &llnum, sizeof(llnum));
  llnum = fdb->limsiz;
  llnum = TCHTOILL(llnum);
  memcpy(hbuf + FDBLIMSIZOFF, &llnum, sizeof(llnum));
  llnum = fdb->min;
  llnum = TCHTOILL(llnum);
  memcpy(hbuf + FDBMINOFF, &llnum, sizeof(llnum));
  llnum = fdb->max;
  llnum = TCHTOILL(llnum);
  memcpy(hbuf + FDBMAXOFF, &llnum, sizeof(llnum));
}


/* Deserialize meta data from a buffer.
   `fdb' specifies the fixed-length database object.
   `hbuf' specifies the buffer. */
static void tcfdbloadmeta(TCFDB *fdb, const char *hbuf){
  memcpy(&(fdb->type), hbuf + FDBTYPEOFF, sizeof(fdb->type));
  memcpy(&(fdb->flags), hbuf + FDBFLAGSOFF, sizeof(fdb->flags));
  uint64_t llnum;
  memcpy(&llnum, hbuf + FDBRNUMOFF, sizeof(llnum));
  fdb->rnum = TCITOHLL(llnum);
  memcpy(&llnum, hbuf + FDBFSIZOFF, sizeof(llnum));
  fdb->fsiz = TCITOHLL(llnum);
  memcpy(&llnum, hbuf + FDBWIDTHOFF, sizeof(llnum));
  fdb->width = TCITOHLL(llnum);
  memcpy(&llnum, hbuf + FDBLIMSIZOFF, sizeof(llnum));
  fdb->limsiz = TCITOHLL(llnum);
  memcpy(&llnum, hbuf + FDBMINOFF, sizeof(llnum));
  fdb->min = TCITOHLL(llnum);
  memcpy(&llnum, hbuf + FDBMAXOFF, sizeof(llnum));
  fdb->max = TCITOHLL(llnum);
}


/* Clear all members.
   `fdb' specifies the fixed-length database object. */
static void tcfdbclear(TCFDB *fdb){
  assert(fdb);
  fdb->mmtx = NULL;
  fdb->amtx = NULL;
  fdb->rmtxs = NULL;
  fdb->tmtx = NULL;
  fdb->wmtx = NULL;
  fdb->eckey = NULL;
  fdb->rpath = NULL;
  fdb->type = TCDBTFIXED;
  fdb->flags = 0;
  fdb->width = FDBDEFWIDTH;
  fdb->limsiz = FDBDEFLIMSIZ;
  fdb->wsiz = 0;
  fdb->rsiz = 0;
  fdb->limid = 0;
  fdb->path = NULL;
  fdb->fd = -1;
  fdb->omode = 0;
  fdb->rnum = 0;
  fdb->fsiz = 0;
  fdb->min = 0;
  fdb->max = 0;
  fdb->iter = 0;
  fdb->map = NULL;
  fdb->array = NULL;
  fdb->ecode = TCESUCCESS;
  fdb->fatal = false;
  fdb->inode = 0;
  fdb->mtime = 0;
  fdb->tran = false;
  fdb->walfd = -1;
  fdb->walend = 0;
  fdb->dbgfd = -1;
  fdb->cnt_writerec = -1;
  fdb->cnt_readrec = -1;
  fdb->cnt_truncfile = -1;
  TCDODEBUG(fdb->cnt_writerec = 0);
  TCDODEBUG(fdb->cnt_readrec = 0);
  TCDODEBUG(fdb->cnt_truncfile = 0);
}


/* Set the open flag.
   `fdb' specifies the fixed-length database object.
   `flag' specifies the flag value.
   `sign' specifies the sign. */
static void tcfdbsetflag(TCFDB *fdb, int flag, bool sign){
  assert(fdb);
  char *fp = (char *)fdb->map + FDBFLAGSOFF;
  if(sign){
    *fp |= (uint8_t)flag;
  } else {
    *fp &= ~(uint8_t)flag;
  }
  fdb->flags = *fp;
}


/* Initialize the write ahead logging file.
   `fdb' specifies the fixed-length database object.
   If successful, the return value is true, else, it is false. */
static bool tcfdbwalinit(TCFDB *fdb){
  assert(fdb);
  if(lseek(fdb->walfd, 0, SEEK_SET) == -1){
    tcfdbsetecode(fdb, TCESEEK, __FILE__, __LINE__, __func__);
    return false;
  }
  if(ftruncate(fdb->walfd, 0) == -1){
    tcfdbsetecode(fdb, TCETRUNC, __FILE__, __LINE__, __func__);
    return false;
  }
  uint64_t llnum = fdb->fsiz;
  llnum = TCHTOILL(llnum);
  if(!tcwrite(fdb->walfd, &llnum, sizeof(llnum))){
    tcfdbsetecode(fdb, TCEWRITE, __FILE__, __LINE__, __func__);
    return false;
  }
  fdb->walend = fdb->fsiz;
  if(!tcfdbwalwrite(fdb, 0, FDBHEADSIZ)) return false;
  return true;
}


/* Write an event into the write ahead logging file.
   `fdb' specifies the fixed-length database object.
   `off' specifies the offset of the region to be updated.
   `size' specifies the size of the region.
   If successful, the return value is true, else, it is false. */
static bool tcfdbwalwrite(TCFDB *fdb, uint64_t off, int64_t size){
  assert(fdb && off >= 0 && size >= 0);
  if(off + size > fdb->walend) size = fdb->walend - off;
  if(size < 1) return true;
  char stack[FDBIOBUFSIZ];
  char *buf;
  if(size + sizeof(off) + sizeof(size) <= FDBIOBUFSIZ){
    buf = stack;
  } else {
    TCMALLOC(buf, size + sizeof(off) + sizeof(size));
  }
  char *wp = buf;
  uint64_t llnum = TCHTOILL(off);
  memcpy(wp, &llnum, sizeof(llnum));
  wp += sizeof(llnum);
  uint32_t lnum = TCHTOIL(size);
  memcpy(wp, &lnum, sizeof(lnum));
  wp += sizeof(lnum);
  memcpy(wp, fdb->map + off, size);
  wp += size;
  if(!FDBLOCKWAL(fdb)) return false;
  if(!tcwrite(fdb->walfd, buf, wp - buf)){
    tcfdbsetecode(fdb, TCEWRITE, __FILE__, __LINE__, __func__);
    if(buf != stack) TCFREE(buf);
    FDBUNLOCKWAL(fdb);
    return false;
  }
  if(buf != stack) TCFREE(buf);
  if((fdb->omode & FDBOTSYNC) && fsync(fdb->walfd) == -1){
    tcfdbsetecode(fdb, TCESYNC, __FILE__, __LINE__, __func__);
    FDBUNLOCKWAL(fdb);
    return false;
  }
  FDBUNLOCKWAL(fdb);
  return true;
}


/* Restore the database from the write ahead logging file.
   `fdb' specifies the fixed-length database object.
   `path' specifies the path of the database file.
   If successful, the return value is true, else, it is false. */
static int tcfdbwalrestore(TCFDB *fdb, const char *path){
  assert(fdb && path);
  char *tpath = tcsprintf("%s%c%s", path, MYEXTCHR, FDBWALSUFFIX);
  int walfd = open(tpath, O_RDONLY, FDBFILEMODE);
  TCFREE(tpath);
  if(walfd < 0) return false;
  bool err = false;
  uint64_t walsiz = 0;
  struct stat sbuf;
  if(fstat(walfd, &sbuf) == 0){
    walsiz = sbuf.st_size;
  } else {
    tcfdbsetecode(fdb, TCESTAT, __FILE__, __LINE__, __func__);
    err = true;
  }
  if(walsiz >= sizeof(walsiz) + FDBHEADSIZ){
    int dbfd = fdb->fd;
    int tfd = -1;
    if(!(fdb->omode & FDBOWRITER)){
      tfd = open(path, O_WRONLY, FDBFILEMODE);
      if(tfd >= 0){
        dbfd = tfd;
      } else {
        int ecode = TCEOPEN;
        switch(errno){
          case EACCES: ecode = TCENOPERM; break;
          case ENOENT: ecode = TCENOFILE; break;
          case ENOTDIR: ecode = TCENOFILE; break;
        }
        tcfdbsetecode(fdb, ecode, __FILE__, __LINE__, __func__);
        err = true;
      }
    }
    uint64_t fsiz = 0;
    if(tcread(walfd, &fsiz, sizeof(fsiz))){
      fsiz = TCITOHLL(fsiz);
    } else {
      tcfdbsetecode(fdb, TCEREAD, __FILE__, __LINE__, __func__);
      err = true;
    }
    TCLIST *list = tclistnew();
    uint64_t waloff = sizeof(fsiz);
    char stack[FDBIOBUFSIZ];
    while(waloff < walsiz){
      uint64_t off;
      uint32_t size;
      if(!tcread(walfd, stack, sizeof(off) + sizeof(size))){
        tcfdbsetecode(fdb, TCEREAD, __FILE__, __LINE__, __func__);
        err = true;
        break;
      }
      memcpy(&off, stack, sizeof(off));
      off = TCITOHLL(off);
      memcpy(&size, stack + sizeof(off), sizeof(size));
      size = TCITOHL(size);
      char *buf;
      if(sizeof(off) + size <= FDBIOBUFSIZ){
        buf = stack;
      } else {
        TCMALLOC(buf, sizeof(off) + size);
      }
      *(uint64_t *)buf = off;
      if(!tcread(walfd, buf + sizeof(off), size)){
        tcfdbsetecode(fdb, TCEREAD, __FILE__, __LINE__, __func__);
        err = true;
        if(buf != stack) TCFREE(buf);
        break;
      }
      TCLISTPUSH(list, buf, sizeof(off) + size);
      if(buf != stack) TCFREE(buf);
      waloff += sizeof(off) + sizeof(size) + size;
    }
    for(int i = TCLISTNUM(list) - 1; i >= 0; i--){
      const char *rec;
      int size;
      TCLISTVAL(rec, list, i, size);
      uint64_t off = *(uint64_t *)rec;
      rec += sizeof(off);
      size -= sizeof(off);
      if(lseek(dbfd, off, SEEK_SET) == -1){
        tcfdbsetecode(fdb, TCESEEK, __FILE__, __LINE__, __func__);
        err = true;
        break;
      }
      if(!tcwrite(dbfd, rec, size)){
        tcfdbsetecode(fdb, TCEWRITE, __FILE__, __LINE__, __func__);
        err = true;
        break;
      }
    }
    tclistdel(list);
    if(ftruncate(dbfd, fsiz) == -1){
      tcfdbsetecode(fdb, TCETRUNC, __FILE__, __LINE__, __func__);
      err = true;
    }
    if((fdb->omode & FDBOTSYNC) && fsync(dbfd) == -1){
      tcfdbsetecode(fdb, TCESYNC, __FILE__, __LINE__, __func__);
      err = true;
    }
    if(tfd >= 0 && close(tfd) == -1){
      tcfdbsetecode(fdb, TCECLOSE, __FILE__, __LINE__, __func__);
      err = true;
    }
  } else {
    err = true;
  }
  if(close(walfd) == -1){
    tcfdbsetecode(fdb, TCECLOSE, __FILE__, __LINE__, __func__);
    err = true;
  }
  return !err;
}


/* Remove the write ahead logging file.
   `fdb' specifies the fixed-length database object.
   `path' specifies the path of the database file.
   If successful, the return value is true, else, it is false. */
static bool tcfdbwalremove(TCFDB *fdb, const char *path){
  assert(fdb && path);
  char *tpath = tcsprintf("%s%c%s", path, MYEXTCHR, FDBWALSUFFIX);
  bool err = false;
  if(unlink(tpath) == -1 && errno != ENOENT){
    tcfdbsetecode(fdb, TCEUNLINK, __FILE__, __LINE__, __func__);
    err = true;
  }
  TCFREE(tpath);
  return !err;
}


/* Open a database file and connect a fixed-length database object.
   `fdb' specifies the fixed-length database object.
   `path' specifies the path of the database file.
   `omode' specifies the connection mode.
   If successful, the return value is true, else, it is false. */
static bool tcfdbopenimpl(TCFDB *fdb, const char *path, int omode){
  assert(fdb && path);
  int mode = O_RDONLY;
  if(omode & FDBOWRITER){
    mode = O_RDWR;
    if(omode & FDBOCREAT) mode |= O_CREAT;
  }
  int fd = open(path, mode, FDBFILEMODE);
  if(fd < 0){
    int ecode = TCEOPEN;
    switch(errno){
      case EACCES: ecode = TCENOPERM; break;
      case ENOENT: ecode = TCENOFILE; break;
      case ENOTDIR: ecode = TCENOFILE; break;
    }
    tcfdbsetecode(fdb, ecode, __FILE__, __LINE__, __func__);
    return false;
  }
  if(!(omode & FDBONOLCK)){
    if(!tclock(fd, omode & FDBOWRITER, omode & FDBOLCKNB)){
      tcfdbsetecode(fdb, TCELOCK, __FILE__, __LINE__, __func__);
      close(fd);
      return false;
    }
  }
  if((omode & FDBOWRITER) && (omode & FDBOTRUNC)){
    if(ftruncate(fd, 0) == -1){
      tcfdbsetecode(fdb, TCETRUNC, __FILE__, __LINE__, __func__);
      close(fd);
      return false;
    }
    if(!tcfdbwalremove(fdb, path)){
      close(fd);
      return false;
    }
  }
  struct stat sbuf;
  if(fstat(fd, &sbuf) == -1 || !S_ISREG(sbuf.st_mode)){
    tcfdbsetecode(fdb, TCESTAT, __FILE__, __LINE__, __func__);
    close(fd);
    return false;
  }
  char hbuf[FDBHEADSIZ];
  if((omode & FDBOWRITER) && sbuf.st_size < 1){
    fdb->flags = 0;
    fdb->rnum = 0;
    fdb->fsiz = FDBHEADSIZ;
    fdb->min = 0;
    fdb->max = 0;
    tcfdbdumpmeta(fdb, hbuf);
    if(!tcwrite(fd, hbuf, FDBHEADSIZ)){
      tcfdbsetecode(fdb, TCEWRITE, __FILE__, __LINE__, __func__);
      close(fd);
      return false;
    }
    sbuf.st_size = fdb->fsiz;
  }
  if(lseek(fd, 0, SEEK_SET) == -1){
    tcfdbsetecode(fdb, TCESEEK, __FILE__, __LINE__, __func__);
    close(fd);
    return false;
  }
  if(!tcread(fd, hbuf, FDBHEADSIZ)){
    tcfdbsetecode(fdb, TCEREAD, __FILE__, __LINE__, __func__);
    close(fd);
    return false;
  }
  int type = fdb->type;
  tcfdbloadmeta(fdb, hbuf);
  if((fdb->flags & FDBFOPEN) && tcfdbwalrestore(fdb, path)){
    if(lseek(fd, 0, SEEK_SET) == -1){
      tcfdbsetecode(fdb, TCESEEK, __FILE__, __LINE__, __func__);
      close(fd);
      return false;
    }
    if(!tcread(fd, hbuf, FDBHEADSIZ)){
      tcfdbsetecode(fdb, TCEREAD, __FILE__, __LINE__, __func__);
      close(fd);
      return false;
    }
    tcfdbloadmeta(fdb, hbuf);
    if(!tcfdbwalremove(fdb, path)){
      close(fd);
      return false;
    }
  }
  if(!(omode & FDBONOLCK)){
    if(memcmp(hbuf, FDBMAGICDATA, strlen(FDBMAGICDATA)) || fdb->type != type ||
       fdb->width < 1 || sbuf.st_size < fdb->fsiz || fdb->limsiz < FDBHEADSIZ ||
       fdb->fsiz > fdb->limsiz){
      tcfdbsetecode(fdb, TCEMETA, __FILE__, __LINE__, __func__);
      close(fd);
      return false;
    }
    if(sbuf.st_size > fdb->fsiz) fdb->fsiz = sbuf.st_size;
  }
  void *map = mmap(0, fdb->limsiz, PROT_READ | ((omode & FDBOWRITER) ? PROT_WRITE : 0),
                   MAP_SHARED, fd, 0);
  if(map == MAP_FAILED){
    tcfdbsetecode(fdb, TCEMMAP, __FILE__, __LINE__, __func__);
    close(fd);
    return false;
  }
  if(fdb->width <= UINT8_MAX){
    fdb->wsiz = sizeof(uint8_t);
  } else if(fdb->width <= UINT16_MAX){
    fdb->wsiz = sizeof(uint16_t);
  } else {
    fdb->wsiz = sizeof(uint32_t);
  }
  fdb->rsiz = fdb->width + fdb->wsiz;
  fdb->limid = (fdb->limsiz - FDBHEADSIZ) / fdb->rsiz;
  fdb->path = tcstrdup(path);
  fdb->fd = fd;
  fdb->omode = omode;
  fdb->iter = 0;
  fdb->map = map;
  fdb->array = (unsigned char *)map + FDBHEADSIZ;
  fdb->ecode = TCESUCCESS;
  fdb->fatal = false;
  fdb->inode = (uint64_t)sbuf.st_ino;
  fdb->mtime = sbuf.st_mtime;
  fdb->tran = false;
  fdb->walfd = -1;
  fdb->walend = 0;
  if(fdb->omode & FDBOWRITER) tcfdbsetflag(fdb, FDBFOPEN, true);
  return true;
}


/* Close a fixed-length database object.
   `fdb' specifies the fixed-length database object.
   If successful, the return value is true, else, it is false. */
static bool tcfdbcloseimpl(TCFDB *fdb){
  assert(fdb);
  bool err = false;
  if(fdb->omode & FDBOWRITER) tcfdbsetflag(fdb, FDBFOPEN, false);
  if((fdb->omode & FDBOWRITER) && !tcfdbmemsync(fdb, false)) err = true;
  if(munmap(fdb->map, fdb->limsiz) == -1){
    tcfdbsetecode(fdb, TCEMMAP, __FILE__, __LINE__, __func__);
    err = true;
  }
  if(fdb->tran){
    if(!tcfdbwalrestore(fdb, fdb->path)) err = true;
    fdb->tran = false;
  }
  if(fdb->walfd >= 0){
    if(close(fdb->walfd) == -1){
      tcfdbsetecode(fdb, TCECLOSE, __FILE__, __LINE__, __func__);
      err = true;
    }
    if(!fdb->fatal && !tcfdbwalremove(fdb, fdb->path)) err = true;
  }
  if(close(fdb->fd) == -1){
    tcfdbsetecode(fdb, TCECLOSE, __FILE__, __LINE__, __func__);
    err = true;
  }
  TCFREE(fdb->path);
  fdb->path = NULL;
  fdb->fd = -1;
  return !err;
}


/* Get the previous record of a record.
   `fdb' specifies the fixed-length database object.
   `id' specifies the ID number.
   The return value is the ID number of the previous record or 0 if no record corresponds. */
static int64_t tcfdbprevid(TCFDB *fdb, int64_t id){
  assert(fdb && id >= 0);
  id--;
  while(id >= fdb->min){
    TCDODEBUG(fdb->cnt_readrec++);
    unsigned char *rec = fdb->array + (id - 1) * (fdb->rsiz);
    unsigned char *rp = rec;
    uint32_t osiz;
    uint16_t snum;
    uint32_t lnum;
    switch(fdb->wsiz){
      case 1:
        osiz = *(rp++);
        break;
      case 2:
        memcpy(&snum, rp, sizeof(snum));
        osiz = TCITOHS(snum);
        rp += sizeof(snum);
        break;
      default:
        memcpy(&lnum, rp, sizeof(lnum));
        osiz = TCITOHL(lnum);
        rp += sizeof(lnum);
        break;
    }
    if(osiz > 0 || *rp != 0) return id;
    id--;
  }
  return 0;
}


/* Get the next record of a record.
   `fdb' specifies the fixed-length database object.
   `id' specifies the ID number.
   The return value is the ID number of the next record or 0 if no record corresponds. */
static int64_t tcfdbnextid(TCFDB *fdb, int64_t id){
  assert(fdb && id >= 0);
  id++;
  while(id <= fdb->max){
    TCDODEBUG(fdb->cnt_readrec++);
    unsigned char *rec = fdb->array + (id - 1) * (fdb->rsiz);
    unsigned char *rp = rec;
    uint32_t osiz;
    uint16_t snum;
    uint32_t lnum;
    switch(fdb->wsiz){
      case 1:
        osiz = *(rp++);
        break;
      case 2:
        memcpy(&snum, rp, sizeof(snum));
        osiz = TCITOHS(snum);
        rp += sizeof(snum);
        break;
      default:
        memcpy(&lnum, rp, sizeof(lnum));
        osiz = TCITOHL(lnum);
        rp += sizeof(lnum);
        break;
    }
    if(osiz > 0 || *rp != 0) return id;
    id++;
  }
  return 0;
}


/* Store a record.
   `fdb' specifies the fixed-length database object.
   `id' specifies the ID number.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   `dmode' specifies behavior when the key overlaps.
   If successful, the return value is true, else, it is false. */
static bool tcfdbputimpl(TCFDB *fdb, int64_t id, const void *vbuf, int vsiz, int dmode){
  assert(fdb && id > 0);
  if(vsiz > (int64_t)fdb->width) vsiz = fdb->width;
  TCDODEBUG(fdb->cnt_readrec++);
  unsigned char *rec = fdb->array + (id - 1) * (fdb->rsiz);
  uint64_t nsiz = FDBHEADSIZ + id * fdb->rsiz;
  if(nsiz > fdb->fsiz){
    if(nsiz > fdb->limsiz){
      tcfdbsetecode(fdb, TCEINVALID, __FILE__, __LINE__, __func__);
      return false;
    }
    if(!FDBLOCKATTR(fdb)) return false;
    if(nsiz > fdb->fsiz){
      if(vsiz < 0){
        tcfdbsetecode(fdb, TCENOREC, __FILE__, __LINE__, __func__);
        FDBUNLOCKATTR(fdb);
        return false;
      }
      if(nsiz + fdb->rsiz * FDBTRUNCALW < fdb->limsiz) nsiz += fdb->rsiz * FDBTRUNCALW;
      if(ftruncate(fdb->fd, nsiz) == -1){
        tcfdbsetecode(fdb, TCETRUNC, __FILE__, __LINE__, __func__);
        FDBUNLOCKATTR(fdb);
        return false;
      }
      TCDODEBUG(fdb->cnt_truncfile++);
      fdb->fsiz = nsiz;
      unsigned char *wp = rec;
      uint16_t snum;
      uint32_t lnum;
      switch(fdb->wsiz){
        case 1:
          *(wp++) = vsiz;
          break;
        case 2:
          snum = TCHTOIS(vsiz);
          memcpy(wp, &snum, sizeof(snum));
          wp += sizeof(snum);
          break;
        default:
          lnum = TCHTOIL(vsiz);
          memcpy(wp, &lnum, sizeof(lnum));
          wp += sizeof(lnum);
          break;
      }
      if(vsiz > 0){
        memcpy(wp, vbuf, vsiz);
      } else {
        *wp = 1;
      }
      TCDODEBUG(fdb->cnt_writerec++);
      fdb->rnum++;
      if(fdb->min < 1 || id < fdb->min) fdb->min = id;
      if(fdb->max < 1 || id > fdb->max) fdb->max = id;
      FDBUNLOCKATTR(fdb);
      return true;
    }
    FDBUNLOCKATTR(fdb);
  }
  unsigned char *rp = rec;
  uint32_t osiz;
  uint16_t snum;
  uint32_t lnum;
  switch(fdb->wsiz){
    case 1:
      osiz = *(rp++);
      break;
    case 2:
      memcpy(&snum, rp, sizeof(snum));
      osiz = TCITOHS(snum);
      rp += sizeof(snum);
      break;
    default:
      memcpy(&lnum, rp, sizeof(lnum));
      osiz = TCITOHL(lnum);
      rp += sizeof(lnum);
      break;
  }
  bool miss = osiz == 0 && *rp == 0;
  if(dmode != FDBPDOVER && !miss){
    if(dmode == FDBPDKEEP){
      tcfdbsetecode(fdb, TCEKEEP, __FILE__, __LINE__, __func__);
      return false;
    }
    if(dmode == FDBPDCAT){
      if(fdb->tran && !tcfdbwalwrite(fdb, (char *)rec - fdb->map, fdb->width)) return false;
      vsiz = tclmin(vsiz, fdb->width - osiz);
      unsigned char *wp = rec;
      int usiz = osiz + vsiz;
      switch(fdb->wsiz){
        case 1:
          *(wp++) = usiz;
          break;
        case 2:
          snum = TCHTOIS(usiz);
          memcpy(wp, &snum, sizeof(snum));
          wp += sizeof(snum);
          break;
        default:
          lnum = TCHTOIL(usiz);
          memcpy(wp, &lnum, sizeof(lnum));
          wp += sizeof(lnum);
          break;
      }
      if(usiz > 0){
        memcpy(wp + osiz, vbuf, vsiz);
      } else {
        *wp = 1;
      }
      TCDODEBUG(fdb->cnt_writerec++);
      return true;
    }
    if(dmode == FDBPDADDINT){
      if(osiz != sizeof(int)){
        tcfdbsetecode(fdb, TCEKEEP, __FILE__, __LINE__, __func__);
        return false;
      }
      int lnum;
      memcpy(&lnum, rp, sizeof(lnum));
      if(*(int *)vbuf == 0){
        *(int *)vbuf = lnum;
        return true;
      }
      if(fdb->tran && !tcfdbwalwrite(fdb, (char *)rec - fdb->map, fdb->width)) return false;
      lnum += *(int *)vbuf;
      *(int *)vbuf = lnum;
      memcpy(rp, &lnum, sizeof(lnum));
      TCDODEBUG(fdb->cnt_writerec++);
      return true;
    }
    if(dmode == FDBPDADDDBL){
      if(osiz != sizeof(double)){
        tcfdbsetecode(fdb, TCEKEEP, __FILE__, __LINE__, __func__);
        return false;
      }
      double dnum;
      memcpy(&dnum, rp, sizeof(dnum));
      if(*(double *)vbuf == 0.0){
        *(double *)vbuf = dnum;
        return true;
      }
      if(fdb->tran && !tcfdbwalwrite(fdb, (char *)rec - fdb->map, fdb->width)) return false;
      dnum += *(double *)vbuf;
      *(double *)vbuf = dnum;
      memcpy(rp, &dnum, sizeof(dnum));
      TCDODEBUG(fdb->cnt_writerec++);
      return true;
    }
    if(dmode == FDBPDPROC){
      FDBPDPROCOP *procptr = *(FDBPDPROCOP **)((char *)vbuf - sizeof(procptr));
      int nvsiz;
      char *nvbuf = procptr->proc(rp, osiz, &nvsiz, procptr->op);
      if(nvbuf == (void *)-1){
        if(fdb->tran && !tcfdbwalwrite(fdb, (char *)rec - fdb->map, fdb->width)) return false;
        memset(rec, 0, fdb->wsiz + 1);
        TCDODEBUG(fdb->cnt_writerec++);
        if(!FDBLOCKATTR(fdb)) return false;
        fdb->rnum--;
        if(fdb->rnum < 1){
          fdb->min = 0;
          fdb->max = 0;
        } else if(fdb->rnum < 2){
          if(fdb->min == id){
            fdb->min = fdb->max;
          } else if(fdb->max == id){
            fdb->max = fdb->min;
          }
        } else {
          if(id == fdb->min) fdb->min = tcfdbnextid(fdb, id);
          if(id == fdb->max) fdb->max = tcfdbprevid(fdb, id);
        }
        FDBUNLOCKATTR(fdb);
        return true;
      }
      if(!nvbuf){
        tcfdbsetecode(fdb, TCEKEEP, __FILE__, __LINE__, __func__);
        return false;
      }
      if(fdb->tran && !tcfdbwalwrite(fdb, (char *)rec - fdb->map, fdb->width)) return false;
      if(nvsiz > fdb->width) nvsiz = fdb->width;
      unsigned char *wp = rec;
      switch(fdb->wsiz){
        case 1:
          *(wp++) = nvsiz;
          break;
        case 2:
          snum = TCHTOIS(nvsiz);
          memcpy(wp, &snum, sizeof(snum));
          wp += sizeof(snum);
          break;
        default:
          lnum = TCHTOIL(nvsiz);
          memcpy(wp, &lnum, sizeof(lnum));
          wp += sizeof(lnum);
          break;
      }
      if(nvsiz > 0){
        memcpy(wp, nvbuf, nvsiz);
      } else {
        *wp = 1;
      }
      TCFREE(nvbuf);
      TCDODEBUG(fdb->cnt_writerec++);
      return true;
    }
  }
  if(vsiz < 0){
    tcfdbsetecode(fdb, TCENOREC, __FILE__, __LINE__, __func__);
    return false;
  }
  if(fdb->tran && !tcfdbwalwrite(fdb, (char *)rec - fdb->map, fdb->width)) return false;
  unsigned char *wp = rec;
  switch(fdb->wsiz){
    case 1:
      *(wp++) = vsiz;
      break;
    case 2:
      snum = TCHTOIS(vsiz);
      memcpy(wp, &snum, sizeof(snum));
      wp += sizeof(snum);
      break;
    default:
      lnum = TCHTOIL(vsiz);
      memcpy(wp, &lnum, sizeof(lnum));
      wp += sizeof(lnum);
      break;
  }
  if(vsiz > 0){
    memcpy(wp, vbuf, vsiz);
  } else {
    *wp = 1;
  }
  TCDODEBUG(fdb->cnt_writerec++);
  if(miss){
    if(!FDBLOCKATTR(fdb)) return false;
    fdb->rnum++;
    if(fdb->min < 1 || id < fdb->min) fdb->min = id;
    if(fdb->max < 1 || id > fdb->max) fdb->max = id;
    FDBUNLOCKATTR(fdb);
  }
  return true;
}


/* Remove a record of a fixed-length database object.
   `fdb' specifies the fixed-length database object.
   `id' specifies the ID number.
   If successful, the return value is true, else, it is false. */
static bool tcfdboutimpl(TCFDB *fdb, int64_t id){
  assert(fdb && id >= 0);
  TCDODEBUG(fdb->cnt_readrec++);
  unsigned char *rec = fdb->array + (id - 1) * (fdb->rsiz);
  uint64_t nsiz = FDBHEADSIZ + id * fdb->rsiz;
  if(nsiz > fdb->fsiz){
    tcfdbsetecode(fdb, TCENOREC, __FILE__, __LINE__, __func__);
    return false;
  }
  unsigned char *rp = rec;
  uint32_t osiz;
  uint16_t snum;
  uint32_t lnum;
  switch(fdb->wsiz){
    case 1:
      osiz = *(rp++);
      break;
    case 2:
      memcpy(&snum, rp, sizeof(snum));
      osiz = TCITOHS(snum);
      rp += sizeof(snum);
      break;
    default:
      memcpy(&lnum, rp, sizeof(lnum));
      osiz = TCITOHL(lnum);
      rp += sizeof(lnum);
      break;
  }
  if(osiz == 0 && *rp == 0){
    tcfdbsetecode(fdb, TCENOREC, __FILE__, __LINE__, __func__);
    return false;
  }
  if(fdb->tran && !tcfdbwalwrite(fdb, (char *)rec - fdb->map, fdb->width)) return false;
  memset(rec, 0, fdb->wsiz + 1);
  TCDODEBUG(fdb->cnt_writerec++);
  if(!FDBLOCKATTR(fdb)) return false;
  fdb->rnum--;
  if(fdb->rnum < 1){
    fdb->min = 0;
    fdb->max = 0;
  } else if(fdb->rnum < 2){
    if(fdb->min == id){
      fdb->min = fdb->max;
    } else if(fdb->max == id){
      fdb->max = fdb->min;
    }
  } else {
    if(id == fdb->min) fdb->min = tcfdbnextid(fdb, id);
    if(id == fdb->max) fdb->max = tcfdbprevid(fdb, id);
  }
  FDBUNLOCKATTR(fdb);
  return true;
}


/* Retrieve a record.
   `fdb' specifies the fixed-length database object.
   `id' specifies the ID number.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the region of the value of the corresponding
   record. */
static const void *tcfdbgetimpl(TCFDB *fdb, int64_t id, int *sp){
  assert(fdb && id >= 0 && sp);
  TCDODEBUG(fdb->cnt_readrec++);
  unsigned char *rec = fdb->array + (id - 1) * (fdb->rsiz);
  uint64_t nsiz = FDBHEADSIZ + id * fdb->rsiz;
  if(nsiz > fdb->fsiz){
    tcfdbsetecode(fdb, TCENOREC, __FILE__, __LINE__, __func__);
    return false;
  }
  unsigned char *rp = rec;
  uint32_t osiz;
  uint16_t snum;
  uint32_t lnum;
  switch(fdb->wsiz){
    case 1:
      osiz = *(rp++);
      break;
    case 2:
      memcpy(&snum, rp, sizeof(snum));
      osiz = TCITOHS(snum);
      rp += sizeof(snum);
      break;
    default:
      memcpy(&lnum, rp, sizeof(lnum));
      osiz = TCITOHL(lnum);
      rp += sizeof(lnum);
      break;
  }
  if(osiz == 0 && *rp == 0){
    tcfdbsetecode(fdb, TCENOREC, __FILE__, __LINE__, __func__);
    return false;
  }
  *sp = osiz;
  return rp;
}


/* Initialize the iterator of a fixed-length database object.
   `fdb' specifies the fixed-length database object.
   If successful, the return value is true, else, it is false. */
static bool tcfdbiterinitimpl(TCFDB *fdb){
  assert(fdb);
  fdb->iter = fdb->min;
  return true;
}


/* Get the next key of the iterator of a fixed-length database object.
   `fdb' specifies the fixed-length database object.
   If successful, the return value is the next ID number of the iterator, else, it is 0. */
static uint64_t tcfdbiternextimpl(TCFDB *fdb){
  assert(fdb);
  if(fdb->iter < 1){
    tcfdbsetecode(fdb, TCENOREC, __FILE__, __LINE__, __func__);
    return 0;
  }
  uint64_t cur = fdb->iter;
  fdb->iter = tcfdbnextid(fdb, fdb->iter);
  return cur;
}


/* Get range matching ID numbers in a fixed-length database object.
   `fdb' specifies the fixed-length database object.
   `lower' specifies the lower limit of the range.
   `upper' specifies the upper limit of the range.
   `max' specifies the maximum number of keys to be fetched.
   `np' specifies the pointer to the variable into which the number of elements of the return
   value is assigned.
   If successful, the return value is the pointer to an array of ID numbers of the corresponding
   records. */
static uint64_t *tcfdbrangeimpl(TCFDB *fdb, int64_t lower, int64_t upper, int max, int *np){
  assert(fdb && lower > 0 && upper > 0 && np);
  if(lower < fdb->min) lower = fdb->min;
  if(upper > fdb->max) upper = fdb->max;
  if(max < 0) max = INT_MAX;
  int anum = FDBIDARYUNIT;
  uint64_t *ids;
  TCMALLOC(ids, anum * sizeof(*ids));
  int num = 0;
  for(int64_t i = lower; i <= upper && num < max; i++){
    int vsiz;
    const void *vbuf = tcfdbgetimpl(fdb, i, &vsiz);
    if(vbuf){
      if(num >= anum){
        anum *= 2;
        TCREALLOC(ids, ids, anum * sizeof(*ids));
      }
      ids[num++] = i;
    }
  }
  *np = num;
  return ids;
}


/* Optimize the file of a fixed-length database object.
   `fdb' specifies the fixed-length database object.
   `width' specifies the width of the value of each record.
   `limsiz' specifies the limit size of the database file.
   If successful, the return value is true, else, it is false. */
static bool tcfdboptimizeimpl(TCFDB *fdb, int32_t width, int64_t limsiz){
  assert(fdb);
  char *tpath = tcsprintf("%s%ctmp%c%llu", fdb->path, MYEXTCHR, MYEXTCHR, fdb->inode);
  TCFDB *tfdb = tcfdbnew();
  tfdb->dbgfd = fdb->dbgfd;
  if(width < 1) width = fdb->width;
  if(limsiz < 1) limsiz = fdb->limsiz;
  tcfdbtune(tfdb, width, limsiz);
  if(!tcfdbopen(tfdb, tpath, FDBOWRITER | FDBOCREAT | FDBOTRUNC)){
    tcfdbsetecode(fdb, tfdb->ecode, __FILE__, __LINE__, __func__);
    tcfdbdel(tfdb);
    TCFREE(tpath);
    return false;
  }
  bool err = false;
  int64_t max = fdb->max;
  for(int i = fdb->min; !err && i <= max; i++){
    int vsiz;
    const void *vbuf = tcfdbgetimpl(fdb, i, &vsiz);
    if(vbuf && !tcfdbput(tfdb, i, vbuf, vsiz)){
      tcfdbsetecode(fdb, tfdb->ecode, __FILE__, __LINE__, __func__);
      err = true;
    }
  }
  if(!tcfdbclose(tfdb)){
    tcfdbsetecode(fdb, tfdb->ecode, __FILE__, __LINE__, __func__);
    err = true;
  }
  tcfdbdel(tfdb);
  if(unlink(fdb->path) == -1){
    tcfdbsetecode(fdb, TCEUNLINK, __FILE__, __LINE__, __func__);
    err = true;
  }
  if(rename(tpath, fdb->path) == -1){
    tcfdbsetecode(fdb, TCERENAME, __FILE__, __LINE__, __func__);
    err = true;
  }
  TCFREE(tpath);
  if(err) return false;
  tpath = tcstrdup(fdb->path);
  int omode = (fdb->omode & ~FDBOCREAT) & ~FDBOTRUNC;
  if(!tcfdbcloseimpl(fdb)){
    TCFREE(tpath);
    return false;
  }
  bool rv = tcfdbopenimpl(fdb, tpath, omode);
  TCFREE(tpath);
  return rv;
}


/* Remove all records of a fixed-length database object.
   `fdb' specifies the fixed-length database object.
   If successful, the return value is true, else, it is false. */
static bool tcfdbvanishimpl(TCFDB *fdb){
  assert(fdb);
  char *path = tcstrdup(fdb->path);
  int omode = fdb->omode;
  bool err = false;
  if(!tcfdbcloseimpl(fdb)) err = true;
  if(!tcfdbopenimpl(fdb, path, FDBOTRUNC | omode)){
    tcpathunlock(fdb->rpath);
    TCFREE(fdb->rpath);
    err = true;
  }
  TCFREE(path);
  return !err;
}


/* Copy the database file of a fixed-length database object.
   `fdb' specifies the fixed-length database object.
   `path' specifies the path of the destination file.
   If successful, the return value is true, else, it is false. */
static bool tcfdbcopyimpl(TCFDB *fdb, const char *path){
  assert(fdb && path);
  bool err = false;
  if(fdb->omode & FDBOWRITER){
    if(!tcfdbmemsync(fdb, false)) err = true;
    tcfdbsetflag(fdb, FDBFOPEN, false);
  }
  if(*path == '@'){
    char tsbuf[TCNUMBUFSIZ];
    sprintf(tsbuf, "%llu", (unsigned long long)(tctime() * 1000000));
    const char *args[3];
    args[0] = path + 1;
    args[1] = fdb->path;
    args[2] = tsbuf;
    if(tcsystem(args, sizeof(args) / sizeof(*args)) != 0) err = true;
  } else {
    if(!tccopyfile(fdb->path, path)){
      tcfdbsetecode(fdb, TCEMISC, __FILE__, __LINE__, __func__);
      err = true;
    }
  }
  if(fdb->omode & FDBOWRITER) tcfdbsetflag(fdb, FDBFOPEN, true);
  return !err;
}


/* Move the iterator to the record corresponding a key of a fixed-length database object.
   `fdb' specifies the fixed-length database object.
   `id' specifies the ID number.
   If successful, the return value is true, else, it is false. */
static bool tcfdbiterjumpimpl(TCFDB *fdb, int64_t id){
  assert(fdb && id >= 0);
  if(id <= fdb->min){
    fdb->iter = fdb->min;
  } else {
    int vsiz;
    if(tcfdbgetimpl(fdb, id, &vsiz)){
      fdb->iter = id;
    } else {
      uint64_t iter = tcfdbnextid(fdb, id);
      if(iter > 0){
        fdb->iter = iter;
      } else {
        return false;
      }
    }
  }
  return true;
}


/* Process each record atomically of a fixed-length database object.
   `fdb' specifies the fixed-length database object.
   `iter' specifies the pointer to the iterator function called for each record.
   `op' specifies an arbitrary pointer to be given as a parameter of the iterator function.
   If successful, the return value is true, else, it is false. */
static bool tcfdbforeachimpl(TCFDB *fdb, TCITER iter, void *op){
  bool err = false;
  uint64_t id = fdb->min;
  while(id > 0){
    int vsiz;
    const void *vbuf = tcfdbgetimpl(fdb, id, &vsiz);
    if(vbuf){
      char kbuf[TCNUMBUFSIZ];
      int ksiz = sprintf(kbuf, "%llu", (unsigned long long)id);
      if(!iter(kbuf, ksiz, vbuf, vsiz, op)) break;
    } else {
      tcfdbsetecode(fdb, TCEMISC, __FILE__, __LINE__, __func__);
      err = true;
    }
    id = tcfdbnextid(fdb, id);
  }
  return !err;
}


/* Lock a method of the fixed-length database object.
   `fdb' specifies the fixed-length database object.
   `wr' specifies whether the lock is writer or not.
   If successful, the return value is true, else, it is false. */
static bool tcfdblockmethod(TCFDB *fdb, bool wr){
  assert(fdb);
  if(wr ? pthread_rwlock_wrlock(fdb->mmtx) != 0 : pthread_rwlock_rdlock(fdb->mmtx) != 0){
    tcfdbsetecode(fdb, TCETHREAD, __FILE__, __LINE__, __func__);
    return false;
  }
  TCTESTYIELD();
  return true;
}


/* Unlock a method of the fixed-length database object.
   `fdb' specifies the fixed-length database object.
   If successful, the return value is true, else, it is false. */
static bool tcfdbunlockmethod(TCFDB *fdb){
  assert(fdb);
  if(pthread_rwlock_unlock(fdb->mmtx) != 0){
    tcfdbsetecode(fdb, TCETHREAD, __FILE__, __LINE__, __func__);
    return false;
  }
  TCTESTYIELD();
  return true;
}


/* Lock the attributes of the fixed-length database object.
   `fdb' specifies the fixed-length database object.
   If successful, the return value is true, else, it is false. */
static bool tcfdblockattr(TCFDB *fdb){
  assert(fdb);
  if(pthread_mutex_lock(fdb->amtx) != 0){
    tcfdbsetecode(fdb, TCETHREAD, __FILE__, __LINE__, __func__);
    return false;
  }
  TCTESTYIELD();
  return true;
}


/* Unlock the attributes of the fixed-length database object.
   `fdb' specifies the fixed-length database object.
   If successful, the return value is true, else, it is false. */
static bool tcfdbunlockattr(TCFDB *fdb){
  assert(fdb);
  if(pthread_mutex_unlock(fdb->amtx) != 0){
    tcfdbsetecode(fdb, TCETHREAD, __FILE__, __LINE__, __func__);
    return false;
  }
  TCTESTYIELD();
  return true;
}


/* Lock a record of the fixed-length database object.
   `fdb' specifies the fixed-length database object.
   `wr' specifies whether the lock is writer or not.
   If successful, the return value is true, else, it is false. */
static bool tcfdblockrecord(TCFDB *fdb, bool wr, uint64_t id){
  assert(fdb && id > 0);
  if(wr ? pthread_rwlock_wrlock((pthread_rwlock_t *)fdb->rmtxs + id % FDBRMTXNUM) != 0 :
     pthread_rwlock_rdlock((pthread_rwlock_t *)fdb->rmtxs + id % FDBRMTXNUM) != 0){
    tcfdbsetecode(fdb, TCETHREAD, __FILE__, __LINE__, __func__);
    return false;
  }
  TCTESTYIELD();
  return true;
}


/* Unlock a record of the fixed-length database object.
   `fdb' specifies the fixed-length database object.
   If successful, the return value is true, else, it is false. */
static bool tcfdbunlockrecord(TCFDB *fdb, uint64_t id){
  assert(fdb);
  if(pthread_rwlock_unlock((pthread_rwlock_t *)fdb->rmtxs + id % FDBRMTXNUM) != 0){
    tcfdbsetecode(fdb, TCETHREAD, __FILE__, __LINE__, __func__);
    return false;
  }
  TCTESTYIELD();
  return true;
}


/* Lock all records of the fixed-length database object.
   `fdb' specifies the fixed-length database object.
   `wr' specifies whether the lock is writer or not.
   If successful, the return value is true, else, it is false. */
static bool tcfdblockallrecords(TCFDB *fdb, bool wr){
  assert(fdb);
  for(int i = 0; i < FDBRMTXNUM; i++){
    if(wr ? pthread_rwlock_wrlock((pthread_rwlock_t *)fdb->rmtxs + i) != 0 :
       pthread_rwlock_rdlock((pthread_rwlock_t *)fdb->rmtxs + i) != 0){
      tcfdbsetecode(fdb, TCETHREAD, __FILE__, __LINE__, __func__);
      while(--i >= 0){
        pthread_rwlock_unlock((pthread_rwlock_t *)fdb->rmtxs + i);
      }
      return false;
    }
  }
  TCTESTYIELD();
  return true;
}


/* Unlock all records of the fixed-length database object.
   `fdb' specifies the fixed-length database object.
   If successful, the return value is true, else, it is false. */
static bool tcfdbunlockallrecords(TCFDB *fdb){
  assert(fdb);
  bool err = false;
  for(int i = FDBRMTXNUM - 1; i >= 0; i--){
    if(pthread_rwlock_unlock((pthread_rwlock_t *)fdb->rmtxs + i)) err = true;
  }
  TCTESTYIELD();
  if(err){
    tcfdbsetecode(fdb, TCETHREAD, __FILE__, __LINE__, __func__);
    return false;
  }
  return true;
}


/* Lock the write ahead logging file of the fixed-length database object.
   `fdb' specifies the fixed-length database object.
   If successful, the return value is true, else, it is false. */
static bool tcfdblockwal(TCFDB *fdb){
  assert(fdb);
  if(pthread_mutex_lock(fdb->wmtx) != 0){
    tcfdbsetecode(fdb, TCETHREAD, __FILE__, __LINE__, __func__);
    return false;
  }
  TCTESTYIELD();
  return true;
}


/* Unlock the write ahead logging file of the fixed-length database object.
   `fdb' specifies the fixed-length database object.
   If successful, the return value is true, else, it is false. */
static bool tcfdbunlockwal(TCFDB *fdb){
  assert(fdb);
  if(pthread_mutex_unlock(fdb->wmtx) != 0){
    tcfdbsetecode(fdb, TCETHREAD, __FILE__, __LINE__, __func__);
    return false;
  }
  TCTESTYIELD();
  return true;
}



/*************************************************************************************************
 * debugging functions
 *************************************************************************************************/


/* Print meta data of the header into the debugging output.
   `fdb' specifies the fixed-length database object. */
void tcfdbprintmeta(TCFDB *fdb){
  assert(fdb);
  if(fdb->dbgfd < 0) return;
  int dbgfd = (fdb->dbgfd == UINT16_MAX) ? 1 : fdb->dbgfd;
  char buf[FDBIOBUFSIZ];
  char *wp = buf;
  wp += sprintf(wp, "META:");
  wp += sprintf(wp, " mmtx=%p", (void *)fdb->mmtx);
  wp += sprintf(wp, " amtx=%p", (void *)fdb->amtx);
  wp += sprintf(wp, " rmtxs=%p", (void *)fdb->rmtxs);
  wp += sprintf(wp, " tmtx=%p", (void *)fdb->tmtx);
  wp += sprintf(wp, " wmtx=%p", (void *)fdb->wmtx);
  wp += sprintf(wp, " eckey=%p", (void *)fdb->eckey);
  wp += sprintf(wp, " rpath=%s", fdb->rpath ? fdb->rpath : "-");
  wp += sprintf(wp, " type=%02X", fdb->type);
  wp += sprintf(wp, " flags=%02X", fdb->flags);
  wp += sprintf(wp, " width=%u", fdb->width);
  wp += sprintf(wp, " limsiz=%llu", (unsigned long long)fdb->limsiz);
  wp += sprintf(wp, " wsiz=%u", fdb->wsiz);
  wp += sprintf(wp, " rsiz=%u", fdb->rsiz);
  wp += sprintf(wp, " limid=%llu", (unsigned long long)fdb->limid);
  wp += sprintf(wp, " path=%s", fdb->path ? fdb->path : "-");
  wp += sprintf(wp, " fd=%d", fdb->fd);
  wp += sprintf(wp, " omode=%u", fdb->omode);
  wp += sprintf(wp, " rnum=%llu", (unsigned long long)fdb->rnum);
  wp += sprintf(wp, " fsiz=%llu", (unsigned long long)fdb->fsiz);
  wp += sprintf(wp, " min=%llu", (unsigned long long)fdb->min);
  wp += sprintf(wp, " max=%llu", (unsigned long long)fdb->max);
  wp += sprintf(wp, " iter=%llu", (unsigned long long)fdb->iter);
  wp += sprintf(wp, " map=%p", (void *)fdb->map);
  wp += sprintf(wp, " array=%p", (void *)fdb->array);
  wp += sprintf(wp, " ecode=%d", fdb->ecode);
  wp += sprintf(wp, " fatal=%u", fdb->fatal);
  wp += sprintf(wp, " inode=%llu", (unsigned long long)fdb->inode);
  wp += sprintf(wp, " mtime=%llu", (unsigned long long)fdb->mtime);
  wp += sprintf(wp, " tran=%d", fdb->tran);
  wp += sprintf(wp, " walfd=%d", fdb->walfd);
  wp += sprintf(wp, " walend=%llu", (unsigned long long)fdb->walend);
  wp += sprintf(wp, " dbgfd=%d", fdb->dbgfd);
  wp += sprintf(wp, " cnt_writerec=%lld", (long long)fdb->cnt_writerec);
  wp += sprintf(wp, " cnt_readrec=%lld", (long long)fdb->cnt_readrec);
  wp += sprintf(wp, " cnt_truncfile=%lld", (long long)fdb->cnt_truncfile);
  *(wp++) = '\n';
  tcwrite(dbgfd, buf, wp - buf);
}



// END OF FILE
