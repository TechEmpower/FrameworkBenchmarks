/*************************************************************************************************
 * The utility API of Tokyo Cabinet
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


#ifndef _TCUTIL_H                        /* duplication check */
#define _TCUTIL_H

#if defined(__cplusplus)
#define __TCUTIL_CLINKAGEBEGIN extern "C" {
#define __TCUTIL_CLINKAGEEND }
#else
#define __TCUTIL_CLINKAGEBEGIN
#define __TCUTIL_CLINKAGEEND
#endif
__TCUTIL_CLINKAGEBEGIN


#include <stdlib.h>
#if ! defined(__cplusplus)
#include <stdbool.h>
#endif
#include <stdint.h>
#include <time.h>
#include <limits.h>
#include <math.h>



/*************************************************************************************************
 * basic utilities
 *************************************************************************************************/


/* String containing the version information. */
extern const char *tcversion;


/* Pointer to the call back function for handling a fatal error.
   The argument specifies the error message.
   The initial value of this variable is `NULL'.  If the value is `NULL', the default function is
   called when a fatal error occurs.  A fatal error occurs when memory allocation is failed. */
extern void (*tcfatalfunc)(const char *);


/* Allocate a region on memory.
   `size' specifies the size of the region.
   The return value is the pointer to the allocated region.
   This function handles failure of memory allocation implicitly.  Because the region of the
   return value is allocated with the `malloc' call, it should be released with the `free' call
   when it is no longer in use. */
void *tcmalloc(size_t size);


/* Allocate a nullified region on memory.
   `nmemb' specifies the number of elements.
   `size' specifies the size of each element.
   The return value is the pointer to the allocated nullified region.
   This function handles failure of memory allocation implicitly.  Because the region of the
   return value is allocated with the `calloc' call, it should be released with the `free' call
   when it is no longer in use. */
void *tccalloc(size_t nmemb, size_t size);


/* Re-allocate a region on memory.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   The return value is the pointer to the re-allocated region.
   This function handles failure of memory allocation implicitly.  Because the region of the
   return value is allocated with the `realloc' call, it should be released with the `free' call
   when it is no longer in use. */
void *tcrealloc(void *ptr, size_t size);


/* Duplicate a region on memory.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   The return value is the pointer to the allocated region of the duplicate.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when
   it is no longer in use. */
void *tcmemdup(const void *ptr, size_t size);


/* Duplicate a string on memory.
   `str' specifies the string.
   The return value is the allocated string equivalent to the specified string.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tcstrdup(const void *str);


/* Free a region on memory.
   `ptr' specifies the pointer to the region.  If it is `NULL', this function has no effect.
   Although this function is just a wrapper of `free' call, this is useful in applications using
   another package of the `malloc' series. */
void tcfree(void *ptr);



/*************************************************************************************************
 * basic utilities (for experts)
 *************************************************************************************************/


/* type of the pointer to a comparison function.
   `aptr' specifies the pointer to the region of one key.
   `asiz' specifies the size of the region of one key.
   `bptr' specifies the pointer to the region of the other key.
   `bsiz' specifies the size of the region of the other key.
   `op' specifies the pointer to the optional opaque object.
   The return value is positive if the former is big, negative if the latter is big, 0 if both
   are equivalent. */
typedef int (*TCCMP)(const char *aptr, int asiz, const char *bptr, int bsiz, void *op);

/* type of the pointer to a encoding or decoding function.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   `op' specifies the pointer to the optional opaque object.
   If successful, the return value is the pointer to the result object allocated with `malloc'
   call, else, it is `NULL'. */
typedef void *(*TCCODEC)(const void *ptr, int size, int *sp, void *op);

/* type of the pointer to a callback function to process record duplication.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   `op' specifies the pointer to the optional opaque object.
   The return value is the pointer to the result object allocated with `malloc'.  It is
   released by the caller.  If it is `NULL', the record is not modified. */
typedef void *(*TCPDPROC)(const void *vbuf, int vsiz, int *sp, void *op);

/* type of the pointer to a iterator function.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   `op' specifies the pointer to the optional opaque object.
   The return value is true to continue iteration or false to stop iteration. */
typedef bool (*TCITER)(const void *kbuf, int ksiz, const void *vbuf, int vsiz, void *op);



/*************************************************************************************************
 * extensible string
 *************************************************************************************************/


typedef struct {                         /* type of structure for an extensible string object */
  char *ptr;                             /* pointer to the region */
  int size;                              /* size of the region */
  int asize;                             /* size of the allocated region */
} TCXSTR;


/* Create an extensible string object.
   The return value is the new extensible string object. */
TCXSTR *tcxstrnew(void);


/* Create an extensible string object from a character string.
   `str' specifies the string of the initial content.
   The return value is the new extensible string object containing the specified string. */
TCXSTR *tcxstrnew2(const char *str);


/* Create an extensible string object with the initial allocation size.
   `asiz' specifies the initial allocation size.
   The return value is the new extensible string object. */
TCXSTR *tcxstrnew3(int asiz);


/* Copy an extensible string object.
   `xstr' specifies the extensible string object.
   The return value is the new extensible string object equivalent to the specified object. */
TCXSTR *tcxstrdup(const TCXSTR *xstr);


/* Delete an extensible string object.
   `xstr' specifies the extensible string object.
   Note that the deleted object and its derivatives can not be used anymore. */
void tcxstrdel(TCXSTR *xstr);


/* Concatenate a region to the end of an extensible string object.
   `xstr' specifies the extensible string object.
   `ptr' specifies the pointer to the region to be appended.
   `size' specifies the size of the region. */
void tcxstrcat(TCXSTR *xstr, const void *ptr, int size);


/* Concatenate a character string to the end of an extensible string object.
   `xstr' specifies the extensible string object.
   `str' specifies the string to be appended. */
void tcxstrcat2(TCXSTR *xstr, const char *str);


/* Get the pointer of the region of an extensible string object.
   `xstr' specifies the extensible string object.
   The return value is the pointer of the region of the object.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string. */
const void *tcxstrptr(const TCXSTR *xstr);


/* Get the size of the region of an extensible string object.
   `xstr' specifies the extensible string object.
   The return value is the size of the region of the object. */
int tcxstrsize(const TCXSTR *xstr);


/* Clear an extensible string object.
   `xstr' specifies the extensible string object.
   The internal buffer of the object is cleared and the size is set zero. */
void tcxstrclear(TCXSTR *xstr);


/* Perform formatted output into an extensible string object.
   `xstr' specifies the extensible string object.
   `format' specifies the printf-like format string.  The conversion character `%' can be used
   with such flag characters as `s', `d', `o', `u', `x', `X', `c', `e', `E', `f', `g', `G', `@',
   `?', `b', and `%'.  `@' works as with `s' but escapes meta characters of XML.  `?' works as
   with `s' but escapes meta characters of URL.  `b' converts an integer to the string as binary
   numbers.  The other conversion character work as with each original.
   The other arguments are used according to the format string. */
void tcxstrprintf(TCXSTR *xstr, const char *format, ...);


/* Allocate a formatted string on memory.
   `format' specifies the printf-like format string.  The conversion character `%' can be used
   with such flag characters as `s', `d', `o', `u', `x', `X', `c', `e', `E', `f', `g', `G', `@',
   `?', `b', and `%'.  `@' works as with `s' but escapes meta characters of XML.  `?' works as
   with `s' but escapes meta characters of URL.  `b' converts an integer to the string as binary
   numbers.  The other conversion character work as with each original.
   The other arguments are used according to the format string.
   The return value is the pointer to the region of the result string.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tcsprintf(const char *format, ...);



/*************************************************************************************************
 * extensible string (for experts)
 *************************************************************************************************/


/* Convert an extensible string object into a usual allocated region.
   `xstr' specifies the extensible string object.
   The return value is the pointer to the allocated region of the object.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when it
   is no longer in use.  Because the region of the original object is deleted, it should not be
   deleted again. */
void *tcxstrtomalloc(TCXSTR *xstr);


/* Create an extensible string object from an allocated region.
   `ptr' specifies the pointer to the region allocated with `malloc' call.
   `size' specifies the size of the region.
   The return value is the new extensible string object wrapping the specified region.
   Note that the specified region is released when the object is deleted. */
TCXSTR *tcxstrfrommalloc(void *ptr, int size);



/*************************************************************************************************
 * array list
 *************************************************************************************************/


typedef struct {                         /* type of structure for an element of a list */
  char *ptr;                             /* pointer to the region */
  int size;                              /* size of the effective region */
} TCLISTDATUM;

typedef struct {                         /* type of structure for an array list */
  TCLISTDATUM *array;                    /* array of data */
  int anum;                              /* number of the elements of the array */
  int start;                             /* start index of used elements */
  int num;                               /* number of used elements */
} TCLIST;


/* Create a list object.
   The return value is the new list object. */
TCLIST *tclistnew(void);


/* Create a list object with expecting the number of elements.
   `anum' specifies the number of elements expected to be stored in the list.
   The return value is the new list object. */
TCLIST *tclistnew2(int anum);


/* Create a list object with initial string elements.
   `str' specifies the string of the first element.
   The other arguments are other elements.  They should be trailed by a `NULL' argument.
   The return value is the new list object. */
TCLIST *tclistnew3(const char *str, ...);


/* Copy a list object.
   `list' specifies the list object.
   The return value is the new list object equivalent to the specified object. */
TCLIST *tclistdup(const TCLIST *list);


/* Delete a list object.
   `list' specifies the list object.
   Note that the deleted object and its derivatives can not be used anymore. */
void tclistdel(TCLIST *list);


/* Get the number of elements of a list object.
   `list' specifies the list object.
   The return value is the number of elements of the list. */
int tclistnum(const TCLIST *list);


/* Get the pointer to the region of an element of a list object.
   `list' specifies the list object.
   `index' specifies the index of the element.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the value.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  If `index' is equal to or more than
   the number of elements, the return value is `NULL'. */
const void *tclistval(const TCLIST *list, int index, int *sp);


/* Get the string of an element of a list object.
   `list' specifies the list object.
   `index' specifies the index of the element.
   The return value is the string of the value.
   If `index' is equal to or more than the number of elements, the return value is `NULL'. */
const char *tclistval2(const TCLIST *list, int index);


/* Add an element at the end of a list object.
   `list' specifies the list object.
   `ptr' specifies the pointer to the region of the new element.
   `size' specifies the size of the region. */
void tclistpush(TCLIST *list, const void *ptr, int size);


/* Add a string element at the end of a list object.
   `list' specifies the list object.
   `str' specifies the string of the new element. */
void tclistpush2(TCLIST *list, const char *str);


/* Remove an element of the end of a list object.
   `list' specifies the list object.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the removed element.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when it
   is no longer in use.  If the list is empty, the return value is `NULL'. */
void *tclistpop(TCLIST *list, int *sp);


/* Remove a string element of the end of a list object.
   `list' specifies the list object.
   The return value is the string of the removed element.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use.  If the list is empty, the return
   value is `NULL'. */
char *tclistpop2(TCLIST *list);


/* Add an element at the top of a list object.
   `list' specifies the list object.
   `ptr' specifies the pointer to the region of the new element.
   `size' specifies the size of the region. */
void tclistunshift(TCLIST *list, const void *ptr, int size);


/* Add a string element at the top of a list object.
   `list' specifies the list object.
   `str' specifies the string of the new element. */
void tclistunshift2(TCLIST *list, const char *str);


/* Remove an element of the top of a list object.
   `list' specifies the list object.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the removed element.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when it
   is no longer in use.  If the list is empty, the return value is `NULL'. */
void *tclistshift(TCLIST *list, int *sp);


/* Remove a string element of the top of a list object.
   `list' specifies the list object.
   The return value is the string of the removed element.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use.  If the list is empty, the return
   value is `NULL'. */
char *tclistshift2(TCLIST *list);


/* Add an element at the specified location of a list object.
   `list' specifies the list object.
   `index' specifies the index of the new element.
   `ptr' specifies the pointer to the region of the new element.
   `size' specifies the size of the region.
   If `index' is equal to or more than the number of elements, this function has no effect. */
void tclistinsert(TCLIST *list, int index, const void *ptr, int size);


/* Add a string element at the specified location of a list object.
   `list' specifies the list object.
   `index' specifies the index of the new element.
   `str' specifies the string of the new element.
   If `index' is equal to or more than the number of elements, this function has no effect. */
void tclistinsert2(TCLIST *list, int index, const char *str);


/* Remove an element at the specified location of a list object.
   `list' specifies the list object.
   `index' specifies the index of the element to be removed.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the removed element.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when it
   is no longer in use.  If `index' is equal to or more than the number of elements, no element
   is removed and the return value is `NULL'. */
void *tclistremove(TCLIST *list, int index, int *sp);


/* Remove a string element at the specified location of a list object.
   `list' specifies the list object.
   `index' specifies the index of the element to be removed.
   The return value is the string of the removed element.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use.  If `index' is equal to or more
   than the number of elements, no element is removed and the return value is `NULL'. */
char *tclistremove2(TCLIST *list, int index);


/* Overwrite an element at the specified location of a list object.
   `list' specifies the list object.
   `index' specifies the index of the element to be overwritten.
   `ptr' specifies the pointer to the region of the new content.
   `size' specifies the size of the new content.
   If `index' is equal to or more than the number of elements, this function has no effect. */
void tclistover(TCLIST *list, int index, const void *ptr, int size);


/* Overwrite a string element at the specified location of a list object.
   `list' specifies the list object.
   `index' specifies the index of the element to be overwritten.
   `str' specifies the string of the new content.
   If `index' is equal to or more than the number of elements, this function has no effect. */
void tclistover2(TCLIST *list, int index, const char *str);


/* Sort elements of a list object in lexical order.
   `list' specifies the list object. */
void tclistsort(TCLIST *list);


/* Search a list object for an element using liner search.
   `list' specifies the list object.
   `ptr' specifies the pointer to the region of the key.
   `size' specifies the size of the region.
   The return value is the index of a corresponding element or -1 if there is no corresponding
   element.
   If two or more elements correspond, the former returns. */
int tclistlsearch(const TCLIST *list, const void *ptr, int size);


/* Search a list object for an element using binary search.
   `list' specifies the list object.  It should be sorted in lexical order.
   `ptr' specifies the pointer to the region of the key.
   `size' specifies the size of the region.
   The return value is the index of a corresponding element or -1 if there is no corresponding
   element.
   If two or more elements correspond, which returns is not defined. */
int tclistbsearch(const TCLIST *list, const void *ptr, int size);


/* Clear a list object.
   `list' specifies the list object.
   All elements are removed. */
void tclistclear(TCLIST *list);


/* Serialize a list object into a byte array.
   `list' specifies the list object.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the result serial region.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
void *tclistdump(const TCLIST *list, int *sp);


/* Create a list object from a serialized byte array.
   `ptr' specifies the pointer to the region of serialized byte array.
   `size' specifies the size of the region.
   The return value is a new list object.
   Because the object of the return value is created with the function `tclistnew', it should
   be deleted with the function `tclistdel' when it is no longer in use. */
TCLIST *tclistload(const void *ptr, int size);



/*************************************************************************************************
 * array list (for experts)
 *************************************************************************************************/


/* Add an allocated element at the end of a list object.
   `list' specifies the list object.
   `ptr' specifies the pointer to the region allocated with `malloc' call.
   `size' specifies the size of the region.
   Note that the specified region is released when the object is deleted. */
void tclistpushmalloc(TCLIST *list, void *ptr, int size);


/* Sort elements of a list object in case-insensitive lexical order.
   `list' specifies the list object. */
void tclistsortci(TCLIST *list);


/* Sort elements of a list object by an arbitrary comparison function.
   `list' specifies the list object.
   `cmp' specifies the pointer to the comparison function.  The structure TCLISTDATUM has the
   member "ptr" which is the pointer to the region of the element, and the member "size" which is
   the size of the region. */
void tclistsortex(TCLIST *list, int (*cmp)(const TCLISTDATUM *, const TCLISTDATUM *));


/* Invert elements of a list object.
   `list' specifies the list object. */
void tclistinvert(TCLIST *list);


/* Perform formatted output into a list object.
   `list' specifies the list object.
   `format' specifies the printf-like format string.  The conversion character `%' can be used
   with such flag characters as `s', `d', `o', `u', `x', `X', `c', `e', `E', `f', `g', `G', `@',
   `?', `b', and `%'.  `@' works as with `s' but escapes meta characters of XML.  `?' works as
   with `s' but escapes meta characters of URL.  `b' converts an integer to the string as binary
   numbers.  The other conversion character work as with each original.
   The other arguments are used according to the format string. */
void tclistprintf(TCLIST *list, const char *format, ...);



/*************************************************************************************************
 * hash map
 *************************************************************************************************/


typedef struct _TCMAPREC {               /* type of structure for an element of a map */
  int32_t ksiz;                          /* size of the region of the key */
  int32_t vsiz;                          /* size of the region of the value */
  struct _TCMAPREC *left;                /* pointer to the left child */
  struct _TCMAPREC *right;               /* pointer to the right child */
  struct _TCMAPREC *prev;                /* pointer to the previous element */
  struct _TCMAPREC *next;                /* pointer to the next element */
} TCMAPREC;

typedef struct {                         /* type of structure for a map */
  TCMAPREC **buckets;                    /* bucket array */
  TCMAPREC *first;                       /* pointer to the first element */
  TCMAPREC *last;                        /* pointer to the last element */
  TCMAPREC *cur;                         /* pointer to the current element */
  uint32_t bnum;                         /* number of buckets */
  uint64_t rnum;                         /* number of records */
  uint64_t msiz;                         /* total size of records */
} TCMAP;


/* Create a map object.
   The return value is the new map object. */
TCMAP *tcmapnew(void);


/* Create a map object with specifying the number of the buckets.
   `bnum' specifies the number of the buckets.
   The return value is the new map object. */
TCMAP *tcmapnew2(uint32_t bnum);


/* Create a map object with initial string elements.
   `str' specifies the string of the first element.
   The other arguments are other elements.  They should be trailed by a `NULL' argument.
   The return value is the new map object.
   The key and the value of each record are situated one after the other. */
TCMAP *tcmapnew3(const char *str, ...);


/* Copy a map object.
   `map' specifies the map object.
   The return value is the new map object equivalent to the specified object. */
TCMAP *tcmapdup(const TCMAP *map);


/* Delete a map object.
   `map' specifies the map object.
   Note that the deleted object and its derivatives can not be used anymore. */
void tcmapdel(TCMAP *map);


/* Store a record into a map object.
   `map' specifies the map object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If a record with the same key exists in the map, it is overwritten. */
void tcmapput(TCMAP *map, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Store a string record into a map object.
   `map' specifies the map object.
   `kstr' specifies the string of the key.
   `vstr' specifies the string of the value.
   If a record with the same key exists in the map, it is overwritten. */
void tcmapput2(TCMAP *map, const char *kstr, const char *vstr);


/* Store a new record into a map object.
   `map' specifies the map object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If successful, the return value is true, else, it is false.
   If a record with the same key exists in the map, this function has no effect. */
bool tcmapputkeep(TCMAP *map, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Store a new string record into a map object.
   `map' specifies the map object.
   `kstr' specifies the string of the key.
   `vstr' specifies the string of the value.
   If successful, the return value is true, else, it is false.
   If a record with the same key exists in the map, this function has no effect. */
bool tcmapputkeep2(TCMAP *map, const char *kstr, const char *vstr);


/* Concatenate a value at the end of the value of the existing record in a map object.
   `map' specifies the map object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If there is no corresponding record, a new record is created. */
void tcmapputcat(TCMAP *map, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Concatenate a string value at the end of the value of the existing record in a map object.
   `map' specifies the map object.
   `kstr' specifies the string of the key.
   `vstr' specifies the string of the value.
   If there is no corresponding record, a new record is created. */
void tcmapputcat2(TCMAP *map, const char *kstr, const char *vstr);


/* Remove a record of a map object.
   `map' specifies the map object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   If successful, the return value is true.  False is returned when no record corresponds to
   the specified key. */
bool tcmapout(TCMAP *map, const void *kbuf, int ksiz);


/* Remove a string record of a map object.
   `map' specifies the map object.
   `kstr' specifies the string of the key.
   If successful, the return value is true.  False is returned when no record corresponds to
   the specified key. */
bool tcmapout2(TCMAP *map, const char *kstr);


/* Retrieve a record in a map object.
   `map' specifies the map object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the region of the value of the
   corresponding record.  `NULL' is returned when no record corresponds.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string. */
const void *tcmapget(const TCMAP *map, const void *kbuf, int ksiz, int *sp);


/* Retrieve a string record in a map object.
   `map' specifies the map object.
   `kstr' specifies the string of the key.
   If successful, the return value is the string of the value of the corresponding record.
   `NULL' is returned when no record corresponds. */
const char *tcmapget2(const TCMAP *map, const char *kstr);


/* Move a record to the edge of a map object.
   `map' specifies the map object.
   `kbuf' specifies the pointer to the region of a key.
   `ksiz' specifies the size of the region of the key.
   `head' specifies the destination which is the head if it is true or the tail if else.
   If successful, the return value is true.  False is returned when no record corresponds to
   the specified key. */
bool tcmapmove(TCMAP *map, const void *kbuf, int ksiz, bool head);


/* Move a string record to the edge of a map object.
   `map' specifies the map object.
   `kstr' specifies the string of a key.
   `head' specifies the destination which is the head if it is true or the tail if else.
   If successful, the return value is true.  False is returned when no record corresponds to
   the specified key. */
bool tcmapmove2(TCMAP *map, const char *kstr, bool head);


/* Initialize the iterator of a map object.
   `map' specifies the map object.
   The iterator is used in order to access the key of every record stored in the map object. */
void tcmapiterinit(TCMAP *map);


/* Get the next key of the iterator of a map object.
   `map' specifies the map object.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the region of the next key, else, it is
   `NULL'.  `NULL' is returned when no record can be fetched from the iterator.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.
   The order of iteration is assured to be the same as the stored order. */
const void *tcmapiternext(TCMAP *map, int *sp);


/* Get the next key string of the iterator of a map object.
   `map' specifies the map object.
   If successful, the return value is the pointer to the region of the next key, else, it is
   `NULL'.  `NULL' is returned when no record can be fetched from the iterator.
   The order of iteration is assured to be the same as the stored order. */
const char *tcmapiternext2(TCMAP *map);


/* Get the number of records stored in a map object.
   `map' specifies the map object.
   The return value is the number of the records stored in the map object. */
uint64_t tcmaprnum(const TCMAP *map);


/* Get the total size of memory used in a map object.
   `map' specifies the map object.
   The return value is the total size of memory used in a map object. */
uint64_t tcmapmsiz(const TCMAP *map);


/* Create a list object containing all keys in a map object.
   `map' specifies the map object.
   The return value is the new list object containing all keys in the map object.
   Because the object of the return value is created with the function `tclistnew', it should
   be deleted with the function `tclistdel' when it is no longer in use. */
TCLIST *tcmapkeys(const TCMAP *map);


/* Create a list object containing all values in a map object.
   `map' specifies the map object.
   The return value is the new list object containing all values in the map object.
   Because the object of the return value is created with the function `tclistnew', it should
   be deleted with the function `tclistdel' when it is no longer in use. */
TCLIST *tcmapvals(const TCMAP *map);


/* Add an integer to a record in a map object.
   `map' specifies the map object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `num' specifies the additional value.
   The return value is the summation value.
   If the corresponding record exists, the value is treated as an integer and is added to.  If no
   record corresponds, a new record of the additional value is stored. */
int tcmapaddint(TCMAP *map, const void *kbuf, int ksiz, int num);


/* Add a real number to a record in a map object.
   `map' specifies the map object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `num' specifies the additional value.
   The return value is the summation value.
   If the corresponding record exists, the value is treated as a real number and is added to.  If
   no record corresponds, a new record of the additional value is stored. */
double tcmapadddouble(TCMAP *map, const void *kbuf, int ksiz, double num);


/* Clear a map object.
   `map' specifies the map object.
   All records are removed. */
void tcmapclear(TCMAP *map);


/* Remove front records of a map object.
   `map' specifies the map object.
   `num' specifies the number of records to be removed. */
void tcmapcutfront(TCMAP *map, int num);


/* Serialize a map object into a byte array.
   `map' specifies the map object.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the result serial region.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
void *tcmapdump(const TCMAP *map, int *sp);


/* Create a map object from a serialized byte array.
   `ptr' specifies the pointer to the region of serialized byte array.
   `size' specifies the size of the region.
   The return value is a new map object.
   Because the object of the return value is created with the function `tcmapnew', it should be
   deleted with the function `tcmapdel' when it is no longer in use. */
TCMAP *tcmapload(const void *ptr, int size);



/*************************************************************************************************
 * hash map (for experts)
 *************************************************************************************************/


/* Store a record and make it semivolatile in a map object.
   `map' specifies the map object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If a record with the same key exists in the map, it is overwritten.  The record is moved to
   the tail. */
void tcmapput3(TCMAP *map, const void *kbuf, int ksiz, const char *vbuf, int vsiz);


/* Store a record of the value of two regions into a map object.
   `map' specifies the map object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `fvbuf' specifies the pointer to the former region of the value.
   `fvsiz' specifies the size of the former region of the value.
   `lvbuf' specifies the pointer to the latter region of the value.
   `lvsiz' specifies the size of the latter region of the value.
   If a record with the same key exists in the map, it is overwritten. */
void tcmapput4(TCMAP *map, const void *kbuf, int ksiz,
               const void *fvbuf, int fvsiz, const void *lvbuf, int lvsiz);


/* Concatenate a value at the existing record and make it semivolatile in a map object.
   `map' specifies the map object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If there is no corresponding record, a new record is created. */
void tcmapputcat3(TCMAP *map, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Store a record into a map object with a duplication handler.
   `map' specifies the map object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.  `NULL' means that record addition is
   ommited if there is no corresponding record.
   `vsiz' specifies the size of the region of the value.
   `proc' specifies the pointer to the callback function to process duplication.  It receives
   four parameters.  The first parameter is the pointer to the region of the value.  The second
   parameter is the size of the region of the value.  The third parameter is the pointer to the
   variable into which the size of the region of the return value is assigned.  The fourth
   parameter is the pointer to the optional opaque object.  It returns the pointer to the result
   object allocated with `malloc'.  It is released by the caller.  If it is `NULL', the record is
   not modified.  If it is `(void *)-1', the record is removed.
   `op' specifies an arbitrary pointer to be given as a parameter of the callback function.  If
   it is not needed, `NULL' can be specified.
   If successful, the return value is true, else, it is false. */
bool tcmapputproc(TCMAP *map, const void *kbuf, int ksiz, const void *vbuf, int vsiz,
                  TCPDPROC proc, void *op);


/* Retrieve a semivolatile record in a map object.
   `map' specifies the map object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the region of the value of the
   corresponding record.  `NULL' is returned when no record corresponds.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  The internal region of the returned
   record is moved to the tail so that the record will survive for a time under LRU cache
   algorithm removing records from the head. */
const void *tcmapget3(TCMAP *map, const void *kbuf, int ksiz, int *sp);


/* Retrieve a string record in a map object with specifying the default value string.
   `map' specifies the map object.
   `kstr' specifies the string of the key.
   `dstr' specifies the string of the default value.
   The return value is the string of the value of the corresponding record or the default value
   string. */
const char *tcmapget4(TCMAP *map, const char *kstr, const char *dstr);


/* Initialize the iterator of a map object at the record corresponding a key.
   `map' specifies the map object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   If there is no record corresponding the condition, the iterator is not modified. */
void tcmapiterinit2(TCMAP *map, const void *kbuf, int ksiz);


/* Initialize the iterator of a map object at the record corresponding a key string.
   `map' specifies the map object.
   `kstr' specifies the string of the key.
   If there is no record corresponding the condition, the iterator is not modified. */
void tcmapiterinit3(TCMAP *map, const char *kstr);


/* Get the value bound to the key fetched from the iterator of a map object.
   `kbuf' specifies the pointer to the region of the iteration key.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the value of the corresponding record.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string. */
const void *tcmapiterval(const void *kbuf, int *sp);


/* Get the value string bound to the key fetched from the iterator of a map object.
   `kstr' specifies the string of the iteration key.
   The return value is the pointer to the region of the value of the corresponding record. */
const char *tcmapiterval2(const char *kstr);


/* Create an array of strings of all keys in a map object.
   `map' specifies the map object.
   `np' specifies the pointer to a variable into which the number of elements of the return value
   is assigned.
   The return value is the pointer to the array of all string keys in the map object.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call if when is no longer in use.  Note that elements of the array
   point to the inner objects, whose life duration is synchronous with the map object. */
const char **tcmapkeys2(const TCMAP *map, int *np);


/* Create an array of strings of all values in a map object.
   `map' specifies the map object.
   `np' specifies the pointer to a variable into which the number of elements of the return value
   is assigned.
   The return value is the pointer to the array of all string values in the map object.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call if when is no longer in use.  Note that elements of the array
   point to the inner objects, whose life duration is synchronous with the map object. */
const char **tcmapvals2(const TCMAP *map, int *np);


/* Extract a map record from a serialized byte array.
   `ptr' specifies the pointer to the region of serialized byte array.
   `size' specifies the size of the region.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the region of the value of the
   corresponding record.  `NULL' is returned when no record corresponds.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string. */
void *tcmaploadone(const void *ptr, int size, const void *kbuf, int ksiz, int *sp);


/* Perform formatted output into a map object.
   `map' specifies the map object.
   `kstr' specifies the string of the key.
   `format' specifies the printf-like format string.  The conversion character `%' can be used
   with such flag characters as `s', `d', `o', `u', `x', `X', `c', `e', `E', `f', `g', `G', `@',
   `?', `b', and `%'.  `@' works as with `s' but escapes meta characters of XML.  `?' works as
   with `s' but escapes meta characters of URL.  `b' converts an integer to the string as binary
   numbers.  The other conversion character work as with each original.
   The other arguments are used according to the format string. */
void tcmapprintf(TCMAP *map, const char *kstr, const char *format, ...);



/*************************************************************************************************
 * ordered tree
 *************************************************************************************************/


typedef struct _TCTREEREC {              /* type of structure for an element of a tree */
  int32_t ksiz;                          /* size of the region of the key */
  int32_t vsiz;                          /* size of the region of the value */
  struct _TCTREEREC *left;               /* pointer to the left child */
  struct _TCTREEREC *right;              /* pointer to the right child */
} TCTREEREC;

typedef struct {                         /* type of structure for a tree */
  TCTREEREC *root;                       /* pointer to the root element */
  TCTREEREC *cur;                        /* pointer to the current element */
  uint64_t rnum;                         /* number of records */
  uint64_t msiz;                         /* total size of records */
  TCCMP cmp;                             /* pointer to the comparison function */
  void *cmpop;                           /* opaque object for the comparison function */
} TCTREE;


/* Create a tree object.
   The return value is the new tree object. */
TCTREE *tctreenew(void);


/* Create a tree object with specifying the custom comparison function.
   `cmp' specifies the pointer to the custom comparison function.  It receives five parameters.
   The first parameter is the pointer to the region of one key.  The second parameter is the size
   of the region of one key.  The third parameter is the pointer to the region of the other key.
   The fourth parameter is the size of the region of the other key.  The fifth parameter is the
   pointer to the optional opaque object.  It returns positive if the former is big, negative if
   the latter is big, 0 if both are equivalent.
   `cmpop' specifies an arbitrary pointer to be given as a parameter of the comparison function.
   If it is not needed, `NULL' can be specified.
   The return value is the new tree object.
   The default comparison function compares keys of two records by lexical order.  The functions
   `tccmplexical' (dafault), `tccmpdecimal', `tccmpint32', and `tccmpint64' are built-in. */
TCTREE *tctreenew2(TCCMP cmp, void *cmpop);


/* Copy a tree object.
   `tree' specifies the tree object.
   The return value is the new tree object equivalent to the specified object. */
TCTREE *tctreedup(const TCTREE *tree);


/* Delete a tree object.
   `tree' specifies the tree object.
   Note that the deleted object and its derivatives can not be used anymore. */
void tctreedel(TCTREE *tree);


/* Store a record into a tree object.
   `tree' specifies the tree object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If a record with the same key exists in the tree, it is overwritten. */
void tctreeput(TCTREE *tree, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Store a string record into a tree object.
   `tree' specifies the tree object.
   `kstr' specifies the string of the key.
   `vstr' specifies the string of the value.
   If a record with the same key exists in the tree, it is overwritten. */
void tctreeput2(TCTREE *tree, const char *kstr, const char *vstr);


/* Store a new record into a tree object.
   `tree' specifies the tree object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If successful, the return value is true, else, it is false.
   If a record with the same key exists in the tree, this function has no effect. */
bool tctreeputkeep(TCTREE *tree, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Store a new string record into a tree object.
   `tree' specifies the tree object.
   `kstr' specifies the string of the key.
   `vstr' specifies the string of the value.
   If successful, the return value is true, else, it is false.
   If a record with the same key exists in the tree, this function has no effect. */
bool tctreeputkeep2(TCTREE *tree, const char *kstr, const char *vstr);


/* Concatenate a value at the end of the value of the existing record in a tree object.
   `tree' specifies the tree object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If there is no corresponding record, a new record is created. */
void tctreeputcat(TCTREE *tree, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Concatenate a string value at the end of the value of the existing record in a tree object.
   `tree' specifies the tree object.
   `kstr' specifies the string of the key.
   `vstr' specifies the string of the value.
   If there is no corresponding record, a new record is created. */
void tctreeputcat2(TCTREE *tree, const char *kstr, const char *vstr);


/* Store a record into a tree object with a duplication handler.
   `tree' specifies the tree object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.  `NULL' means that record addition is
   ommited if there is no corresponding record.
   `vsiz' specifies the size of the region of the value.
   `proc' specifies the pointer to the callback function to process duplication.  It receives
   four parameters.  The first parameter is the pointer to the region of the value.  The second
   parameter is the size of the region of the value.  The third parameter is the pointer to the
   variable into which the size of the region of the return value is assigned.  The fourth
   parameter is the pointer to the optional opaque object.  It returns the pointer to the result
   object allocated with `malloc'.  It is released by the caller.  If it is `NULL', the record is
   not modified.  If it is `(void *)-1', the record is removed.
   `op' specifies an arbitrary pointer to be given as a parameter of the callback function.  If
   it is not needed, `NULL' can be specified.
   If successful, the return value is true, else, it is false. */
bool tctreeputproc(TCTREE *tree, const void *kbuf, int ksiz, const void *vbuf, int vsiz,
                   TCPDPROC proc, void *op);


/* Remove a record of a tree object.
   `tree' specifies the tree object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   If successful, the return value is true.  False is returned when no record corresponds to
   the specified key. */
bool tctreeout(TCTREE *tree, const void *kbuf, int ksiz);


/* Remove a string record of a tree object.
   `tree' specifies the tree object.
   `kstr' specifies the string of the key.
   If successful, the return value is true.  False is returned when no record corresponds to
   the specified key. */
bool tctreeout2(TCTREE *tree, const char *kstr);


/* Retrieve a record in a tree object.
   `tree' specifies the tree object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the region of the value of the
   corresponding record.  `NULL' is returned when no record corresponds.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string. */
const void *tctreeget(TCTREE *tree, const void *kbuf, int ksiz, int *sp);


/* Retrieve a string record in a tree object.
   `tree' specifies the tree object.
   `kstr' specifies the string of the key.
   If successful, the return value is the string of the value of the corresponding record.
   `NULL' is returned when no record corresponds. */
const char *tctreeget2(TCTREE *tree, const char *kstr);


/* Initialize the iterator of a tree object.
   `tree' specifies the tree object.
   The iterator is used in order to access the key of every record stored in the tree object. */
void tctreeiterinit(TCTREE *tree);


/* Get the next key of the iterator of a tree object.
   `tree' specifies the tree object.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the region of the next key, else, it is
   `NULL'.  `NULL' is returned when no record can be fetched from the iterator.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.
   The order of iteration is assured to be ascending of the keys. */
const void *tctreeiternext(TCTREE *tree, int *sp);


/* Get the next key string of the iterator of a tree object.
   `tree' specifies the tree object.
   If successful, the return value is the pointer to the region of the next key, else, it is
   `NULL'.  `NULL' is returned when no record can be fetched from the iterator.
   The order of iteration is assured to be ascending of the keys. */
const char *tctreeiternext2(TCTREE *tree);


/* Get the number of records stored in a tree object.
   `tree' specifies the tree object.
   The return value is the number of the records stored in the tree object. */
uint64_t tctreernum(const TCTREE *tree);


/* Get the total size of memory used in a tree object.
   `tree' specifies the tree object.
   The return value is the total size of memory used in a tree object. */
uint64_t tctreemsiz(const TCTREE *tree);


/* Create a list object containing all keys in a tree object.
   `tree' specifies the tree object.
   The return value is the new list object containing all keys in the tree object.
   Because the object of the return value is created with the function `tclistnew', it should
   be deleted with the function `tclistdel' when it is no longer in use. */
TCLIST *tctreekeys(const TCTREE *tree);


/* Create a list object containing all values in a tree object.
   `tree' specifies the tree object.
   The return value is the new list object containing all values in the tree object.
   Because the object of the return value is created with the function `tclistnew', it should
   be deleted with the function `tclistdel' when it is no longer in use. */
TCLIST *tctreevals(const TCTREE *tree);


/* Add an integer to a record in a tree object.
   `tree' specifies the tree object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `num' specifies the additional value.
   The return value is the summation value.
   If the corresponding record exists, the value is treated as an integer and is added to.  If no
   record corresponds, a new record of the additional value is stored. */
int tctreeaddint(TCTREE *tree, const void *kbuf, int ksiz, int num);


/* Add a real number to a record in a tree object.
   `tree' specifies the tree object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `num' specifies the additional value.
   The return value is the summation value.
   If the corresponding record exists, the value is treated as a real number and is added to.  If
   no record corresponds, a new record of the additional value is stored. */
double tctreeadddouble(TCTREE *tree, const void *kbuf, int ksiz, double num);


/* Clear a tree object.
   `tree' specifies the tree object.
   All records are removed. */
void tctreeclear(TCTREE *tree);


/* Remove fringe records of a tree object.
   `tree' specifies the tree object.
   `num' specifies the number of records to be removed. */
void tctreecutfringe(TCTREE *tree, int num);


/* Serialize a tree object into a byte array.
   `tree' specifies the tree object.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the result serial region.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
void *tctreedump(const TCTREE *tree, int *sp);


/* Create a tree object from a serialized byte array.
   `ptr' specifies the pointer to the region of serialized byte array.
   `size' specifies the size of the region.
   `cmp' specifies the pointer to the custom comparison function.
   `cmpop' specifies an arbitrary pointer to be given as a parameter of the comparison function.
   If it is not needed, `NULL' can be specified.
   The return value is a new tree object.
   Because the object of the return value is created with the function `tctreenew', it should be
   deleted with the function `tctreedel' when it is no longer in use. */
TCTREE *tctreeload(const void *ptr, int size, TCCMP cmp, void *cmpop);



/*************************************************************************************************
 * ordered tree (for experts)
 *************************************************************************************************/


/* Store a record into a tree object without balancing nodes.
   `tree' specifies the tree object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If a record with the same key exists in the tree, it is overwritten.  The structure of the
   tree is not modifed by this function. */
void tctreeput3(TCTREE *tree, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Store a new record into a tree object without balancing nodes.
   `tree' specifies the tree object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If successful, the return value is true, else, it is false.
   If a record with the same key exists in the tree, this function has no effect.  The structure
   of the tree is not modifed by this function. */
bool tctreeputkeep3(TCTREE *tree, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Concatenate a value at the existing record in a tree object without balancing nodes.
   `tree' specifies the tree object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If there is no corresponding record, a new record is created.  The structure of the tree is
   not modifed by this function. */
void tctreeputcat3(TCTREE *tree, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Retrieve a record in a tree object without balancing nodes.
   `tree' specifies the tree object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the region of the value of the
   corresponding record.  `NULL' is returned when no record corresponds.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  The structure of the tree is not
   modifed by this function. */
const void *tctreeget3(const TCTREE *tree, const void *kbuf, int ksiz, int *sp);


/* Retrieve a string record in a tree object with specifying the default value string.
   `tree' specifies the tree object.
   `kstr' specifies the string of the key.
   `dstr' specifies the string of the default value.
   The return value is the string of the value of the corresponding record or the default value
   string. */
const char *tctreeget4(TCTREE *tree, const char *kstr, const char *dstr);


/* Initialize the iterator of a tree object in front of records corresponding a key.
   `tree' specifies the tree object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   The iterator is set to the first record corresponding the key or the next substitute if
   completely matching record does not exist. */
void tctreeiterinit2(TCTREE *tree, const void *kbuf, int ksiz);


/* Initialize the iterator of a tree object in front of records corresponding a key string.
   `tree' specifies the tree object.
   `kstr' specifies the string of the key.
   The iterator is set to the first record corresponding the key or the next substitute if
   completely matching record does not exist. */
void tctreeiterinit3(TCTREE *tree, const char *kstr);


/* Get the value bound to the key fetched from the iterator of a tree object.
   `kbuf' specifies the pointer to the region of the iteration key.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the value of the corresponding record.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string. */
const void *tctreeiterval(const void *kbuf, int *sp);


/* Get the value string bound to the key fetched from the iterator of a tree object.
   `kstr' specifies the string of the iteration key.
   The return value is the pointer to the region of the value of the corresponding record. */
const char *tctreeiterval2(const char *kstr);


/* Create an array of strings of all keys in a tree object.
   `tree' specifies the tree object.
   `np' specifies the pointer to a variable into which the number of elements of the return value
   is assigned.
   The return value is the pointer to the array of all string keys in the tree object.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call if when is no longer in use.  Note that elements of the array
   point to the inner objects, whose life duration is synchronous with the tree object. */
const char **tctreekeys2(const TCTREE *tree, int *np);


/* Create an array of strings of all values in a tree object.
   `tree' specifies the tree object.
   `np' specifies the pointer to a variable into which the number of elements of the return value
   is assigned.
   The return value is the pointer to the array of all string values in the tree object.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call if when is no longer in use.  Note that elements of the array
   point to the inner objects, whose life duration is synchronous with the tree object. */
const char **tctreevals2(const TCTREE *tree, int *np);


/* Extract a tree record from a serialized byte array.
   `ptr' specifies the pointer to the region of serialized byte array.
   `size' specifies the size of the region.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the region of the value of the
   corresponding record.  `NULL' is returned when no record corresponds.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string. */
void *tctreeloadone(const void *ptr, int size, const void *kbuf, int ksiz, int *sp);


/* Perform formatted output into a tree object.
   `map' specifies the tree object.
   `kstr' specifies the string of the key.
   `format' specifies the printf-like format string.  The conversion character `%' can be used
   with such flag characters as `s', `d', `o', `u', `x', `X', `c', `e', `E', `f', `g', `G', `@',
   `?', `b', and `%'.  `@' works as with `s' but escapes meta characters of XML.  `?' works as
   with `s' but escapes meta characters of URL.  `b' converts an integer to the string as binary
   numbers.  The other conversion character work as with each original.
   The other arguments are used according to the format string. */
void tctreeprintf(TCTREE *tree, const char *kstr, const char *format, ...);



/*************************************************************************************************
 * on-memory hash database
 *************************************************************************************************/


typedef struct {                         /* type of structure for a on-memory hash database */
  void **mmtxs;                          /* mutexes for method */
  void *imtx;                            /* mutex for iterator */
  TCMAP **maps;                          /* internal map objects */
  int iter;                              /* index of maps for the iterator */
} TCMDB;


/* Create an on-memory hash database object.
   The return value is the new on-memory hash database object.
   The object can be shared by plural threads because of the internal mutex. */
TCMDB *tcmdbnew(void);


/* Create an on-memory hash database object with specifying the number of the buckets.
   `bnum' specifies the number of the buckets.
   The return value is the new on-memory hash database object.
   The object can be shared by plural threads because of the internal mutex. */
TCMDB *tcmdbnew2(uint32_t bnum);


/* Delete an on-memory hash database object.
   `mdb' specifies the on-memory hash database object. */
void tcmdbdel(TCMDB *mdb);


/* Store a record into an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If a record with the same key exists in the database, it is overwritten. */
void tcmdbput(TCMDB *mdb, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Store a string record into an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `kstr' specifies the string of the key.
   `vstr' specifies the string of the value.
   If a record with the same key exists in the database, it is overwritten. */
void tcmdbput2(TCMDB *mdb, const char *kstr, const char *vstr);


/* Store a new record into an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If successful, the return value is true, else, it is false.
   If a record with the same key exists in the database, this function has no effect. */
bool tcmdbputkeep(TCMDB *mdb, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Store a new string record into an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `kstr' specifies the string of the key.
   `vstr' specifies the string of the value.
   If successful, the return value is true, else, it is false.
   If a record with the same key exists in the database, this function has no effect. */
bool tcmdbputkeep2(TCMDB *mdb, const char *kstr, const char *vstr);


/* Concatenate a value at the end of the existing record in an on-memory hash database.
   `mdb' specifies the on-memory hash database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If there is no corresponding record, a new record is created. */
void tcmdbputcat(TCMDB *mdb, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Concatenate a string at the end of the existing record in an on-memory hash database.
   `mdb' specifies the on-memory hash database object.
   `kstr' specifies the string of the key.
   `vstr' specifies the string of the value.
   If there is no corresponding record, a new record is created. */
void tcmdbputcat2(TCMDB *mdb, const char *kstr, const char *vstr);


/* Remove a record of an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   If successful, the return value is true.  False is returned when no record corresponds to
   the specified key. */
bool tcmdbout(TCMDB *mdb, const void *kbuf, int ksiz);


/* Remove a string record of an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `kstr' specifies the string of the key.
   If successful, the return value is true.  False is returned when no record corresponds to
   the specified key. */
bool tcmdbout2(TCMDB *mdb, const char *kstr);


/* Retrieve a record in an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the region of the value of the
   corresponding record.  `NULL' is returned when no record corresponds.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when
   it is no longer in use. */
void *tcmdbget(TCMDB *mdb, const void *kbuf, int ksiz, int *sp);


/* Retrieve a string record in an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `kstr' specifies the string of the key.
   If successful, the return value is the string of the value of the corresponding record.
   `NULL' is returned when no record corresponds.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tcmdbget2(TCMDB *mdb, const char *kstr);


/* Get the size of the value of a record in an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   If successful, the return value is the size of the value of the corresponding record, else,
   it is -1. */
int tcmdbvsiz(TCMDB *mdb, const void *kbuf, int ksiz);


/* Get the size of the value of a string record in an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `kstr' specifies the string of the key.
   If successful, the return value is the size of the value of the corresponding record, else,
   it is -1. */
int tcmdbvsiz2(TCMDB *mdb, const char *kstr);


/* Initialize the iterator of an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   The iterator is used in order to access the key of every record stored in the on-memory
   database. */
void tcmdbiterinit(TCMDB *mdb);


/* Get the next key of the iterator of an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the region of the next key, else, it is
   `NULL'.  `NULL' is returned when no record can be fetched from the iterator.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when
   it is no longer in use.  The order of iteration is assured to be the same as the stored
   order. */
void *tcmdbiternext(TCMDB *mdb, int *sp);


/* Get the next key string of the iterator of an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   If successful, the return value is the pointer to the region of the next key, else, it is
   `NULL'.  `NULL' is returned when no record can be fetched from the iterator.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use.  The order of iteration is assured
   to be the same as the stored order. */
char *tcmdbiternext2(TCMDB *mdb);


/* Get forward matching keys in an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `pbuf' specifies the pointer to the region of the prefix.
   `psiz' specifies the size of the region of the prefix.
   `max' specifies the maximum number of keys to be fetched.  If it is negative, no limit is
   specified.
   The return value is a list object of the corresponding keys.  This function does never fail.
   It returns an empty list even if no key corresponds.
   Because the object of the return value is created with the function `tclistnew', it should be
   deleted with the function `tclistdel' when it is no longer in use.  Note that this function
   may be very slow because every key in the database is scanned. */
TCLIST *tcmdbfwmkeys(TCMDB *mdb, const void *pbuf, int psiz, int max);


/* Get forward matching string keys in an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `pstr' specifies the string of the prefix.
   `max' specifies the maximum number of keys to be fetched.  If it is negative, no limit is
   specified.
   The return value is a list object of the corresponding keys.  This function does never fail.
   It returns an empty list even if no key corresponds.
   Because the object of the return value is created with the function `tclistnew', it should be
   deleted with the function `tclistdel' when it is no longer in use.  Note that this function
   may be very slow because every key in the database is scanned. */
TCLIST *tcmdbfwmkeys2(TCMDB *mdb, const char *pstr, int max);


/* Get the number of records stored in an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   The return value is the number of the records stored in the database. */
uint64_t tcmdbrnum(TCMDB *mdb);


/* Get the total size of memory used in an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   The return value is the total size of memory used in the database. */
uint64_t tcmdbmsiz(TCMDB *mdb);


/* Add an integer to a record in an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `num' specifies the additional value.
   The return value is the summation value.
   If the corresponding record exists, the value is treated as an integer and is added to.  If no
   record corresponds, a new record of the additional value is stored. */
int tcmdbaddint(TCMDB *mdb, const void *kbuf, int ksiz, int num);


/* Add a real number to a record in an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `num' specifies the additional value.
   The return value is the summation value.
   If the corresponding record exists, the value is treated as a real number and is added to.  If
   no record corresponds, a new record of the additional value is stored. */
double tcmdbadddouble(TCMDB *mdb, const void *kbuf, int ksiz, double num);


/* Clear an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   All records are removed. */
void tcmdbvanish(TCMDB *mdb);


/* Remove front records of an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `num' specifies the number of records to be removed. */
void tcmdbcutfront(TCMDB *mdb, int num);



/*************************************************************************************************
 * on-memory hash database (for experts)
 *************************************************************************************************/


/* Store a record and make it semivolatile in an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If a record with the same key exists in the map, it is overwritten.  The record is moved to
   the tail. */
void tcmdbput3(TCMDB *mdb, const void *kbuf, int ksiz, const char *vbuf, int vsiz);


/* Store a record of the value of two regions into an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `fvbuf' specifies the pointer to the former region of the value.
   `fvsiz' specifies the size of the former region of the value.
   `lvbuf' specifies the pointer to the latter region of the value.
   `lvsiz' specifies the size of the latter region of the value.
   If a record with the same key exists in the database, it is overwritten. */
void tcmdbput4(TCMDB *mdb, const void *kbuf, int ksiz,
               const void *fvbuf, int fvsiz, const void *lvbuf, int lvsiz);


/* Concatenate a value and make it semivolatile in on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If there is no corresponding record, a new record is created. */
void tcmdbputcat3(TCMDB *mdb, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Store a record into a on-memory hash database object with a duplication handler.
   `mdb' specifies the on-memory hash database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.  `NULL' means that record addition is
   ommited if there is no corresponding record.
   `vsiz' specifies the size of the region of the value.
   `proc' specifies the pointer to the callback function to process duplication.  It receives
   four parameters.  The first parameter is the pointer to the region of the value.  The second
   parameter is the size of the region of the value.  The third parameter is the pointer to the
   variable into which the size of the region of the return value is assigned.  The fourth
   parameter is the pointer to the optional opaque object.  It returns the pointer to the result
   object allocated with `malloc'.  It is released by the caller.  If it is `NULL', the record is
   not modified.  If it is `(void *)-1', the record is removed.
   `op' specifies an arbitrary pointer to be given as a parameter of the callback function.  If
   it is not needed, `NULL' can be specified.
   If successful, the return value is true, else, it is false. */
bool tcmdbputproc(TCMDB *mdb, const void *kbuf, int ksiz, const void *vbuf, int vsiz,
                  TCPDPROC proc, void *op);


/* Retrieve a record and move it astern in an on-memory hash database object.
   `mdb' specifies the on-memory hash database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the region of the value of the
   corresponding record.  `NULL' is returned when no record corresponds.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return value
   is allocated with the `malloc' call, it should be released with the `free' call when it is no
   longer in use.  The internal region of the returned record is moved to the tail so that the
   record will survive for a time under LRU cache algorithm removing records from the head. */
void *tcmdbget3(TCMDB *mdb, const void *kbuf, int ksiz, int *sp);


/* Initialize the iterator of an on-memory map database object in front of a key.
   `mdb' specifies the on-memory map database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   If there is no record corresponding the condition, the iterator is not modified. */
void tcmdbiterinit2(TCMDB *mdb, const void *kbuf, int ksiz);


/* Initialize the iterator of an on-memory map database object in front of a key string.
   `mdb' specifies the on-memory map database object.
   `kstr' specifies the string of the key.
   If there is no record corresponding the condition, the iterator is not modified. */
void tcmdbiterinit3(TCMDB *mdb, const char *kstr);


/* Process each record atomically of an on-memory hash database object.
   `iter' specifies the pointer to the iterator function called for each record.  It receives
   five parameters.  The first parameter is the pointer to the region of the key.  The second
   parameter is the size of the region of the key.  The third parameter is the pointer to the
   region of the value.  The fourth parameter is the size of the region of the value.  The fifth
   parameter is the pointer to the optional opaque object.  It returns true to continue iteration
   or false to stop iteration.
   `op' specifies an arbitrary pointer to be given as a parameter of the iterator function.  If
   it is not needed, `NULL' can be specified. */
void tcmdbforeach(TCMDB *mdb, TCITER iter, void *op);



/*************************************************************************************************
 * on-memory tree database
 *************************************************************************************************/


typedef struct {                         /* type of structure for a on-memory tree database */
  void *mmtx;                            /* mutex for method */
  TCTREE *tree;                          /* internal tree object */
} TCNDB;


/* Create an on-memory tree database object.
   The return value is the new on-memory tree database object.
   The object can be shared by plural threads because of the internal mutex. */
TCNDB *tcndbnew(void);


/* Create an on-memory tree database object with specifying the custom comparison function.
   `cmp' specifies the pointer to the custom comparison function.
   `cmpop' specifies an arbitrary pointer to be given as a parameter of the comparison function.
   If it is not needed, `NULL' can be specified.
   The return value is the new on-memory tree database object.
   The default comparison function compares keys of two records by lexical order.  The functions
   `tccmplexical' (dafault), `tccmpdecimal', `tccmpint32', and `tccmpint64' are built-in.  The
   object can be shared by plural threads because of the internal mutex. */
TCNDB *tcndbnew2(TCCMP cmp, void *cmpop);


/* Delete an on-memory tree database object.
   `ndb' specifies the on-memory tree database object. */
void tcndbdel(TCNDB *ndb);


/* Store a record into an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If a record with the same key exists in the database, it is overwritten. */
void tcndbput(TCNDB *ndb, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Store a string record into an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   `kstr' specifies the string of the key.
   `vstr' specifies the string of the value.
   If a record with the same key exists in the database, it is overwritten. */
void tcndbput2(TCNDB *ndb, const char *kstr, const char *vstr);


/* Store a new record into an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If successful, the return value is true, else, it is false.
   If a record with the same key exists in the database, this function has no effect. */
bool tcndbputkeep(TCNDB *ndb, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Store a new string record into an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   `kstr' specifies the string of the key.
   `vstr' specifies the string of the value.
   If successful, the return value is true, else, it is false.
   If a record with the same key exists in the database, this function has no effect. */
bool tcndbputkeep2(TCNDB *ndb, const char *kstr, const char *vstr);


/* Concatenate a value at the end of the existing record in an on-memory tree database.
   `ndb' specifies the on-memory tree database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If there is no corresponding record, a new record is created. */
void tcndbputcat(TCNDB *ndb, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Concatenate a string at the end of the existing record in an on-memory tree database.
   `ndb' specifies the on-memory tree database object.
   `kstr' specifies the string of the key.
   `vstr' specifies the string of the value.
   If there is no corresponding record, a new record is created. */
void tcndbputcat2(TCNDB *ndb, const char *kstr, const char *vstr);


/* Remove a record of an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   If successful, the return value is true.  False is returned when no record corresponds to
   the specified key. */
bool tcndbout(TCNDB *ndb, const void *kbuf, int ksiz);


/* Remove a string record of an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   `kstr' specifies the string of the key.
   If successful, the return value is true.  False is returned when no record corresponds to
   the specified key. */
bool tcndbout2(TCNDB *ndb, const char *kstr);


/* Retrieve a record in an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the region of the value of the
   corresponding record.  `NULL' is returned when no record corresponds.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when
   it is no longer in use. */
void *tcndbget(TCNDB *ndb, const void *kbuf, int ksiz, int *sp);


/* Retrieve a string record in an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   `kstr' specifies the string of the key.
   If successful, the return value is the string of the value of the corresponding record.
   `NULL' is returned when no record corresponds.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tcndbget2(TCNDB *ndb, const char *kstr);


/* Get the size of the value of a record in an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   If successful, the return value is the size of the value of the corresponding record, else,
   it is -1. */
int tcndbvsiz(TCNDB *ndb, const void *kbuf, int ksiz);


/* Get the size of the value of a string record in an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   `kstr' specifies the string of the key.
   If successful, the return value is the size of the value of the corresponding record, else,
   it is -1. */
int tcndbvsiz2(TCNDB *ndb, const char *kstr);


/* Initialize the iterator of an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   The iterator is used in order to access the key of every record stored in the on-memory
   database. */
void tcndbiterinit(TCNDB *ndb);


/* Get the next key of the iterator of an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the region of the next key, else, it is
   `NULL'.  `NULL' is returned when no record can be fetched from the iterator.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when
   it is no longer in use.  The order of iteration is assured to be the same as the stored
   order. */
void *tcndbiternext(TCNDB *ndb, int *sp);


/* Get the next key string of the iterator of an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   If successful, the return value is the pointer to the region of the next key, else, it is
   `NULL'.  `NULL' is returned when no record can be fetched from the iterator.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use.  The order of iteration is assured
   to be the same as the stored order. */
char *tcndbiternext2(TCNDB *ndb);


/* Get forward matching keys in an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   `pbuf' specifies the pointer to the region of the prefix.
   `psiz' specifies the size of the region of the prefix.
   `max' specifies the maximum number of keys to be fetched.  If it is negative, no limit is
   specified.
   The return value is a list object of the corresponding keys.  This function does never fail.
   It returns an empty list even if no key corresponds.
   Because the object of the return value is created with the function `tclistnew', it should be
   deleted with the function `tclistdel' when it is no longer in use. */
TCLIST *tcndbfwmkeys(TCNDB *ndb, const void *pbuf, int psiz, int max);


/* Get forward matching string keys in an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   `pstr' specifies the string of the prefix.
   `max' specifies the maximum number of keys to be fetched.  If it is negative, no limit is
   specified.
   The return value is a list object of the corresponding keys.  This function does never fail.
   It returns an empty list even if no key corresponds.
   Because the object of the return value is created with the function `tclistnew', it should be
   deleted with the function `tclistdel' when it is no longer in use. */
TCLIST *tcndbfwmkeys2(TCNDB *ndb, const char *pstr, int max);


/* Get the number of records stored in an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   The return value is the number of the records stored in the database. */
uint64_t tcndbrnum(TCNDB *ndb);


/* Get the total size of memory used in an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   The return value is the total size of memory used in the database. */
uint64_t tcndbmsiz(TCNDB *ndb);


/* Add an integer to a record in an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `num' specifies the additional value.
   The return value is the summation value.
   If the corresponding record exists, the value is treated as an integer and is added to.  If no
   record corresponds, a new record of the additional value is stored. */
int tcndbaddint(TCNDB *ndb, const void *kbuf, int ksiz, int num);


/* Add a real number to a record in an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `num' specifies the additional value.
   The return value is the summation value.
   If the corresponding record exists, the value is treated as a real number and is added to.  If
   no record corresponds, a new record of the additional value is stored. */
double tcndbadddouble(TCNDB *ndb, const void *kbuf, int ksiz, double num);


/* Clear an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   All records are removed. */
void tcndbvanish(TCNDB *ndb);


/* Remove fringe records of an on-memory tree database object.
   `ndb' specifies the on-memory tree database object.
   `num' specifies the number of records to be removed. */
void tcndbcutfringe(TCNDB *ndb, int num);



/*************************************************************************************************
 * ordered tree (for experts)
 *************************************************************************************************/


/* Store a record into a on-memory tree database without balancing nodes.
   `ndb' specifies the on-memory tree database.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If a record with the same key exists in the database, it is overwritten.  The structure of the
   tree is not modifed by this function. */
void tcndbput3(TCNDB *ndb, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Store a new record into a on-memory tree database object without balancing nodes.
   `ndb' specifies the on-memory tree database.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If successful, the return value is true, else, it is false.
   If a record with the same key exists in the database, this function has no effect.  The
   structure of the tree is not modifed by this function. */
bool tcndbputkeep3(TCNDB *ndb, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Concatenate a value in a on-memory tree database without balancing nodes.
   `ndb' specifies the on-memory tree database.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.
   `vsiz' specifies the size of the region of the value.
   If there is no corresponding record, a new record is created.  The structure of the tree is
   not modifed by this function. */
void tcndbputcat3(TCNDB *ndb, const void *kbuf, int ksiz, const void *vbuf, int vsiz);


/* Store a record into a on-memory tree database object with a duplication handler.
   `ndb' specifies the on-memory tree database.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `vbuf' specifies the pointer to the region of the value.  `NULL' means that record addition is
   ommited if there is no corresponding record.
   `vsiz' specifies the size of the region of the value.
   `proc' specifies the pointer to the callback function to process duplication.  It receives
   four parameters.  The first parameter is the pointer to the region of the value.  The second
   parameter is the size of the region of the value.  The third parameter is the pointer to the
   variable into which the size of the region of the return value is assigned.  The fourth
   parameter is the pointer to the optional opaque object.  It returns the pointer to the result
   object allocated with `malloc'.  It is released by the caller.  If it is `NULL', the record is
   not modified.  If it is `(void *)-1', the record is removed.
   `op' specifies an arbitrary pointer to be given as a parameter of the callback function.  If
   it is not needed, `NULL' can be specified.
   If successful, the return value is true, else, it is false. */
bool tcndbputproc(TCNDB *ndb, const void *kbuf, int ksiz, const void *vbuf, int vsiz,
                  TCPDPROC proc, void *op);


/* Retrieve a record in an on-memory tree database object without balancing nodes.
   `ndb' specifies the on-memory tree database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the region of the value of the
   corresponding record.  `NULL' is returned when no record corresponds.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return value
   is allocated with the `malloc' call, it should be released with the `free' call when it is no
   longer in use.  The structure of the tree is not modifed by this function. */
void *tcndbget3(TCNDB *ndb, const void *kbuf, int ksiz, int *sp);


/* Initialize the iterator of an on-memory tree database object in front of a key.
   `ndb' specifies the on-memory tree database object.
   `kbuf' specifies the pointer to the region of the key.
   `ksiz' specifies the size of the region of the key.
   The iterator is set to the first record corresponding the key or the next substitute if
   completely matching record does not exist. */
void tcndbiterinit2(TCNDB *ndb, const void *kbuf, int ksiz);


/* Initialize the iterator of an on-memory tree database object in front of a key string.
   `ndb' specifies the on-memory tree database object.
   `kstr' specifies the string of the key.
   The iterator is set to the first record corresponding the key or the next substitute if
   completely matching record does not exist. */
void tcndbiterinit3(TCNDB *ndb, const char *kstr);


/* Process each record atomically of an on-memory tree database object.
   `iter' specifies the pointer to the iterator function called for each record.  It receives
   five parameters.  The first parameter is the pointer to the region of the key.  The second
   parameter is the size of the region of the key.  The third parameter is the pointer to the
   region of the value.  The fourth parameter is the size of the region of the value.  The fifth
   parameter is the pointer to the optional opaque object.  It returns true to continue iteration
   or false to stop iteration.
   `op' specifies an arbitrary pointer to be given as a parameter of the iterator function.  If
   it is not needed, `NULL' can be specified. */
void tcndbforeach(TCNDB *ndb, TCITER iter, void *op);



/*************************************************************************************************
 * memory pool
 *************************************************************************************************/


typedef struct {                         /* type of an element of memory pool */
  void *ptr;                             /* pointer */
  void (*del)(void *);                   /* deleting function */
} TCMPELEM;

typedef struct {                         /* type of structure for a memory pool object */
  void *mutex;                           /* mutex for operations */
  TCMPELEM *elems;                       /* array of elements */
  int anum;                              /* number of the elements of the array */
  int num;                               /* number of used elements */
} TCMPOOL;


/* Create a memory pool object.
   The return value is the new memory pool object. */
TCMPOOL *tcmpoolnew(void);


/* Delete a memory pool object.
   `mpool' specifies the memory pool object.
   Note that the deleted object and its derivatives can not be used anymore. */
void tcmpooldel(TCMPOOL *mpool);


/* Relegate an arbitrary object to a memory pool object.
   `mpool' specifies the memory pool object.
   `ptr' specifies the pointer to the object to be relegated.  If it is `NULL', this function has
   no effect.
   `del' specifies the pointer to the function to delete the object.
   The return value is the pointer to the given object.
   This function assures that the specified object is deleted when the memory pool object is
   deleted. */
void *tcmpoolpush(TCMPOOL *mpool, void *ptr, void (*del)(void *));


/* Relegate an allocated region to a memory pool object.
   `mpool' specifies the memory pool object.
   `ptr' specifies the pointer to the region to be relegated.  If it is `NULL', this function has
   no effect.
   The return value is the pointer to the given object.
   This function assures that the specified region is released when the memory pool object is
   deleted. */
void *tcmpoolpushptr(TCMPOOL *mpool, void *ptr);


/* Relegate an extensible string object to a memory pool object.
   `mpool' specifies the memory pool object.
   `xstr' specifies the extensible string object.  If it is `NULL', this function has no effect.
   The return value is the pointer to the given object.
   This function assures that the specified object is deleted when the memory pool object is
   deleted. */
TCXSTR *tcmpoolpushxstr(TCMPOOL *mpool, TCXSTR *xstr);


/* Relegate a list object to a memory pool object.
   `mpool' specifies the memory pool object.
   `list' specifies the list object.  If it is `NULL', this function has no effect.
   The return value is the pointer to the given object.
   This function assures that the specified object is deleted when the memory pool object is
   deleted. */
TCLIST *tcmpoolpushlist(TCMPOOL *mpool, TCLIST *list);


/* Relegate a map object to a memory pool object.
   `mpool' specifies the memory pool object.
   `map' specifies the map object.  If it is `NULL', this function has no effect.
   The return value is the pointer to the given object.
   This function assures that the specified object is deleted when the memory pool object is
   deleted. */
TCMAP *tcmpoolpushmap(TCMPOOL *mpool, TCMAP *map);


/* Relegate a tree object to a memory pool object.
   `mpool' specifies the memory pool object.
   `tree' specifies the tree object.  If it is `NULL', this function has no effect.
   The return value is the pointer to the given object.
   This function assures that the specified object is deleted when the memory pool object is
   deleted. */
TCTREE *tcmpoolpushtree(TCMPOOL *mpool, TCTREE *tree);


/* Allocate a region relegated to a memory pool object.
   `mpool' specifies the memory pool object.
   The return value is the pointer to the allocated region under the memory pool. */
void *tcmpoolmalloc(TCMPOOL *mpool, size_t size);


/* Create an extensible string object relegated to a memory pool object.
   The return value is the new extensible string object under the memory pool. */
TCXSTR *tcmpoolxstrnew(TCMPOOL *mpool);


/* Create a list object relegated to a memory pool object.
   The return value is the new list object under the memory pool. */
TCLIST *tcmpoollistnew(TCMPOOL *mpool);


/* Create a map object relegated to a memory pool object.
   The return value is the new map object under the memory pool. */
TCMAP *tcmpoolmapnew(TCMPOOL *mpool);


/* Create a tree object relegated to a memory pool object.
   The return value is the new tree object under the memory pool. */
TCTREE *tcmpooltreenew(TCMPOOL *mpool);


/* Remove the most recently installed cleanup handler of a memory pool object.
   `mpool' specifies the memory pool object.
   `exe' specifies whether to execute the destructor of the removed handler. */
void tcmpoolpop(TCMPOOL *mpool, bool exe);


/* Remove all cleanup handler of a memory pool object.
   `mpool' specifies the memory pool object.
   `exe' specifies whether to execute the destructors of the removed handlers. */
void tcmpoolclear(TCMPOOL *mpool, bool exe);


/* Get the global memory pool object.
   The return value is the global memory pool object.
   The global memory pool object is a singleton and assured to be deleted when the porcess is
   terminating normally. */
TCMPOOL *tcmpoolglobal(void);



/*************************************************************************************************
 * miscellaneous utilities
 *************************************************************************************************/


/* Get the larger value of two integers.
   `a' specifies an integer.
   `b' specifies the other integer.
   The return value is the larger value of the two. */
long tclmax(long a, long b);


/* Get the lesser value of two integers.
   `a' specifies an integer.
   `b' specifies the other integer.
   The return value is the lesser value of the two. */
long tclmin(long a, long b);


/* Get a random number as long integer based on uniform distribution.
   The return value is the random number between 0 and `ULONG_MAX'.
   This function uses the random number source device and generates a real random number if
   possible. */
unsigned long tclrand(void);


/* Get a random number as double decimal based on uniform distribution.
   The return value is the random number equal to or greater than 0, and less than 1.0.
   This function uses the random number source device and generates a real random number if
   possible. */
double tcdrand(void);


/* Get a random number as double decimal based on normal distribution.
   `avg' specifies the average.
   `sd' specifies the standard deviation.
   The return value is the random number.
   This function uses the random number source device and generates a real random number if
   possible. */
double tcdrandnd(double avg, double sd);


/* Compare two strings with case insensitive evaluation.
   `astr' specifies a string.
   `bstr' specifies of the other string.
   The return value is positive if the former is big, negative if the latter is big, 0 if both
   are equivalent. */
int tcstricmp(const char *astr, const char *bstr);


/* Check whether a string begins with a key.
   `str' specifies the target string.
   `key' specifies the forward matching key string.
   The return value is true if the target string begins with the key, else, it is false. */
bool tcstrfwm(const char *str, const char *key);


/* Check whether a string begins with a key with case insensitive evaluation.
   `str' specifies the target string.
   `key' specifies the forward matching key string.
   The return value is true if the target string begins with the key, else, it is false. */
bool tcstrifwm(const char *str, const char *key);


/* Check whether a string ends with a key.
   `str' specifies the target string.
   `key' specifies the backward matching key string.
   The return value is true if the target string ends with the key, else, it is false. */
bool tcstrbwm(const char *str, const char *key);


/* Check whether a string ends with a key with case insensitive evaluation.
   `str' specifies the target string.
   `key' specifies the backward matching key string.
   The return value is true if the target string ends with the key, else, it is false. */
bool tcstribwm(const char *str, const char *key);


/* Calculate the edit distance of two strings.
   `astr' specifies a string.
   `bstr' specifies of the other string.
   The return value is the edit distance which is known as the Levenshtein distance.  The cost is
   calculated by byte. */
int tcstrdist(const char *astr, const char *bstr);


/* Calculate the edit distance of two UTF-8 strings.
   `astr' specifies a string.
   `bstr' specifies of the other string.
   The return value is the edit distance which is known as the Levenshtein distance.  The cost is
   calculated by Unicode character. */
int tcstrdistutf(const char *astr, const char *bstr);


/* Convert the letters of a string into upper case.
   `str' specifies the string to be converted.
   The return value is the string itself. */
char *tcstrtoupper(char *str);


/* Convert the letters of a string into lower case.
   `str' specifies the string to be converted.
   The return value is the string itself. */
char *tcstrtolower(char *str);


/* Cut space characters at head or tail of a string.
   `str' specifies the string to be converted.
   The return value is the string itself. */
char *tcstrtrim(char *str);


/* Squeeze space characters in a string and trim it.
   `str' specifies the string to be converted.
   The return value is the string itself. */
char *tcstrsqzspc(char *str);


/* Substitute characters in a string.
   `str' specifies the string to be converted.
   `rstr' specifies the string containing characters to be replaced.
   `sstr' specifies the string containing characters to be substituted.
   If the substitute string is shorter then the replacement string, corresponding characters are
   removed. */
char *tcstrsubchr(char *str, const char *rstr, const char *sstr);


/* Count the number of characters in a string of UTF-8.
   `str' specifies the string of UTF-8.
   The return value is the number of characters in the string. */
int tcstrcntutf(const char *str);


/* Cut a string of UTF-8 at the specified number of characters.
   `str' specifies the string of UTF-8.
   `num' specifies the number of characters to be kept.
   The return value is the string itself. */
char *tcstrcututf(char *str, int num);


/* Convert a UTF-8 string into a UCS-2 array.
   `str' specifies the UTF-8 string.
   `ary' specifies the pointer to the region into which the result UCS-2 codes are written.  The
   size of the buffer should be sufficient.
   `np' specifies the pointer to a variable into which the number of elements of the result array
   is assigned. */
void tcstrutftoucs(const char *str, uint16_t *ary, int *np);


/* Convert a UCS-2 array into a UTF-8 string.
   `ary' specifies the array of UCS-2 codes.
   `num' specifies the number of the array.
   `str' specifies the pointer to the region into which the result UTF-8 string is written.  The
   size of the buffer should be sufficient.
   The return value is the length of the result string. */
int tcstrucstoutf(const uint16_t *ary, int num, char *str);


/* Create a list object by splitting a string.
   `str' specifies the source string.
   `delim' specifies a string containing delimiting characters.
   The return value is a list object of the split elements.
   If two delimiters are successive, it is assumed that an empty element is between the two.
   Because the object of the return value is created with the function `tclistnew', it should be
   deleted with the function `tclistdel' when it is no longer in use. */
TCLIST *tcstrsplit(const char *str, const char *delims);


/* Create a string by joining all elements of a list object.
   `list' specifies a list object.
   `delim' specifies a delimiting character.
   The return value is the result string.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tcstrjoin(const TCLIST *list, char delim);


/* Convert a string to an integer.
   `str' specifies the string.
   The return value is the integer.  If the string does not contain numeric expression, 0 is
   returned.
   This function is equivalent to `atoll' except that it does not depend on the locale. */
int64_t tcatoi(const char *str);


/* Convert a string with a metric prefix to an integer.
   `str' specifies the string, which can be trailed by a binary metric prefix.  "K", "M", "G",
   "T", "P", and "E" are supported.  They are case-insensitive.
   The return value is the integer.  If the string does not contain numeric expression, 0 is
   returned.  If the integer overflows the domain, `INT64_MAX' or `INT64_MIN' is returned
   according to the sign. */
int64_t tcatoix(const char *str);


/* Convert a string to a real number.
   `str' specifies the string.
   The return value is the real number.  If the string does not contain numeric expression, 0.0
   is returned.
   This function is equivalent to `atof' except that it does not depend on the locale. */
double tcatof(const char *str);


/* Check whether a string matches a regular expression.
   `str' specifies the target string.
   `regex' specifies the regular expression string.  If it begins with `*', the trailing
   substring is used as a case-insensitive regular expression.
   The return value is true if matching is success, else, it is false. */
bool tcregexmatch(const char *str, const char *regex);


/* Replace each substring matching a regular expression string.
   `str' specifies the target string.
   `regex' specifies the regular expression string for substrings.  If it begins with `*', the
   trailing substring is used as a case-insensitive regular expression.
   `alt' specifies the alternative string with which each substrings is replaced.  Each `&' in
   the string is replaced with the matched substring.  Each `\' in the string escapes the
   following character.  Special escapes "\1" through "\9" referring to the corresponding
   matching sub-expressions in the regular expression string are supported.
   The return value is a new converted string.  Even if the regular expression is invalid, a copy
   of the original string is returned.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tcregexreplace(const char *str, const char *regex, const char *alt);


/* Get the MD5 hash value of a serial object.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   `buf' specifies the pointer to the region into which the result string is written.  The size
   of the buffer should be equal to or more than 48 bytes. */
void tcmd5hash(const void *ptr, int size, char *buf);


/* Cipher or decipher a serial object with the Arcfour stream cipher.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   `kbuf' specifies the pointer to the region of the cipher key.
   `ksiz' specifies the size of the region of the cipher key.
   `obuf' specifies the pointer to the region into which the result data is written.  The size
   of the buffer should be equal to or more than the input region. */
void tcarccipher(const void *ptr, int size, const void *kbuf, int ksiz, void *obuf);


/* Get the time of day in seconds.
   The return value is the time of day in seconds.  The accuracy is in microseconds. */
double tctime(void);


/* Get the Gregorian calendar of a time.
   `t' specifies the source time in seconds from the epoch.  If it is `INT64_MAX', the current
   time is specified.
   `jl' specifies the jet lag of a location in seconds.  If it is `INT_MAX', the local jet lag is
   specified.
   `yearp' specifies the pointer to a variable to which the year is assigned.  If it is `NULL',
   it is not used.
   `monp' specifies the pointer to a variable to which the month is assigned.  If it is `NULL',
   it is not used.  1 means January and 12 means December.
   `dayp' specifies the pointer to a variable to which the day of the month is assigned.  If it
   is `NULL', it is not used.
   `hourp' specifies the pointer to a variable to which the hours is assigned.  If it is `NULL',
   it is not used.
   `minp' specifies the pointer to a variable to which the minutes is assigned.  If it is `NULL',
   it is not used.
   `secp' specifies the pointer to a variable to which the seconds is assigned.  If it is `NULL',
   it is not used. */
void tccalendar(int64_t t, int jl, int *yearp, int *monp, int *dayp,
                int *hourp, int *minp, int *secp);


/* Format a date as a string in W3CDTF.
   `t' specifies the source time in seconds from the epoch.  If it is `INT64_MAX', the current
   time is specified.
   `jl' specifies the jet lag of a location in seconds.  If it is `INT_MAX', the local jet lag is
   specified.
   `buf' specifies the pointer to the region into which the result string is written.  The size
   of the buffer should be equal to or more than 48 bytes.
   W3CDTF represents a date as "YYYY-MM-DDThh:mm:ddTZD". */
void tcdatestrwww(int64_t t, int jl, char *buf);


/* Format a date as a string in RFC 1123 format.
   `t' specifies the source time in seconds from the epoch.  If it is `INT64_MAX', the current
   time is specified.
   `jl' specifies the jet lag of a location in seconds.  If it is `INT_MAX', the local jet lag is
   specified.
   `buf' specifies the pointer to the region into which the result string is written.  The size
   of the buffer should be equal to or more than 48 bytes.
   RFC 1123 format represents a date as "Wdy, DD-Mon-YYYY hh:mm:dd TZD". */
void tcdatestrhttp(int64_t t, int jl, char *buf);


/* Get the time value of a date string.
   `str' specifies the date string in decimal, hexadecimal, W3CDTF, or RFC 822 (1123).  Decimal
   can be trailed by "s" for in seconds, "m" for in minutes, "h" for in hours, and "d" for in
   days.
   The return value is the time value of the date or `INT64_MIN' if the format is invalid. */
int64_t tcstrmktime(const char *str);


/* Get the jet lag of the local time.
   The return value is the jet lag of the local time in seconds. */
int tcjetlag(void);


/* Get the day of week of a date.
   `year' specifies the year of a date.
   `mon' specifies the month of the date.
   `day' specifies the day of the date.
   The return value is the day of week of the date.  0 means Sunday and 6 means Saturday. */
int tcdayofweek(int year, int mon, int day);



/*************************************************************************************************
 * miscellaneous utilities (for experts)
 *************************************************************************************************/


enum {                                   /* enumeration for UCS normalization */
  TCUNSPACE = 1 << 0,                    /* white space normalization */
  TCUNLOWER = 1 << 1,                    /* lower case normalization */
  TCUNNOACC = 1 << 2,                    /* strip accent marks */
  TCUNWIDTH = 1 << 3                     /* half-width normalization */
};

enum {                                   /* enumeration for KWIC generator */
  TCKWMUTAB = 1 << 0,                    /* mark up by tabs */
  TCKWMUCTRL = 1 << 1,                   /* mark up by control characters */
  TCKWMUBRCT = 1 << 2,                   /* mark up by brackets */
  TCKWNOOVER = 1 << 24,                  /* no overlap */
  TCKWPULEAD = 1 << 25                   /* pick up the lead string */
};

typedef struct {                         /* type of structure for a consistent hashing node */
  uint32_t seq;                          /* sequential number */
  uint32_t hash;                         /* hash value */
} TCCHIDXNODE;

typedef struct {                         /* type of structure for a consistent hashing object */
  TCCHIDXNODE *nodes;                    /* node array */
  int nnum;                              /* number of the node array */
} TCCHIDX;


/* Check whether a string is numeric completely or not.
   `str' specifies the string to be checked.
   The return value is true if the string is numeric, else, it is false. */
bool tcstrisnum(const char *str);


/* Convert a hexadecimal string to an integer.
   `str' specifies the string.
   The return value is the integer.  If the string does not contain numeric expression, 0 is
   returned. */
int64_t tcatoih(const char *str);


/* Skip space characters at head of a string.
   `str' specifies the string.
   The return value is the pointer to the first non-space character. */
const char *tcstrskipspc(const char *str);


/* Normalize a UTF-8 string.
   `str' specifies the string of UTF-8.
   `opts' specifies options by bitwise-or: `TCUNSPACE' specifies that white space characters are
   normalized into the ASCII space and they are squeezed into one, `TCUNLOWER' specifies that
   alphabetical characters are normalized into lower cases, `TCUNNOACC' specifies that
   alphabetical characters with accent marks are normalized without accent marks, `TCUNWIDTH'
   specifies that full-width characters are normalized into half-width characters.
   The return value is the string itself. */
char *tcstrutfnorm(char *str, int opts);


/* Normalize a UCS-2 array.
   `ary' specifies the array of UCS-2 codes.
   `num' specifies the number of elements of the array.
   `opts' specifies options by bitwise-or: `TCUNSPACE' specifies that white space characters are
   normalized into the ASCII space and they are squeezed into one, `TCUNLOWER' specifies that
   alphabetical characters are normalized into lower cases, `TCUNNOACC' specifies that
   alphabetical characters with accent marks are normalized without accent marks, `TCUNWIDTH'
   specifies that full-width characters are normalized into half-width characters.
   The return value is the number of elements of the result array. */
int tcstrucsnorm(uint16_t *ary, int num, int opts);


/* Generate a keyword-in-context string from a text and keywords.
   `str' specifies the text string of UTF-8.
   `words' specifies a list object of the keyword strings.
   `width' specifies the width of strings picked up around each keyword.
   `opts' specifies options by bitwise-or: `TCKWMUTAB' specifies that each keyword is marked up
   between two tab characters, `TCKWMUCTRL' specifies that each keyword is marked up by the STX
   (0x02) code and the ETX (0x03) code, `TCKWMUBRCT' specifies that each keyword is marked up by
   the two square brackets, `TCKWNOOVER' specifies that each context does not overlap,
   `TCKWPULEAD' specifies that the lead string is picked up forcibly.
   The return value is the list object whose elements are strings around keywords.
   Because the object of the return value is created with the function `tclistnew', it should
   be deleted with the function `tclistdel' when it is no longer in use. */
TCLIST *tcstrkwic(const char *str, const TCLIST *words, int width, int opts);


/* Tokenize a text separating by white space characters.
   `str' specifies the string.
   The return value is the list object whose elements are extracted tokens.
   Because the object of the return value is created with the function `tclistnew', it should
   be deleted with the function `tclistdel' when it is no longer in use. */
TCLIST *tcstrtokenize(const char *str);


/* Create a list object by splitting a region by zero code.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   The return value is a list object of the split elements.
   If two delimiters are successive, it is assumed that an empty element is between the two.
   Because the object of the return value is created with the function `tclistnew', it should be
   deleted with the function `tclistdel' when it is no longer in use. */
TCLIST *tcstrsplit2(const void *ptr, int size);


/* Create a map object by splitting a string.
   `str' specifies the source string where the key and the value of each record are situated one
   after the other.
   `delim' specifies a string containing delimiting characters.
   The return value is a map object of the split records.
   Because the object of the return value is created with the function `tcmapnew', it should be
   deleted with the function `tcmapdel' when it is no longer in use. */
TCMAP *tcstrsplit3(const char *str, const char *delims);


/* Create a map object by splitting a region by zero code.
   `ptr' specifies the pointer to the region where the key and the value of each record are
   situated one after the other.
   `size' specifies the size of the region.
   The return value is a map object of the split records.
   Because the object of the return value is created with the function `tcmapnew', it should be
   deleted with the function `tcmapdel' when it is no longer in use. */
TCMAP *tcstrsplit4(const void *ptr, int size);


/* Create a region separated by zero code by joining all elements of a list object.
   `list' specifies a list object.
   The return value is the result region.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
void *tcstrjoin2(const TCLIST *list, int *sp);


/* Create a string by joining all records of a map object.
   `map' specifies a map object.
   `delim' specifies a delimiting character.
   The return value is the result string where the key and the value of each record are situated
   one after the other.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tcstrjoin3(const TCMAP *map, char delim);


/* Create a region separated by zero code by joining all records of a map object.
   `list' specifies a list object.
   The return value is the result region, where the key and the value of each record are
   situated one after the other.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
void *tcstrjoin4(const TCMAP *map, int *sp);


/* Sort top records of an array.
   `base' spacifies the pointer to an array.
   `nmemb' specifies the number of elements of the array.
   `size' specifies the size of each element.
   `top' specifies the number of top records.
   `compar' specifies the pointer to comparing function.  The two arguments specify the pointers
   of elements.  The comparing function should returns positive if the former is big, negative
   if the latter is big, 0 if both are equal. */
void tctopsort(void *base, size_t nmemb, size_t size, size_t top,
               int(*compar)(const void *, const void *));


/* Suspend execution of the current thread.
   `sec' specifies the interval of the suspension in seconds.
   If successful, the return value is true, else, it is false. */
bool tcsleep(double sec);


/* Get the current system information.
   The return value is a map object of the current system information or `NULL' on failure.
   The key "utime" indicates the user time of the CPU.  The key "stime" indicates the system time
   of the CPU.  The key "size" indicates the process size in bytes.  The "rss" indicates the
   resident set size in bytes.  "total" indicates the total size of the real memory.  "free"
   indicates the free size of the real memory.  "cached" indicates the cached size of the real
   memory.
   Because the object of the return value is created with the function `tcmapnew', it should be
   deleted with the function `tcmapdel' when it is no longer in use. */
TCMAP *tcsysinfo(void);


/* Create a consistent hashing object.
   `range' specifies the number of nodes.  It should be more than 0.  The range of hash values is
   from 0 to less than the specified number.
   The return value is the new consistent hashing object.
   Consistent hashing is useful because the addition or removal of one node does not
   significantly change the mapping of keys to nodes. */
TCCHIDX *tcchidxnew(int range);


/* Delete a consistent hashing object.
   `chidx' specifies the consistent hashing object. */
void tcchidxdel(TCCHIDX *chidx);


/* Get the consistent hashing value of a record.
   `chidx' specifies the consistent hashing object.
   `ptr' specifies the pointer to the region of the record.
   `size' specifies the size of the region.
   The return value is the hash value of the record. */
int tcchidxhash(TCCHIDX *chidx, const void *ptr, int size);



/*************************************************************************************************
 * filesystem utilities
 *************************************************************************************************/


/* Get the canonicalized absolute path of a file.
   `path' specifies the path of the file.
   The return value is the canonicalized absolute path of a file, or `NULL' if the path is
   invalid.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tcrealpath(const char *path);


/* Get the status information of a file.
   `path' specifies the path of the file.
   `isdirp' specifies the pointer to a variable into which whether the file is a directory is
   assigned.  If it is `NULL', it is ignored.
   `sizep' specifies the pointer to a variable into which the size of the file is assigned.  If
   it is `NULL', it is ignored.
   `ntimep' specifies the pointer to a variable into which the size of the file is assigned.  If
   it is `NULL', it is ignored.
   If successful, the return value is true, else, it is false. */
bool tcstatfile(const char *path, bool *isdirp, int64_t *sizep, int64_t *mtimep);


/* Read whole data of a file.
   `path' specifies the path of the file.  If it is `NULL', the standard input is specified.
   `limit' specifies the limiting size of reading data.  If it is not more than 0, the limitation
   is not specified.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.  If it is `NULL', it is not used.
   The return value is the pointer to the allocated region of the read data, or `NULL' if the
   file could not be opened.
   Because an additional zero code is appended at the end of the region of the return value, the
   return value can be treated as a character string.  Because the region of the return value is
   allocated with the `malloc' call, it should be released with the `free' call when when is no
   longer in use.  */
void *tcreadfile(const char *path, int limit, int *sp);


/* Read every line of a file.
   `path' specifies the path of the file.  If it is `NULL', the standard input is specified.
   The return value is a list object of every lines if successful, else it is `NULL'.
   Line separators are cut out.  Because the object of the return value is created with the
   function `tclistnew', it should be deleted with the function `tclistdel' when it is no longer
   in use. */
TCLIST *tcreadfilelines(const char *path);


/* Write data into a file.
   `path' specifies the path of the file.  If it is `NULL', the standard output is specified.
   `ptr' specifies the pointer to the data region.
   `size' specifies the size of the region.
   If successful, the return value is true, else, it is false. */
bool tcwritefile(const char *path, const void *ptr, int size);


/* Copy a file.
   `src' specifies the path of the source file.
   `dest' specifies the path of the destination file.
   The return value is true if successful, else, it is false.
   If the destination file exists, it is overwritten. */
bool tccopyfile(const char *src, const char *dest);


/* Read names of files in a directory.
   `path' specifies the path of the directory.
   The return value is a list object of names if successful, else it is `NULL'.
   Links to the directory itself and to the parent directory are ignored.
   Because the object of the return value is created with the function `tclistnew', it should
   be deleted with the function `tclistdel' when it is no longer in use. */
TCLIST *tcreaddir(const char *path);


/* Expand a pattern into a list of matched paths.
   `pattern' specifies the matching pattern.
   The return value is a list object of matched paths.  If no path is matched, an empty list is
   returned.
   Because the object of the return value is created with the function `tclistnew', it should
   be deleted with the function `tclistdel' when it is no longer in use. */
TCLIST *tcglobpat(const char *pattern);


/* Remove a file or a directory and its sub ones recursively.
   `path' specifies the path of the link.
   If successful, the return value is true, else, it is false.  False is returned when the link
   does not exist or the permission is denied. */
bool tcremovelink(const char *path);


/* Write data into a file.
   `fd' specifies the file descriptor.
   `buf' specifies the buffer to be written.
   `size' specifies the size of the buffer.
   The return value is true if successful, else, it is false. */
bool tcwrite(int fd, const void *buf, size_t size);


/* Read data from a file.
   `fd' specifies the file descriptor.
   `buf' specifies the buffer to store into.
   `size' specifies the size of the buffer.
   The return value is true if successful, else, it is false. */
bool tcread(int fd, void *buf, size_t size);


/* Lock a file.
   `fd' specifies the file descriptor.
   `ex' specifies whether an exclusive lock or a shared lock is performed.
   `nb' specifies whether to request with non-blocking.
   The return value is true if successful, else, it is false. */
bool tclock(int fd, bool ex, bool nb);


/* Unlock a file.
   `fd' specifies the file descriptor.
   The return value is true if successful, else, it is false. */
bool tcunlock(int fd);


/* Execute a shell command.
   `args' specifies an array of the command name and its arguments.
   `anum' specifies the number of elements of the array.
   The return value is the exit code of the command or `INT_MAX' on failure.
   The command name and the arguments are quoted and meta characters are escaped. */
int tcsystem(const char **args, int anum);



/*************************************************************************************************
 * encoding utilities
 *************************************************************************************************/


/* Encode a serial object with URL encoding.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   The return value is the result string.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call if when is no longer in use. */
char *tcurlencode(const char *ptr, int size);


/* Decode a string encoded with URL encoding.
   `str' specifies the encoded string.
   `sp' specifies the pointer to a variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the result.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when
   it is no longer in use. */
char *tcurldecode(const char *str, int *sp);


/* Break up a URL into elements.
   `str' specifies the URL string.
   The return value is the map object whose keys are the name of elements.  The key "self"
   specifies the URL itself.  The key "scheme" indicates the scheme.  The key "host" indicates
   the host of the server.  The key "port" indicates the port number of the server.  The key
   "authority" indicates the authority information.  The key "path" indicates the path of the
   resource.  The key "file" indicates the file name without the directory section.  The key
   "query" indicates the query string.  The key "fragment" indicates the fragment string.
   Supported schema are HTTP, HTTPS, FTP, and FILE.  Absolute URL and relative URL are supported.
   Because the object of the return value is created with the function `tcmapnew', it should be
   deleted with the function `tcmapdel' when it is no longer in use. */
TCMAP *tcurlbreak(const char *str);


/* Resolve a relative URL with an absolute URL.
   `base' specifies the absolute URL of the base location.
   `target' specifies the URL to be resolved.
   The return value is the resolved URL.  If the target URL is relative, a new URL of relative
   location from the base location is returned.  Else, a copy of the target URL is returned.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tcurlresolve(const char *base, const char *target);


/* Encode a serial object with Base64 encoding.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   The return value is the result string.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call if when is no longer in use. */
char *tcbaseencode(const char *ptr, int size);


/* Decode a string encoded with Base64 encoding.
   `str' specifies the encoded string.
   `sp' specifies the pointer to a variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the result.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when
   it is no longer in use. */
char *tcbasedecode(const char *str, int *sp);


/* Encode a serial object with Quoted-printable encoding.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   The return value is the result string.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call if when is no longer in use. */
char *tcquoteencode(const char *ptr, int size);


/* Decode a string encoded with Quoted-printable encoding.
   `str' specifies the encoded string.
   `sp' specifies the pointer to a variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the result.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when
   it is no longer in use. */
char *tcquotedecode(const char *str, int *sp);


/* Encode a string with MIME encoding.
   `str' specifies the string.
   `encname' specifies the string of the name of the character encoding.
   `base' specifies whether to use Base64 encoding.  If it is false, Quoted-printable is used.
   The return value is the result string.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tcmimeencode(const char *str, const char *encname, bool base);


/* Decode a string encoded with MIME encoding.
   `str' specifies the encoded string.
   `enp' specifies the pointer to the region into which the name of encoding is written.  If it
   is `NULL', it is not used.  The size of the buffer should be equal to or more than 32 bytes.
   The return value is the result string.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tcmimedecode(const char *str, char *enp);


/* Split a string of MIME into headers and the body.
   `ptr' specifies the pointer to the region of MIME data.
   `size' specifies the size of the region.
   `headers' specifies a map object to store headers.  If it is `NULL', it is not used.  Each key
   of the map is an uncapitalized header name.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the body data.
   If the content type is defined, the header map has the key "TYPE" specifying the type.  If the
   character encoding is defined, the key "CHARSET" indicates the encoding name.  If the boundary
   string of multipart is defined, the key "BOUNDARY" indicates the string.  If the content
   disposition is defined, the key "DISPOSITION" indicates the direction.  If the file name is
   defined, the key "FILENAME" indicates the name.  If the attribute name is defined, the key
   "NAME" indicates the name.  Because the region of the return value is allocated with the
   `malloc' call, it should be released with the `free' call when it is no longer in use. */
char *tcmimebreak(const char *ptr, int size, TCMAP *headers, int *sp);


/* Split multipart data of MIME into its parts.
   `ptr' specifies the pointer to the region of multipart data of MIME.
   `size' specifies the size of the region.
   `boundary' specifies the boundary string.
   The return value is a list object.  Each element of the list is the data of a part.
   Because the object of the return value is created with the function `tclistnew', it should be
   deleted with the function `tclistdel' when it is no longer in use. */
TCLIST *tcmimeparts(const char *ptr, int size, const char *boundary);


/* Encode a serial object with hexadecimal encoding.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   The return value is the result string.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call if when is no longer in use. */
char *tchexencode(const char *ptr, int size);


/* Decode a string encoded with hexadecimal encoding.
   `str' specifies the encoded string.
   `sp' specifies the pointer to a variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the result.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when
   it is no longer in use. */
char *tchexdecode(const char *str, int *sp);


/* Compress a serial object with Packbits encoding.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the result object, else, it is `NULL'.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tcpackencode(const char *ptr, int size, int *sp);


/* Decompress a serial object compressed with Packbits encoding.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   `sp' specifies the pointer to a variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the result object, else, it is `NULL'.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when it
   is no longer in use. */
char *tcpackdecode(const char *ptr, int size, int *sp);


/* Compress a serial object with TCBS encoding.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the result object, else, it is `NULL'.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tcbsencode(const char *ptr, int size, int *sp);


/* Decompress a serial object compressed with TCBS encoding.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   `sp' specifies the pointer to a variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the result object, else, it is `NULL'.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when it
   is no longer in use. */
char *tcbsdecode(const char *ptr, int size, int *sp);


/* Compress a serial object with Deflate encoding.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the result object, else, it is `NULL'.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tcdeflate(const char *ptr, int size, int *sp);


/* Decompress a serial object compressed with Deflate encoding.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   `sp' specifies the pointer to a variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the result object, else, it is `NULL'.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when it
   is no longer in use. */
char *tcinflate(const char *ptr, int size, int *sp);


/* Compress a serial object with GZIP encoding.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the result object, else, it is `NULL'.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tcgzipencode(const char *ptr, int size, int *sp);


/* Decompress a serial object compressed with GZIP encoding.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   `sp' specifies the pointer to a variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the result object, else, it is `NULL'.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when it
   is no longer in use. */
char *tcgzipdecode(const char *ptr, int size, int *sp);


/* Get the CRC32 checksum of a serial object.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   The return value is the CRC32 checksum of the object. */
unsigned int tcgetcrc(const char *ptr, int size);


/* Compress a serial object with BZIP2 encoding.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the result object, else, it is `NULL'.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tcbzipencode(const char *ptr, int size, int *sp);


/* Decompress a serial object compressed with BZIP2 encoding.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   `sp' specifies the pointer to a variable into which the size of the region of the return
   value is assigned.
   If successful, the return value is the pointer to the result object, else, it is `NULL'.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when it
   is no longer in use. */
char *tcbzipdecode(const char *ptr, int size, int *sp);


/* Encode an array of nonnegative integers with BER encoding.
   `ary' specifies the pointer to the array of nonnegative integers.
   `anum' specifies the size of the array.
   `sp' specifies the pointer to a variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the result.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call if when is no longer in use. */
char *tcberencode(const unsigned int *ary, int anum, int *sp);


/* Decode a serial object encoded with BER encoding.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   `np' specifies the pointer to a variable into which the number of elements of the return value
   is assigned.
   The return value is the pointer to the array of the result.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call if when is no longer in use. */
unsigned int *tcberdecode(const char *ptr, int size, int *np);


/* Escape meta characters in a string with the entity references of XML.
   `str' specifies the string.
   The return value is the pointer to the escaped string.
   This function escapes only `&', `<', `>', and `"'.  Because the region of the return value
   is allocated with the `malloc' call, it should be released with the `free' call when it is no
   longer in use. */
char *tcxmlescape(const char *str);


/* Unescape entity references in a string of XML.
   `str' specifies the string.
   The return value is the unescaped string.
   This function restores only `&amp;', `&lt;', `&gt;', and `&quot;'.  Because the region of the
   return value is allocated with the `malloc' call, it should be released with the `free' call
   when it is no longer in use. */
char *tcxmlunescape(const char *str);



/*************************************************************************************************
 * encoding utilities (for experts)
 *************************************************************************************************/


/* Encode a map object into a string in the x-www-form-urlencoded format.
   `params' specifies a map object of parameters.
   The return value is the result string.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tcwwwformencode(const TCMAP *params);


/* Decode a query string in the x-www-form-urlencoded format.
   `str' specifies the query string.
   `params' specifies a map object into which the result parameters are stored. */
void tcwwwformdecode(const char *str, TCMAP *params);


/* Decode a data region in the x-www-form-urlencoded or multipart-form-data format.
   `ptr' specifies the pointer to the data region.
   `size' specifies the size of the data region.
   `type' specifies the value of the content-type header.  If it is `NULL', the type is specified
   as x-www-form-urlencoded.
   `params' specifies a map object into which the result parameters are stored. */
void tcwwwformdecode2(const void *ptr, int size, const char *type, TCMAP *params);


/* Split an XML string into tags and text sections.
   `str' specifies the string.
   The return value is the list object whose elements are strings of tags or text sections.
   Because the object of the return value is created with the function `tclistnew', it should
   be deleted with the function `tclistdel' when it is no longer in use.  Because this function
   does not check validation, it can handle also HTML and SGML. */
TCLIST *tcxmlbreak(const char *str);


/* Get the map of attributes of an XML tag.
   `str' specifies the pointer to the region of a tag string.
   The return value is the map object containing attribute names and their values which are
   unescaped.  You can get the name of the tag with the key of an empty string.
   Because the object of the return value is created with the function `tcmapnew', it should
   be deleted with the function `tcmapdel' when it is no longer in use. */
TCMAP *tcxmlattrs(const char *str);


/* Escape meta characters in a string with backslash escaping of the C language.
   `str' specifies the string.
   The return value is the pointer to the escaped string.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call if when is no longer in use. */
char *tccstrescape(const char *str);


/* Unescape a string escaped by backslash escaping of the C language.
   `str' specifies the string.
   The return value is the unescaped string.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call if when is no longer in use. */
char *tccstrunescape(const char *str);


/* Escape meta characters in a string with backslash escaping of JSON.
   `str' specifies the string.
   The return value is the pointer to the escaped string.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call if when is no longer in use. */
char *tcjsonescape(const char *str);


/* Unescape a string escaped by backslash escaping of JSON.
   `str' specifies the string.
   The return value is the unescaped string.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call if when is no longer in use. */
char *tcjsonunescape(const char *str);



/*************************************************************************************************
 * template serializer
 *************************************************************************************************/


typedef struct {                         /* type of structure for a template */
  TCLIST *elems;                         /* elements separated by the separators */
  char *begsep;                          /* beginning separator */
  char *endsep;                          /* ending separator */
  TCMAP *conf;                           /* configuration variables */
} TCTMPL;


/* Create a template object.
   The return value is the new template object. */
TCTMPL *tctmplnew(void);


/* Delete a template object.
   `tmpl' specifies the template object. */
void tctmpldel(TCTMPL *tmpl);


/* Set the separator strings of a template object.
   `tmpl' specifies the template object.
   `begsep' specifies the beginning separator string.  By default, it is "[%".
   `endsep' specifies the ending separator string.  By default, it is "%]". */
void tctmplsetsep(TCTMPL *tmpl, const char *begsep, const char *endsep);


/* Load a template string into a template object.
   `tmpl' specifies the template object.
   `str' specifies the template string.  Directives between "[%" and "%]" can be included in the
   template string.  If the variable name is specified in the directive, it is expanded as the
   value of the variable.  "." is used in order to access a record of a hash variable.  For
   example, "[% foo.bar.baz %]" is expanded as the value of the record whose key is "baz" in the
   hash variable of the record whose key is "bar" in the hash variable whose name is "foo".
   Moreover, control flow directives are also supported.  "[% IF ... %]", "[% ELSE %]", and
   "[% END %]" are conditional directives.  "[% FOREACH ... %]" and "[% END %]" are iterator
   directives for a list object.  "[% SET ... %]" is a session variable setting directive.
   "[% CONF ... %]" is a configuration directive.  If the ending separator of a directive is
   leaded by "\", the next linefeed character is ignored.  Variable expansion directive needs the
   parameter for the variable name.  The optional parameter "DEF" trailed by a string specifies
   the default value.  The optional parameter "ENC" trailed by a string specifies the encoding
   format.  "URL" for the URL escape encoding, "XML" for the XML escape encoding, "CSTR" for
   C-string escape encoding, and "JSON" for JSON escape encoding are supported.  The conditional
   directive needs the parameter for the variable name.  If the variable exists, the block to the
   correspondent ending directive is evaluated, else, the block is ignored.  The optional
   parameter "EQ" trailed by a string specifies the string full matching test.  The optional
   parameter "INC" trailed by a string specifies the string including matching test.  The
   optional parameter "PRT" indicates the printable test.  The optional parameter "RX" trailed by
   a string specifies the regular expression matching test.  The optional parameter "NOT" inverts
   the logical determination.  The iterator directive needs the parameter for the variable name
   of a list object.  The block to the correspondent ending directive is evaluated for each
   element of the list.  The optional parameter specifies the local variable name of each
   element.  The session variable setting directive needs the parameters for the variable name
   and its value.  The configuration directive needs the parameters for the variable name and
   its value. */
void tctmplload(TCTMPL *tmpl, const char *str);


/* Load a template string from a file into a template object.
   `tmpl' specifies the template object.
   `path' specifies the input file.
   If successful, the return value is true, else, it is false. */
bool tctmplload2(TCTMPL *tmpl, const char *path);


/* Serialize the template string of a template object.
   `tmpl' specifies the template object.
   `vars' specifies the variables to be applied into the template.
   The return value is the dumped template string.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
char *tctmpldump(TCTMPL *tmpl, const TCMAP *vars);


/* Get the value of a configuration variable of a template object.
   `tmpl' specifies the template object.
   `name' specifies the name of the configuration variable.
   The return value is the string value of the configuration variable or `NULL' if it is not
   defined. */
const char *tctmplconf(TCTMPL *tmpl, const char *name);


/* Store a list object into a list object with the type information.
   `list' specifies the container list object.
   `obj' specifies the list object to be stored. */
void tclistpushlist(TCLIST *list, const TCLIST *obj);


/* Store a map object into a list object with the type information.
   `list' specifies the container list object.
   `obj' specifies the map object to be stored. */
void tclistpushmap(TCLIST *list, const TCMAP *obj);


/* Store a list object into a map object with the type information.
   `map' specifies the container map object.
   `kstr' specifies the string of the key.
   `obj' specifies the list object to be stored. */
void tcmapputlist(TCMAP *map, const char *kstr, const TCLIST *obj);


/* Store a map object into a map object with the type information.
   `map' specifies the container map object.
   `kstr' specifies the string of the key.
   `obj' specifies the map object to be stored. */
void tcmapputmap(TCMAP *map, const char *kstr, const TCMAP *obj);



/*************************************************************************************************
 * pointer list
 *************************************************************************************************/


typedef struct {                         /* type of structure for a pointer list */
  void **array;                          /* array of pointers */
  int anum;                              /* number of the elements of the array */
  int start;                             /* start index of used elements */
  int num;                               /* number of used elements */
} TCPTRLIST;


/* Create a pointer list object.
   The return value is the new pointer list object. */
TCPTRLIST *tcptrlistnew(void);


/* Create a pointer list object with expecting the number of elements.
   `anum' specifies the number of elements expected to be stored in the list.
   The return value is the new pointer list object. */
TCPTRLIST *tcptrlistnew2(int anum);


/* Copy a pointer list object.
   `ptrlist' specifies the pointer list object.
   The return value is the new pointer list object equivalent to the specified object. */
TCPTRLIST *tcptrlistdup(const TCPTRLIST *ptrlist);


/* Delete a pointer list object.
   `ptrlist' specifies the pointer list object.
   Note that the deleted object and its derivatives can not be used anymore. */
void tcptrlistdel(TCPTRLIST *ptrlist);


/* Get the number of elements of a pointer list object.
   `ptrlist' specifies the pointer list object.
   The return value is the number of elements of the list. */
int tcptrlistnum(const TCPTRLIST *ptrlist);


/* Get the pointer to the region of an element of a pointer list object.
   `ptrlist' specifies the pointer list object.
   `index' specifies the index of the element.
   The return value is the pointer to the region of the value.
   If `index' is equal to or more than the number of elements, the return value is `NULL'. */
void *tcptrlistval(const TCPTRLIST *ptrlist, int index);


/* Add an element at the end of a pointer list object.
   `ptrlist' specifies the pointer list object.
   `ptr' specifies the pointer to the region of the new element. */
void tcptrlistpush(TCPTRLIST *ptrlist, void *ptr);


/* Remove an element of the end of a pointer list object.
   `ptrlist' specifies the pointer list object.
   The return value is the pointer to the region of the removed element.
   If the list is empty, the return value is `NULL'. */
void *tcptrlistpop(TCPTRLIST *ptrlist);


/* Add an element at the top of a pointer list object.
   `ptrlist' specifies the pointer list object.
   `ptr' specifies the pointer to the region of the new element. */
void tcptrlistunshift(TCPTRLIST *ptrlist, void *ptr);


/* Remove an element of the top of a pointer list object.
   `ptrlist' specifies the pointer list object.
   The return value is the pointer to the region of the removed element.
   If the list is empty, the return value is `NULL'. */
void *tcptrlistshift(TCPTRLIST *ptrlist);


/* Add an element at the specified location of a pointer list object.
   `ptrlist' specifies the pointer list object.
   `index' specifies the index of the new element.
   `ptr' specifies the pointer to the region of the new element.
   If `index' is equal to or more than the number of elements, this function has no effect. */
void tcptrlistinsert(TCPTRLIST *ptrlist, int index, void *ptr);


/* Remove an element at the specified location of a pointer list object.
   `ptrlist' specifies the pointer list object.
   `index' specifies the index of the element to be removed.
   The return value is the pointer to the region of the removed element.
   If `index' is equal to or more than the number of elements, no element is removed and the
   return value is `NULL'. */
void *tcptrlistremove(TCPTRLIST *ptrlist, int index);


/* Overwrite an element at the specified location of a pointer list object.
   `ptrlist' specifies the pointer list object.
   `index' specifies the index of the element to be overwritten.
   `ptr' specifies the pointer to the region of the new content.
   If `index' is equal to or more than the number of elements, this function has no effect. */
void tcptrlistover(TCPTRLIST *ptrlist, int index, void *ptr);


/* Clear a pointer list object.
   `ptrlist' specifies the pointer list object.
   All elements are removed. */
void tcptrlistclear(TCPTRLIST *ptrlist);



/*************************************************************************************************
 * bit operation utilities
 *************************************************************************************************/


typedef struct {                         /* type of structure for a bit stream object */
  uint8_t *sp;                           /* start pointer */
  uint8_t *cp;                           /* current pointer */
  int idx;                               /* bit index */
  int size;                              /* size of used region */
} TCBITSTRM;

typedef unsigned char TCBITMAP;          /* type of a bit map object */


/* Create a bitmap object. */
#define TCBITMAPNEW(TC_num) \
  tccalloc(((TC_num) >> 3) + 1, 1);


/* Delete a bitmap object */
#define TCBITMAPDEL(TC_bitmap) \
  do { \
    tcfree((TC_bitmap)); \
  } while(false);


/* Turn on a field of a bitmap object. */
#define TCBITMAPON(TC_bitmap, TC_idx) \
  do { \
    (TC_bitmap)[(TC_idx)>>3] |= 0x1 << ((TC_idx) & 0x7); \
  } while(false);


/* Turn off a field of a bitmap object. */
#define TCBITMAPOFF(TC_bitmap, TC_idx) \
  do { \
    (TC_bitmap)[(TC_idx)>>3] &= ~(0x1 << ((TC_idx) & 0x7)); \
  } while(false);


/* Check a field of a bitmap object. */
#define TCBITMAPCHECK(TC_bitmap, TC_idx) \
  ((TC_bitmap)[(TC_idx)>>3] & 0x1 << ((TC_idx) & 0x7))


/* Initialize a bit stream object as writer. */
#define TCBITSTRMINITW(TC_bitstrm, TC_ptr) \
  do { \
    (TC_bitstrm).sp = (uint8_t *)(TC_ptr); \
    (TC_bitstrm).cp = (TC_bitstrm).sp; \
    *(TC_bitstrm).cp = 0; \
    (TC_bitstrm).idx = 3; \
    (TC_bitstrm).size = 1; \
  } while(false);


/* Concatenate a bit to a bit stream object. */
#define TCBITSTRMCAT(TC_bitstrm, sign) \
  do { \
    if((TC_bitstrm).idx >= 8){ \
      *(++(TC_bitstrm).cp) = 0; \
      (TC_bitstrm).idx = 0; \
      (TC_bitstrm).size++; \
    } \
    *(TC_bitstrm).cp |= (sign << (TC_bitstrm).idx); \
    (TC_bitstrm).idx++; \
  } while(false);


/* Set the end mark to a bit stream object. */
#define TCBITSTRMSETEND(TC_bitstrm) \
  do { \
    if((TC_bitstrm).idx >= 8){ \
      *(++(TC_bitstrm).cp) = 0; \
      (TC_bitstrm).idx = 0; \
      (TC_bitstrm).size++; \
    } \
    *(TC_bitstrm).sp |= (TC_bitstrm).idx & 7; \
  } while(false);


/* Get the size of the used region of a bit stream object. */
#define TCBITSTRMSIZE(TC_bitstrm) \
  ((TC_bitstrm).size)


/* Initialize a bit stream object as reader. */
#define TCBITSTRMINITR(TC_bitstrm, TC_ptr, TC_size) \
  do { \
    (TC_bitstrm).sp = (uint8_t *)(TC_ptr); \
    (TC_bitstrm).cp = (TC_bitstrm).sp; \
    (TC_bitstrm).idx = 3; \
    (TC_bitstrm).size = (TC_size); \
  } while(false);


/* Read a bit from a bit stream object. */
#define TCBITSTRMREAD(TC_bitstrm, TC_sign) \
  do { \
    if((TC_bitstrm).idx >= 8){ \
      (TC_bitstrm).cp++; \
      (TC_bitstrm).idx = 0; \
    } \
    (TC_sign) = (*((TC_bitstrm).cp) & (1 << (TC_bitstrm).idx)) > 0; \
    (TC_bitstrm).idx++; \
  } while(false);


/* Get the number of bits of a bit stream object. */
#define TCBITSTRMNUM(TC_bitstrm) \
  ((((TC_bitstrm).size - 1) << 3) + (*(TC_bitstrm).sp & 7) - 3)



/*************************************************************************************************
 * features for experts
 *************************************************************************************************/


#include <stdio.h>

#define _TC_VERSION    "1.4.46"
#define _TC_LIBVER     909
#define _TC_FORMATVER  "1.0"

enum {                                   /* enumeration for error codes */
  TCESUCCESS,                            /* success */
  TCETHREAD,                             /* threading error */
  TCEINVALID,                            /* invalid operation */
  TCENOFILE,                             /* file not found */
  TCENOPERM,                             /* no permission */
  TCEMETA,                               /* invalid meta data */
  TCERHEAD,                              /* invalid record header */
  TCEOPEN,                               /* open error */
  TCECLOSE,                              /* close error */
  TCETRUNC,                              /* trunc error */
  TCESYNC,                               /* sync error */
  TCESTAT,                               /* stat error */
  TCESEEK,                               /* seek error */
  TCEREAD,                               /* read error */
  TCEWRITE,                              /* write error */
  TCEMMAP,                               /* mmap error */
  TCELOCK,                               /* lock error */
  TCEUNLINK,                             /* unlink error */
  TCERENAME,                             /* rename error */
  TCEMKDIR,                              /* mkdir error */
  TCERMDIR,                              /* rmdir error */
  TCEKEEP,                               /* existing record */
  TCENOREC,                              /* no record found */
  TCEMISC = 9999                         /* miscellaneous error */
};

enum {                                   /* enumeration for database type */
  TCDBTHASH,                             /* hash table */
  TCDBTBTREE,                            /* B+ tree */
  TCDBTFIXED,                            /* fixed-length */
  TCDBTTABLE                             /* table */
};


/* Get the message string corresponding to an error code.
   `ecode' specifies the error code.
   The return value is the message string of the error code. */
const char *tcerrmsg(int ecode);


/* Show error message on the standard error output and exit.
   `message' specifies an error message.
   This function does not return. */
void *tcmyfatal(const char *message);


/* Allocate a large nullified region.
   `size' specifies the size of the region.
   The return value is the pointer to the allocated nullified region.
   This function handles failure of memory allocation implicitly.  The region of the return value
   should be released with the function `tczerounmap' when it is no longer in use. */
void *tczeromap(uint64_t size);


/* Free a large nullfied region.
   `ptr' specifies the pointer to the region. */
void tczerounmap(void *ptr);


/* Lock the global mutex object.
   If successful, the return value is true, else, it is false. */
bool tcglobalmutexlock(void);


/* Lock the global mutex object by shared locking.
   If successful, the return value is true, else, it is false. */
bool tcglobalmutexlockshared(void);


/* Unlock the global mutex object.
   If successful, the return value is true, else, it is false. */
bool tcglobalmutexunlock(void);


/* Lock the absolute path of a file.
   `path' specifies the path of the file.
   If successful, the return value is true, else, it is false. */
bool tcpathlock(const char *path);


/* Unock the absolute path of a file.
   `path' specifies the path of the file.
   If successful, the return value is true, else, it is false. */
bool tcpathunlock(const char *path);


/* Convert an integer to the string as binary numbers.
   `num' specifies the integer.
   `buf' specifies the pointer to the region into which the result string is written.  The size
   of the buffer should be equal to or more than 65 bytes.
   `col' specifies the number of columns.  If it is not more than 0, it depends on the integer.
   `fc' specifies the filling character.
   The return value is the length of the result string. */
int tcnumtostrbin(uint64_t num, char *buf, int col, int fc);


/* Compare two keys by lexical order.
   `aptr' specifies the pointer to the region of one key.
   `asiz' specifies the size of the region of one key.
   `bptr' specifies the pointer to the region of the other key.
   `bsiz' specifies the size of the region of the other key.
   `op' is ignored.
   The return value is positive if the former is big, negative if the latter is big, 0 if both
   are equivalent. */
int tccmplexical(const char *aptr, int asiz, const char *bptr, int bsiz, void *op);


/* Compare two keys as decimal strings of real numbers.
   `aptr' specifies the pointer to the region of one key.
   `asiz' specifies the size of the region of one key.
   `bptr' specifies the pointer to the region of the other key.
   `bsiz' specifies the size of the region of the other key.
   `op' is ignored.
   The return value is positive if the former is big, negative if the latter is big, 0 if both
   are equivalent. */
int tccmpdecimal(const char *aptr, int asiz, const char *bptr, int bsiz, void *op);


/* Compare two keys as 32-bit integers in the native byte order.
   `aptr' specifies the pointer to the region of one key.
   `asiz' specifies the size of the region of one key.
   `bptr' specifies the pointer to the region of the other key.
   `bsiz' specifies the size of the region of the other key.
   `op' is ignored.
   The return value is positive if the former is big, negative if the latter is big, 0 if both
   are equivalent. */
int tccmpint32(const char *aptr, int asiz, const char *bptr, int bsiz, void *op);


/* Compare two keys as 64-bit integers in the native byte order.
   `aptr' specifies the pointer to the region of one key.
   `asiz' specifies the size of the region of one key.
   `bptr' specifies the pointer to the region of the other key.
   `bsiz' specifies the size of the region of the other key.
   `op' is ignored.
   The return value is positive if the former is big, negative if the latter is big, 0 if both
   are equivalent. */
int tccmpint64(const char *aptr, int asiz, const char *bptr, int bsiz, void *op);


/* Encode a serial object with BWT encoding.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   `idxp' specifies the pointer to the variable into which the index of the original string in
   the rotation array is assigned.
   The return value is the pointer to the result object.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when it
   is no longer in use. */
char *tcbwtencode(const char *ptr, int size, int *idxp);


/* Decode a serial object encoded with BWT encoding.
   `ptr' specifies the pointer to the region.
   `size' specifies the size of the region.
   `idx' specifies the index of the original string in the rotation array is assigned.
   The return value is the pointer to the result object.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when it
   is no longer in use. */
char *tcbwtdecode(const char *ptr, int size, int idx);


/* Get the binary logarithm of an integer.
   `num' specifies an integer.
   The return value is the binary logarithm. */
long tclog2l(long num);


/* Get the binary logarithm of a real number.
   `num' specifies a real number.
   The return value is the binary logarithm. */
double tclog2d(double num);


/* Get the aligned offset of a file offset.
   `off' specifies the file offset.
   The return value is the aligned offset. */
uint64_t tcpagealign(uint64_t off);


/* Print debug information with a formatted string as with `printf'. */
#if __STDC_VERSION__ >= 199901L
#define TCDPRINTF(...) \
  do { \
    fprintf(stderr, "%s:%d:%s: ", __FILE__, __LINE__, __func__); \
    fprintf(stderr, __VA_ARGS__); \
    fprintf(stderr, "\n"); \
  } while(false);
#else
#define TCDPRINTF(TC_str) \
  do { \
    fprintf(stderr, "%s:%d:%s: %s\n", __FILE__, __LINE__, __func__, TC_str); \
  } while(false);
#endif


/* Print hexadecimal pattern of a binary region. */
#define TCPRINTHEX(TC_ptr, TC_size) \
  do { \
    for(int TC_i = 0; TC_i < (TC_size); TC_i++){ \
      if(TC_i > 0) putchar(' '); \
      printf("%02X", ((unsigned char *)(TC_ptr))[TC_i]); \
    } \
    putchar('\n'); \
  } while(false);


/* Print an extensible string object. */
#define TCPRINTXSTR(TC_xstr) \
  do { \
    fwrite(tcxstrptr((TC_xstr)), tcxstrsize((TC_xstr)), 1, stdout); \
    putchar('\n'); \
  } while(false);


/* Print all elements of a list object. */
#define TCPRINTLIST(TC_list) \
  do { \
    for(int TC_i = 0; TC_i < tclistnum((TC_list)); TC_i++){ \
      int TC_size; \
      const char *TC_ptr = tclistval((TC_list), TC_i, &TC_size); \
      printf("%p\t", (void *)(TC_list)); \
      fwrite(TC_ptr, TC_size, 1, stdout); \
      putchar('\n'); \
    } \
    putchar('\n'); \
  } while(false);


/* Print all records of a list object. */
#define TCPRINTMAP(TC_map) \
  do { \
    TCLIST *TC_keys = tcmapkeys((TC_map)); \
    for(int TC_i = 0; TC_i < tclistnum(TC_keys); TC_i++){ \
      int TC_ksiz; \
      const char *TC_kbuf = tclistval(TC_keys, TC_i, &TC_ksiz); \
      int TC_vsiz; \
      const char *TC_vbuf = tcmapget((TC_map), TC_kbuf, TC_ksiz, &TC_vsiz); \
      printf("%p\t", (void *)(TC_map)); \
      fwrite(TC_kbuf, TC_ksiz, 1, stdout); \
      putchar('\t'); \
      fwrite(TC_vbuf, TC_vsiz, 1, stdout); \
      putchar('\n'); \
    } \
    putchar('\n'); \
    tclistdel(TC_keys); \
  } while(false);


/* Alias of `tcmalloc'. */
#if defined(_MYFASTEST)
#define TCMALLOC(TC_res, TC_size) \
  do { \
    (TC_res) = MYMALLOC(TC_size); \
  } while(false)
#else
#define TCMALLOC(TC_res, TC_size) \
  do { \
    if(!((TC_res) = MYMALLOC(TC_size))) tcmyfatal("out of memory"); \
  } while(false)
#endif


/* Alias of `tccalloc'. */
#if defined(_MYFASTEST)
#define TCCALLOC(TC_res, TC_nmemb, TC_size) \
  do { \
    (TC_res) = MYCALLOC((TC_nmemb), (TC_size)); \
  } while(false)
#else
#define TCCALLOC(TC_res, TC_nmemb, TC_size) \
  do { \
    if(!((TC_res) = MYCALLOC((TC_nmemb), (TC_size)))) tcmyfatal("out of memory"); \
  } while(false)
#endif


/* Alias of `tcrealloc'. */
#if defined(_MYFASTEST)
#define TCREALLOC(TC_res, TC_ptr, TC_size) \
  do { \
    (TC_res) = MYREALLOC((TC_ptr), (TC_size)); \
  } while(false)
#else
#define TCREALLOC(TC_res, TC_ptr, TC_size) \
  do { \
    if(!((TC_res) = MYREALLOC((TC_ptr), (TC_size)))) tcmyfatal("out of memory"); \
  } while(false)
#endif


/* Alias of `tcmemdup'. */
#define TCMEMDUP(TC_res, TC_ptr, TC_size) \
  do { \
    TCMALLOC((TC_res), (TC_size) + 1); \
    memcpy((TC_res), (TC_ptr), (TC_size)); \
    (TC_res)[TC_size] = '\0'; \
  } while(false)


/* Alias of `tcfree'. */
#define TCFREE(TC_ptr) \
  do { \
    MYFREE(TC_ptr); \
  } while(false)


/* Get the alignment of a variable type. */
#define TCALIGNOF(TC_a) \
  ((int)offsetof(struct { int8_t TC_top; TC_a TC_bot; }, TC_bot))


/* Get the size of padding bytes for pointer alignment. */
typedef union { int32_t i; int64_t l; double d; void *p; TCCMP f; } tcgeneric_t;
#define TCALIGNPAD(TC_hsiz) \
  (((TC_hsiz | ~-TCALIGNOF(tcgeneric_t)) + 1) - TC_hsiz)


/* Alias of `tcxstrcat'. */
#define TCXSTRCAT(TC_xstr, TC_ptr, TC_size) \
  do { \
    int TC_mysize = (TC_size); \
    int TC_nsize = (TC_xstr)->size + TC_mysize + 1; \
    if((TC_xstr)->asize < TC_nsize){ \
      while((TC_xstr)->asize < TC_nsize){ \
        (TC_xstr)->asize *= 2; \
        if((TC_xstr)->asize < TC_nsize) (TC_xstr)->asize = TC_nsize; \
      } \
      TCREALLOC((TC_xstr)->ptr, (TC_xstr)->ptr, (TC_xstr)->asize); \
    } \
    memcpy((TC_xstr)->ptr + (TC_xstr)->size, (TC_ptr), TC_mysize); \
    (TC_xstr)->size += TC_mysize; \
    (TC_xstr)->ptr[(TC_xstr)->size] = '\0'; \
  } while(false)


/* Alias of `tcxstrptr'. */
#define TCXSTRPTR(TC_xstr) \
  ((TC_xstr)->ptr)


/* Alias of `tcxstrsize'. */
#define TCXSTRSIZE(TC_xstr) \
  ((TC_xstr)->size)


/* Alias of `tclistnum'. */
#define TCLISTNUM(TC_list) \
  ((TC_list)->num)


/* Alias of `tclistval' but not checking size. */
#define TCLISTVAL(TC_ptr, TC_list, TC_index, TC_size) \
  do { \
    (TC_ptr) = (TC_list)->array[(TC_index)+(TC_list)->start].ptr; \
    (TC_size) = (TC_list)->array[(TC_index)+(TC_list)->start].size; \
  } while(false)


/* Alias of `tclistval' but not checking size and not using the third parameter. */
#define TCLISTVALPTR(TC_list, TC_index) \
  ((void *)((TC_list)->array[(TC_index)+(TC_list)->start].ptr))


/* Alias of `tclistval' but not checking size and returning the size of the value. */
#define TCLISTVALSIZ(TC_list, TC_index) \
  ((TC_list)->array[(TC_index)+(TC_list)->start].size)


/* Alias of `tclistpush'. */
#define TCLISTPUSH(TC_list, TC_ptr, TC_size) \
  do { \
    int TC_mysize = (TC_size); \
    int TC_index = (TC_list)->start + (TC_list)->num; \
    if(TC_index >= (TC_list)->anum){ \
      (TC_list)->anum += (TC_list)->num + 1; \
      TCREALLOC((TC_list)->array, (TC_list)->array, \
                (TC_list)->anum * sizeof((TC_list)->array[0])); \
    } \
    TCLISTDATUM *array = (TC_list)->array; \
    TCMALLOC(array[TC_index].ptr, TC_mysize + 1);     \
    memcpy(array[TC_index].ptr, (TC_ptr), TC_mysize); \
    array[TC_index].ptr[TC_mysize] = '\0'; \
    array[TC_index].size = TC_mysize; \
    (TC_list)->num++; \
  } while(false)


/* Alias of `tclistinsert'. */
#define TCLISTINSERT(TC_list, TC_index, TC_ptr, TC_size) \
  do { \
    int TC_myindex = (TC_index); \
    TC_myindex += (TC_list)->start; \
    if((TC_list)->start + (TC_list)->num >= (TC_list)->anum){ \
      (TC_list)->anum += (TC_list)->num + 1; \
      TCREALLOC((TC_list)->array, (TC_list)->array, \
                (TC_list)->anum * sizeof((TC_list)->array[0])); \
    } \
    memmove((TC_list)->array + TC_myindex + 1, (TC_list)->array + TC_myindex, \
            sizeof((TC_list)->array[0]) * ((TC_list)->start + (TC_list)->num - TC_myindex)); \
    TCMALLOC((TC_list)->array[TC_myindex].ptr, (TC_size) + 1); \
    memcpy((TC_list)->array[TC_myindex].ptr, (TC_ptr), (TC_size)); \
    (TC_list)->array[TC_myindex].ptr[(TC_size)] = '\0'; \
    (TC_list)->array[TC_myindex].size = (TC_size); \
    (TC_list)->num++; \
  } while(false)


/* Truncate a list object. */
#define TCLISTTRUNC(TC_list, TC_num) \
  do { \
    while((TC_list)->num > (TC_num)){ \
      TCFREE((TC_list)->array[--(TC_list)->num].ptr); \
    } \
  } while(false)


/* Alias of `tcmaprnum'. */
#define TCMAPRNUM(TC_map) \
  ((TC_map)->rnum)


/* Alias of `tcptrlistnum'. */
#define TCPTRLISTNUM(TC_ptrlist) \
  ((TC_ptrlist)->num)


/* Alias of `tcptrlistval'. */
#define TCPTRLISTVAL(TC_ptrlist, TC_index) \
  ((void *)((TC_ptrlist)->array[(TC_index)+(TC_ptrlist)->start]))


/* Alias of `tcptrlistpush'. */
#define TCPTRLISTPUSH(TC_ptrlist, TC_ptr) \
  do { \
    int TC_index = (TC_ptrlist)->start + (TC_ptrlist)->num; \
    if(TC_index >= (TC_ptrlist)->anum){ \
      (TC_ptrlist)->anum += (TC_ptrlist)->num + 1; \
      TCREALLOC((TC_ptrlist)->array, (TC_ptrlist)->array, \
                (TC_ptrlist)->anum * sizeof((TC_ptrlist)->array[0])); \
    } \
    (TC_ptrlist)->array[TC_index] = (TC_ptr); \
    (TC_ptrlist)->num++; \
  } while(false)


/* Alias of `tcptrlistinsert'. */
#define TCPTRLISTINSERT(TC_ptrlist, TC_index, TC_ptr) \
  do { \
    int TC_myindex = (TC_index); \
    TC_myindex += (TC_ptrlist)->start; \
    if((TC_ptrlist)->start + (TC_ptrlist)->num >= (TC_ptrlist)->anum){ \
      (TC_ptrlist)->anum += (TC_ptrlist)->num + 1; \
      TCREALLOC((TC_ptrlist)->array, (TC_ptrlist)->array, \
                (TC_ptrlist)->anum * sizeof((TC_ptrlist)->array[0])); \
    } \
    memmove((TC_ptrlist)->array + TC_myindex + 1, (TC_ptrlist)->array + TC_myindex, \
            sizeof((TC_ptrlist)->array[0]) * ((TC_ptrlist)->start + \
                                              (TC_ptrlist)->num - TC_myindex)); \
    (TC_ptrlist)->array[TC_myindex] = (TC_ptr); \
    (TC_ptrlist)->num++; \
  } while(false)


/* Truncate a pointer list object. */
#define TCPTRLISTTRUNC(TC_ptrlist, TC_num) \
  do { \
    (TC_ptrlist)->num = (TC_num); \
  } while(false)


/* tricks for backward compatibility */
#define BDBCMP            TCCMP
#define tcbdbrange3       tcbdbfwmkeys2
#define tcbdbcmplexical   tccmplexical
#define tcbdbcmpdecimal   tccmpdecimal
#define tcbdbcmpint32     tccmpint32
#define tcbdbcmpint64     tccmpint64
#define tctdbqryprocout   tctdbqrysearchout
#define tctdbqrysetmax(TC_tdb, TC_max) \
  tctdbqrysetlimit((TC_tdb), (TC_max), 0)



__TCUTIL_CLINKAGEEND
#endif                                   /* duplication check */


/* END OF FILE */
