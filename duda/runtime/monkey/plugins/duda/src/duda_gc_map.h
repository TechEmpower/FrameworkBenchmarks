#ifndef DUDA_GC_MAP_H
#define DUDA_GC_MAP_H

#define DUDA_GC_ENTRIES     64
#define DUDA_GC_CHUNK       16

struct duda_gc_map {
  int used;                     /* number of used cells */
  int size;                     /* number of cells      */

  struct duda_gc_entry *cells;
};

struct duda_gc_entry {
  int   status;             /*  0 = free ; 1 = used */
  void  *p;                 /* pointer to target memory address */
};

#endif
