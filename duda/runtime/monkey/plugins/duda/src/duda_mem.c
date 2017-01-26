/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  Copyright (C) 2012-2014, Eduardo Silva P. <edsiper@gmail.com>
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

#include "duda.h"
#include "duda_mem.h"

/*
 * @OBJ_NAME: mem
 * @OBJ_MENU: Memory Handler
 * @OBJ_DESC: The Memory Handler object provides common interfaces to manipulate
 * dynamic memory inside the services. As the stack supports different memory
 * allocators, the usage of these interfaces is mandatory.
 */

struct duda_api_mem *duda_mem_object()
{
  struct duda_api_mem *obj;

  obj = mk_api->mem_alloc(sizeof(struct duda_api_mem));
  obj->alloc   = mk_mem_malloc;
  obj->alloc_z = mk_mem_malloc_z;
  obj->realloc = mk_mem_realloc;
  obj->free    = mk_mem_free;

  return obj;
}


/*
 * @METHOD_NAME: alloc
 * @METHOD_DESC: It allocates a fixed number of bytes and returns a pointer to the
 * allocated memory.
 * @METHOD_PROTO: void *alloc(size_t size)
 * @METHOD_PARAM: size the number of bytes to allocate.
 * @METHOD_RETURN: On success it returns a pointer to the allocated memory, on error
 * it returns NULL.
 */

/*
 * @METHOD_NAME: alloc_z
 * @METHOD_DESC: Similar behavior than alloc() method, but on this call the allocated
 * memory is zeroed.
 * @METHOD_PROTO: void *alloc_z(size_t size)
 * @METHOD_PARAM: size the number of bytes to allocate.
 * @METHOD_RETURN: On success it returns zero, on error -1.
 */

/*
 * @METHOD_NAME: realloc
 * @METHOD_DESC: Resize an allocated memory.
 * @METHOD_PROTO: void *realloc(void *ptr, size_t size)
 * @METHOD_PARAM: ptr pointer to original memory address to resize.
 * @METHOD_PARAM: size specify the new size.
 * @METHOD_RETURN: On success it returns a pointer to the resized memory, note
 * than the returned pointer could be different from the original ptr, after
 * use realloc() is suggested to validate if both pointers are equal or not. On error
 * it returns NULL.
 */

/*
 * @METHOD_NAME: free
 * @METHOD_DESC: This method frees the memory space pointed to by ptr.
 * @METHOD_PROTO: void free(void *ptr)
 * @METHOD_PARAM: ptr pointer to memory address.
 * @METHOD_RETURN: This method do not return any value.
 */
