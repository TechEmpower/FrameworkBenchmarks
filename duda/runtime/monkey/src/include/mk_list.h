/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Monkey HTTP Daemon
 *  ------------------
 *  Copyright (C) 2010, Jonathan Gonzalez V. <zeus@gnu.org>
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU Lesser General Public  License as published
 *  by the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 *  License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#ifndef   	MK_LIST_H_
#define   	MK_LIST_H_

#include <stddef.h>

#ifndef offsetof
#define offsetof(TYPE, MEMBER) ((size_t) &((TYPE *)0)->MEMBER)
#endif

#define container_of(ptr, type, member) ({                      \
      const typeof( ((type *)0)->member ) *__mptr = (ptr);      \
      (type *)( (char *)__mptr - offsetof(type,member) );})


struct mk_list
{
    struct mk_list *prev, *next;
};

static inline void mk_list_init(struct mk_list *list)
{
    list->next = list;
    list->prev = list;
}

static inline void __mk_list_add(struct mk_list *new, struct mk_list *prev,
                                 struct mk_list *next)
{
    next->prev = new;
    new->next = next;
    new->prev = prev;
    prev->next = new;
}

static inline void mk_list_add(struct mk_list *new, struct mk_list *head)
{
    __mk_list_add(new, head->prev, head);
}

static inline void __mk_list_del(struct mk_list *prev, struct mk_list *next)
{
    prev->next = next;
    next->prev = prev;
}

static inline void mk_list_del(struct mk_list *entry)
{
    __mk_list_del(entry->prev, entry->next);
    entry->prev = NULL;
    entry->next = NULL;
}

static inline int mk_list_is_empty(struct mk_list *head)
{
    if (head->next == head) return 0;
    else return -1;
}

#define mk_list_foreach(curr, head) for( curr = (head)->next; curr != (head); curr = curr->next )
#define mk_list_foreach_safe(curr, n, head) \
    for (curr = (head)->next, n = curr->next; curr != (head); curr = n, n = curr->next)

#define mk_list_entry( ptr, type, member ) container_of( ptr, type, member )

/*
 * First node of the list
 * ----------------------
 * Be careful with this Macro, its intended to be used when some node is already linked
 * to the list (ptr). If the list is empty it will return the list address as it points
 * to it self: list == list->prev == list->next.
 *
 * If exists some possiblity that your code handle an empty list, use mk_list_is_empty()
 * previously to check if its empty or not.
 */
#define mk_list_entry_first(ptr, type, member) container_of((ptr)->next, type, member)

/* First node of the list
 * ---------------------
 * Be careful with this Macro, its intended to be used when some node is already linked
 * to the list (ptr). If the list is empty it will return the list address as it points
 * to it self: list == list->prev == list->next.
 *
 * If exists some possiblity that your code handle an empty list, use mk_list_is_empty()
 * previously to check if its empty or not.
 */
#define mk_list_entry_last(ptr, type, member) container_of((ptr)->prev, type, member)

/* Next node */
#define mk_list_entry_next(ptr, type, member, head)                     \
    (ptr)->next == (head) ? container_of((head)->next, type, member) :  \
        container_of((ptr)->next, type, member);

#endif /* !MK_LIST_H_ */
