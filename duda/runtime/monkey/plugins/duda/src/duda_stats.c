/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  Copyright (C) 2012-2014, Eduardo Silva P. <edsiper@gmail.com>.
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

#include <string.h>

#include "duda.h"
#include "duda_stats.h"

#define drp(dr, ...)   duda_response_printf(dr, __VA_ARGS__)

static char *human_readable_size(long size)
{
    long u = 1024, i, len = 128;
    char *buf = api->mem_alloc(len);
    static const char *__units[] = { "b", "K", "M", "G",
        "T", "P", "E", "Z", "Y", NULL
    };

    for (i = 0; __units[i] != NULL; i++) {
        if ((size / u) == 0) {
            break;
        }
        u *= 1024;
    }
    if (!i) {
        snprintf(buf, len, "%ld %s", size, __units[0]);
    }
    else {
        float fsize = (float) ((double) size / (u / 1024));
        snprintf(buf, len, "%.2f%s", fsize, __units[i]);
    }

    return buf;
}

void duda_stats_cb(duda_request_t *dr)
{
    char *hr;
    uint64_t total;
    uint64_t total_all = 0;
    struct mk_list *head;
    struct duda_stats_worker *st;

    duda_response_http_status(dr, 200);
    duda_response_http_header(dr, "Content-Type: text/html");

    /* Header */
    duda_response_printf(dr, DD_HTML_HEADER, "Console Stats", DD_HTML_CSS);
    duda_response_printf(dr, DD_HTML_NAVBAR_BASIC, "console stats");

    duda_response_printf(dr,
                         "<div class=\"container\">"
                         "  <div class=\"duda-template\">"
                         "  <h1>%s/</h1>\n"
                         "  <address>\n"
                         "    Statistics for <strong>%s</strong> web service<br>\n"
                         "  </address>\n",
                         dr->ws_root->name.data,
                         dr->ws_root->name.data);

    /* <ul> */
    duda_response_printf(dr, "<hr>\n");
    duda_response_printf(dr, "<h2>Memory Usage</h2>\n");
    duda_response_printf(dr, "<p class=\"muted\">\n"
                             "   the following section list every built-in or custom"
                             "   worker ID and it's actual memory usage.\n"
                             "</p>\n");


    duda_response_printf(dr,
                         "<table class=\"table table-bordered table-hover table-condensed\">\n"
                         "<thead>\n"
                         "<tr>\n"
                         "  <th>Task ID</th>\n"
                         "  <th>Memory <small>(bytes)</small></th>\n"
                         "  <th>Memory <small>(Human Readable)</small></th>\n"
                         "</tr>\n"
                         "</thead>\n"
                         "<tbody>\n");


    mk_list_foreach(head, &duda_stats.mem) {
        st = mk_list_entry(head, struct duda_stats_worker, _head);

        total = (*st->mem_allocated - *st->mem_deallocated);
        hr    = human_readable_size(total);

        duda_response_printf(dr,
                             "    <tr>\n"
                             "        <td>%lu</td>\n"
                             "        <td>%lu</td>\n"
                             "        <td>%s</td>\n"
                             "    </tr>\n",
                             st->task_id, total, hr
                             );
        total_all += total;
        api->mem_free(hr);
    }

    hr    = human_readable_size(total_all);
    duda_response_printf(dr,
                         "    <tr class=\"success\">\n"
                         "        <td>total</td>\n"
                         "        <td>%lu</td>\n"
                         "        <td>%s</td>\n"
                         "    </tr>\n",
                         total_all,
                         hr
                         );
    api->mem_free(hr);
    duda_response_printf(dr, "</tbody></table>\n\n");
    duda_response_end(dr, NULL);
}

void duda_stats_txt_cb(duda_request_t *dr)
{
    char *hr;
    uint64_t total;
    struct mk_list *head;
    struct duda_stats_worker *st;

    duda_response_http_status(dr, 200);
    duda_response_http_header(dr, "Content-Type: text/plain");

    drp(dr, "Duda I/O Statistics\n===================\n\n");
    drp(dr,
        "+--------------------------------------------+\n"
        "|          Workers Memory Usage              |\n"
        "+-----------+-----------------+--------------+\n"
        "| task ID   | mem bytes       | mem unit     |\n"
        "+-----------+-----------------+--------------+\n");

    mk_list_foreach(head, &duda_stats.mem) {
        st = mk_list_entry(head, struct duda_stats_worker, _head);

        total = (*st->mem_allocated - *st->mem_deallocated);
        hr    = human_readable_size(total);
        drp(dr, "| %-9lu | %-15lu | %-12s |\n", st->task_id, total, hr);

        /* current memory in use */
        //drp(dr, "   memory usage: %lu (%s)\n", total, hr);
        api->mem_free(hr);
    }

    drp(dr, "+-----------+-----------------+--------------+\n");
    duda_response_end(dr, NULL);
}

/*
 * This function is invoked by each worker of the stack on initialization,
 * mostly to set their own data and pointer access to memory statistics.
 */
int duda_stats_worker_init()
{
    size_t sz;
    struct duda_stats_worker *st;

    st = api->mem_alloc(sizeof(struct duda_stats_worker));
    st->task_id     = syscall(__NR_gettid);
    st->worker_name = NULL;

    /* Protect this section as it needs to be atomic */
    pthread_mutex_lock(&duda_mutex_stats);

    sz = sizeof(st->mem_allocated);

    /* Get pointers to memory counters */
    je_mallctl("thread.allocatedp", &st->mem_allocated, &sz, NULL, 0);
    je_mallctl("thread.deallocatedp", &st->mem_deallocated, &sz, NULL, 0);

    /* Link the worker info into the global list */
    mk_list_add(&st->_head, &duda_stats.mem);

    pthread_mutex_unlock(&duda_mutex_stats);

    return 0;
}

int duda_stats_init()
{
    /* Initialize global statistics */
    memset(&duda_stats, '\0', sizeof(struct duda_statistics));
    mk_list_init(&duda_stats.mem);

    /* Set mutex for stats initialization through workers */
    pthread_mutex_init(&duda_mutex_stats, (pthread_mutexattr_t *) NULL);
    return 0;
}
