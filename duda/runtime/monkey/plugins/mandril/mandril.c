/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Monkey HTTP Daemon
 *  ------------------
 *  Copyright (C) 2001-2012, Eduardo Silva P. <edsiper@gmail.com>
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *  MA 02110-1301  USA
 */

#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

/* network */
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

/* Monkey API */
#include "MKPlugin.h"
#include "mandril.h"

MONKEY_PLUGIN("mandril",  /* shortname */
              "Mandril",  /* name */
              VERSION,    /* version */
              MK_PLUGIN_STAGE_10 | MK_PLUGIN_STAGE_30); /* hooks */

static struct mk_config *conf;

/* Read database configuration parameters */
static int mk_security_conf(char *confdir)
{
    int n;
    int ret = 0;
    unsigned long len;
    char *conf_path = NULL;
    char *_net, *_mask;

    struct mk_secure_ip_t *new_ip;
    struct mk_secure_url_t *new_url;
    struct mk_secure_deny_hotlink_t *new_deny_hotlink;

    struct mk_config_section *section;
    struct mk_config_entry *entry;
    struct mk_list *head;

    /* Read configuration */
    mk_api->str_build(&conf_path, &len, "%s/mandril.conf", confdir);
    conf = mk_api->config_create(conf_path);
    section = mk_api->config_section_get(conf, "RULES");


    mk_list_foreach(head, &section->entries) {
        entry = mk_list_entry(head, struct mk_config_entry, _head);

        /* Passing to internal struct */
        if (strcasecmp(entry->key, "IP") == 0) {
            new_ip = mk_api->mem_alloc(sizeof(struct mk_secure_ip_t));
            n = mk_api->str_search(entry->val, "/", 1);

            /* subnet */
            if (n > 0) {
                /* split network addr and netmask */
                _net  = mk_api->str_copy_substr(entry->val, 0, n);
                _mask = mk_api->str_copy_substr(entry->val,
                                                n + 1,
                                                strlen(entry->val));

                /* validations... */
                if (!_net ||  !_mask) {
                    mk_warn("Mandril: cannot parse entry '%s' in RULES section",
                            entry->val);
                    goto ip_next;
                }

                mk_info("network: '%s' mask: '%s'", _net, _mask);

                /* convert ip string to network address */
                if (inet_aton(_net, &new_ip->ip) == 0) {
                    mk_warn("Mandril: invalid ip address '%s' in RULES section",
                            entry->val);
                    goto ip_next;
                }

                /* parse mask */
                new_ip->netmask = strtol(_mask, (char **) NULL, 10);
                if (new_ip->netmask <= 0 || new_ip->netmask >= 32) {
                    mk_warn("Mandril: invalid mask value '%s' in RULES section",
                            entry->val);
                    goto ip_next;
                }

                /* complete struct data */
                new_ip->is_subnet = MK_TRUE;
                new_ip->network = MK_NET_NETWORK(new_ip->ip.s_addr, new_ip->netmask);
                new_ip->hostmin = MK_NET_HOSTMIN(new_ip->ip.s_addr, new_ip->netmask);
                new_ip->hostmax = MK_NET_HOSTMAX(new_ip->ip.s_addr, new_ip->netmask);

                /* link node with main list */
                mk_list_add(&new_ip->_head, &mk_secure_ip);

            /*
             * I know, you were instructed to hate 'goto' statements!, ok, show this
             * code to your teacher and let him blame :P
             */
            ip_next:
                if (_net) {
                    mk_api->mem_free(_net);
                }
                if (_mask) {
                    mk_api->mem_free(_mask);
                }
            }
            else { /* normal IP address */

                /* convert ip string to network address */
                if (inet_aton(entry->val, &new_ip->ip) == 0) {
                    mk_warn("Mandril: invalid ip address '%s' in RULES section",
                            entry->val);
                }
                else {
                    new_ip->is_subnet = MK_FALSE;
                    mk_list_add(&new_ip->_head, &mk_secure_ip);
                }
            }
        }
        else if (strcasecmp(entry->key, "URL") == 0) {
            /* simple allcotion and data association */
            new_url = mk_api->mem_alloc(sizeof(struct mk_secure_url_t));
            new_url->criteria = entry->val;

            /* link node with main list */
            mk_list_add(&new_url->_head, &mk_secure_url);
        }
        else if (strcasecmp(entry->key, "deny_hotlink") == 0) {
            new_deny_hotlink = mk_api->mem_alloc(sizeof(*new_deny_hotlink));
            new_deny_hotlink->criteria = entry->val;

            mk_list_add(&new_deny_hotlink->_head, &mk_secure_deny_hotlink);
        }
    }

    mk_api->mem_free(conf_path);
    return ret;
}

static int mk_security_check_ip(int socket)
{
    int network;
    struct mk_secure_ip_t *entry;
    struct mk_list *head;
    struct in_addr addr_t, *addr = &addr_t;
    socklen_t len = sizeof(addr);

    if (getpeername(socket, (struct sockaddr *)&addr_t, &len) < 0) {
        return -1;
    }

    PLUGIN_TRACE("[FD %i] Mandril validating IP address", socket);
    mk_list_foreach(head, &mk_secure_ip) {
        entry = mk_list_entry(head, struct mk_secure_ip_t, _head);

        if (entry->is_subnet == MK_TRUE) {
            /* Validate network */
            network = MK_NET_NETWORK(addr->s_addr, entry->netmask);
            if (network != entry->network) {
                continue;
            }

            /* Validate host range */
            if (addr->s_addr <= entry->hostmax && addr->s_addr >= entry->hostmin) {
                PLUGIN_TRACE("[FD %i] Mandril closing by rule in ranges", socket);
                return -1;
            }
        }
        else {
            if (addr->s_addr == entry->ip.s_addr) {
                PLUGIN_TRACE("[FD %i] Mandril closing by rule in IP match", socket);
                return -1;
            }
        }
    }
    return 0;
}

/* Check if the incoming URL is restricted for some rule */
static int mk_security_check_url(mk_pointer url)
{
    int n;
    struct mk_list *head;
    struct mk_secure_url_t *entry;

    mk_list_foreach(head, &mk_secure_url) {
        entry = mk_list_entry(head, struct mk_secure_url_t, _head);
        n = mk_api->str_search_n(url.data, entry->criteria, MK_STR_INSENSITIVE, url.len);
        if (n >= 0) {
            return -1;
        }
    }

    return 0;
}

mk_pointer parse_referer_host(mk_pointer ref)
{
    unsigned int i, beginHost, endHost;
    mk_pointer host;

    host.data = NULL;
    host.len = 0;

    // Find end of "protocol://"
    for (i = 0; i < ref.len && !(ref.data[i] == '/' && ref.data[i+1] == '/'); i++);
    if (i == ref.len) {
        goto error;
    }
    beginHost = i + 2;

    // Find end of any "user:password@"
    for (; i < ref.len && ref.data[i] != '@'; i++);
    if (i < ref.len) {
        beginHost = i + 1;
    }

    // Find end of "host", (beginning of :port or /path)
    for (i = beginHost; i < ref.len && ref.data[i] != ':' && ref.data[i] != '/'; i++);
    endHost = i;

    host.data = ref.data + beginHost;
    host.len = endHost - beginHost;
    return host;
error:
    host.data = NULL;
    host.len = 0;
    return host;
}

static int mk_security_check_hotlink(mk_pointer url, mk_pointer host,
        mk_pointer referer)
{
    mk_pointer ref_host = parse_referer_host(referer);
    unsigned int domains_matched = 0;
    int i = 0;
    const char *curA, *curB;
    struct mk_list *head;
    struct mk_secure_deny_hotlink_t *entry;

    if (ref_host.data == NULL) {
        return 0;
    }
    else if (host.data == NULL) {
        mk_err("No host data.");
        return -1;
    }

    mk_list_foreach(head, &mk_secure_url) {
        entry = mk_list_entry(head, struct mk_secure_deny_hotlink_t, _head);
        i = mk_api->str_search_n(url.data, entry->criteria, MK_STR_INSENSITIVE, url.len);
        if (i >= 0) {
            break;
        }
    }
    if (i < 0) {
        return 0;
    }

    curA = host.data + host.len;
    curB = ref_host.data + ref_host.len;

    // Match backwards from root domain.
    while (curA > host.data && curB > ref_host.data) {
        i++;
        curA--;
        curB--;

        if ((*curA == '.' && *curB == '.') ||
                curA == host.data || curB == ref_host.data) {
            if (i < 1) {
                break;
            }
            else if (curA == host.data &&
                    !(curB == ref_host.data || *(curB - 1) == '.')) {
                break;
            }
            else if (curB == ref_host.data &&
                    !(curA == host.data || *(curA - 1) == '.')) {
                break;
            }
            else if (strncasecmp(curA, curB, i)) {
                break;
            }
            domains_matched += 1;
            i = 0;
        }
    }

    // Block connection if none or only top domain matched.
    return domains_matched >= 2 ? 0 : -1;
}

int _mkp_init(struct plugin_api **api, char *confdir)
{
    mk_api = *api;

    /* Init security lists */
    mk_list_init(&mk_secure_ip);
    mk_list_init(&mk_secure_url);
    mk_list_init(&mk_secure_deny_hotlink);

    /* Read configuration */
    mk_security_conf(confdir);
    return 0;
}

void _mkp_exit()
{
}

int _mkp_stage_10(unsigned int socket, struct sched_connection *conx)
{
    (void) conx;

    /* Validate ip address with Mandril rules */
    if (mk_security_check_ip(socket) != 0) {
        PLUGIN_TRACE("[FD %i] Mandril close connection", socket);
        return MK_PLUGIN_RET_CLOSE_CONX;
    }
    return MK_PLUGIN_RET_CONTINUE;
}

int _mkp_stage_30(struct plugin *p,
        struct client_session *cs,
        struct session_request *sr)
{
    mk_pointer referer;
    (void) p;
    (void) cs;

    PLUGIN_TRACE("[FD %i] Mandril validating URL", cs->socket);
    if (mk_security_check_url(sr->uri) < 0) {
        PLUGIN_TRACE("[FD %i] Close connection, blocked URL", cs->socket);
        mk_api->header_set_http_status(sr, MK_CLIENT_FORBIDDEN);
        return MK_PLUGIN_RET_CLOSE_CONX;
    }

    PLUGIN_TRACE("[FD %d] Mandril validating hotlinking", cs->socket);
    referer = mk_api->header_get(&sr->headers_toc, "Referer", strlen("Referer"));
    if (mk_security_check_hotlink(sr->uri_processed, sr->host, referer) < 0) {
        PLUGIN_TRACE("[FD %i] Close connection, deny hotlinking.", cs->socket);
        mk_api->header_set_http_status(sr, MK_CLIENT_FORBIDDEN);
        return MK_PLUGIN_RET_CLOSE_CONX;
    }

    return MK_PLUGIN_RET_NOT_ME;
}
