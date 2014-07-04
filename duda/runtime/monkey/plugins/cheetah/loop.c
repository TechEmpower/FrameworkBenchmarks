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
#include <stdlib.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

/* Monkey Plugin Interface */
#include "MKPlugin.h"
#include "cheetah.h"
#include "cutils.h"
#include "cmd.h"
#include "loop.h"

void mk_cheetah_loop_stdin()
{
    int len;
    char cmd[200];
    char line[200];
    char *rcmd;

    mk_cheetah_welcome_msg();

    while (1) {
        CHEETAH_WRITE(MK_CHEETAH_PROMPT, ANSI_BOLD, ANSI_GREEN, ANSI_RESET);

        rcmd = fgets(line, sizeof(line), cheetah_input);
        if (!rcmd) {
            continue;
        }

        len = strlen(line);

        if (len == 0){
            CHEETAH_WRITE("\n");
            mk_cheetah_cmd_quit();
        }

        strncpy(cmd, line, len - 1);
        cmd[len - 1] = '\0';

        mk_cheetah_cmd(cmd);
        memset(line, '\0', sizeof(line));
    }
}

void mk_cheetah_loop_server()
{
    int n, ret;
    int buf_len;
    unsigned long len;
    char buf[1024];
    char cmd[1024];
    int server_fd;
    int remote_fd;
    size_t address_length;
    struct sockaddr_un address;
    socklen_t socket_size = sizeof(struct sockaddr_in);

    /* Create listening socket */
    server_fd = socket(PF_UNIX, SOCK_STREAM, 0);
    if (server_fd < 0) {
        perror("socket() failed");
        exit(EXIT_FAILURE);
    }

    cheetah_server = NULL;
    mk_api->str_build(&cheetah_server, &len, "/tmp/cheetah.%i",
                      mk_api->config->serverport);

    unlink(cheetah_server);

    address.sun_family = AF_UNIX;
    sprintf(address.sun_path, "%s", cheetah_server);
    address_length = sizeof(address.sun_family) + len;

    if(bind(server_fd, (struct sockaddr *) &address, address_length) != 0) {
        perror("bind");
        exit(EXIT_FAILURE);
    }

    if(listen(server_fd, 5) != 0) {
        perror("listen");
        exit(EXIT_FAILURE);
    }

    while(1) {
        /* Listen for incoming connections */
        remote_fd = accept(server_fd, (struct sockaddr *) &address, &socket_size);
        cheetah_socket = remote_fd;

        buf_len = 0;
        memset(buf, '\0', 1024);

        /* Send welcome message and prompt */
        mk_cheetah_welcome_msg();
        CHEETAH_WRITE(MK_CHEETAH_PROMPT, ANSI_BOLD, ANSI_GREEN, ANSI_RESET);

        while (1) {
            /* Read incoming data */
            n = read(remote_fd, buf+buf_len, 1024 - buf_len);
            if (n <= 0) {
                break;
            }
            else {
              buf_len += n;
              if (buf[buf_len-1] == '\n') {
                  /* Filter command */
                  strncpy(cmd, buf, buf_len - 1);
                  cmd[buf_len - 1] = '\0';

                  /* Run command */
                  ret = mk_cheetah_cmd(cmd);

                  if (ret == -1) {
                      break;
                  }

                  /* Write prompt */
                  CHEETAH_WRITE(MK_CHEETAH_PROMPT, ANSI_BOLD, ANSI_GREEN, ANSI_RESET);
                  buf_len = 0;
                  memset(buf, '\0', 1024);
              }
            }
        }

        close(remote_fd);
    }
}
