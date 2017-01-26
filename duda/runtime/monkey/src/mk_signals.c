/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Monkey HTTP Daemon
 *  ------------------
 *  Copyright (C) 2001-2014, Eduardo Silva P. <edsiper@gmail.com>
 *  Copyright (C) 2010 Davidlohr Bueso <dave@gnu.org>
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

/* If a library, do not interfere with the app's signals */
#ifndef SHAREDLIB

#define _GNU_SOURCE

#include <features.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "monkey.h"
#include "mk_signals.h"
#include "mk_clock.h"
#include "mk_plugin.h"
#include "mk_macros.h"


/*
 * Some old uclib versions do not implment the sys_siglist, this is mostly
 * related to embedded environments with old toolchains
 */
#ifdef UCLIB_MODE
#include "contrib/uclib/sys_siglist.h"
#endif

/* when we catch a signal and want to exit we call this function
   to do it gracefully */
static void mk_signal_exit()
{
    /* ignore future signals to properly handle the cleanup */
    signal(SIGTERM, SIG_IGN);
    signal(SIGINT,  SIG_IGN);
    signal(SIGHUP,  SIG_IGN);

    mk_utils_remove_pid();
    mk_plugin_exit_all();

#ifdef SAFE_FREE
    mk_config_free_all();
#endif

    mk_info("Exiting... >:(");
    exit(EXIT_SUCCESS);
}

void mk_signal_thread_sigpipe_safe()
{
    sigset_t set, old;

    sigemptyset(&set);
    sigaddset(&set, SIGPIPE);
    pthread_sigmask(SIG_BLOCK, &set, &old);
}


static void mk_signal_handler(int signo, siginfo_t *si, void *context UNUSED_PARAM)
{
    switch (signo) {
    case SIGTERM:
    case SIGINT:
        mk_signal_exit();
        break;
    case SIGHUP:
        /*
         * TODO:
         * we should implement the httpd config reload here (not in SIGUSR2).
         * Daemon processes “overload” this signal with a mechanism to instruct them to
         * reload their configuration files. Sending SIGHUP to Apache, for example,
         * instructs it to reread httpd.conf.
         */
        mk_signal_exit();
        break;
    case SIGBUS:
    case SIGSEGV:
#ifdef DEBUG
        mk_utils_stacktrace();
#endif

        mk_err("%s (%d), code=%d, addr=%p",
               strsignal(signo), signo, si->si_code, si->si_addr);
        /* DISABLED by now: pthread_exit(NULL); */
        abort();
    default:
        /* let the kernel handle it */
        kill(getpid(), signo);
    }

}

void mk_signal_init()
{
    struct sigaction act;
    memset(&act, 0x0, sizeof(act));

    /* allow signals to be handled concurrently */
    act.sa_flags = SA_SIGINFO | SA_NODEFER;
    act.sa_sigaction = &mk_signal_handler;

    sigaction(SIGSEGV, &act, NULL);
    sigaction(SIGBUS,  &act, NULL);
    sigaction(SIGHUP,  &act, NULL);
    sigaction(SIGINT,  &act, NULL);
    sigaction(SIGTERM, &act, NULL);
}

#endif // !SHAREDLIB
