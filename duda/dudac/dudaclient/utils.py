# Copyright (C) 2012, Eduardo Silva <edsiper@gmail.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

import os
import sys
import time
import commands

# BUILD: Set the default branch to dst-1 (Duda Stable API Level 1)
DEFAULT_API_LEVEL = 1

# ANSI Colors
ANSI_BOLD      = "\033[1m"
ANSI_CYAN      = "\033[36m"
ANSI_MAGENTA   = "\033[35m"
ANSI_RED     = "\033[31m"
ANSI_YELLOW  = "\033[33m"
ANSI_BLUE    = "\033[34m"
ANSI_GREEN   = "\033[32m"
ANSI_WHITE   = "\033[37m"
ANSI_RESET   = "\033[0m"

MSG_TAG_INIT = ANSI_RESET + ANSI_BOLD + '['
MSG_TAG_END  = ANSI_RESET + ANSI_BOLD + ']' + ANSI_RESET
MSG_OK       = MSG_TAG_INIT + ANSI_GREEN + 'OK' + MSG_TAG_END
MSG_FAIL     = MSG_TAG_INIT + ANSI_RED + 'FAILED' + MSG_TAG_END
MSG_NEW      = '[' + ANSI_YELLOW + '+' + ANSI_RESET + ']'

# Print a failure message
def fail_msg(msg):
    print ANSI_RED + "[-] " + ANSI_RESET + msg

# Determinate if the system create coredumps with PID or not
def core_with_pid():
    f = open('/proc/sys/kernel/core_uses_pid')
    data = f.read(1)
    f.close()

    if data == '0':
        return False

    return True

def gdb_trace(command, core):
    next_highlight = False
    gdb = 'gdb --batch -ex bt --core=' + core + ' ' + command
    sys.stdout.flush()
    ret = commands.getstatusoutput(gdb)
    if ret[0] == 0:
        fail_msg('Stack Trace lookup')
        os.write(0, ANSI_YELLOW)
        for line in ret[1].split('\n'):
            if len(line) < 2:
                continue

            if line[0] == '#':
                if next_highlight is True:
                    print ANSI_BOLD + ANSI_YELLOW + '    ' + \
                        line + ANSI_RESET + ANSI_YELLOW
                    next_highlight = False
                    continue

                if line.find('<signal handler called>') > 0:
                    next_highlight = True

                print '    ' + line

        os.write(0, ANSI_RESET)

    return ret[1]

def output_pid(out):
    pid = None

    # Get Monkey PID
    for line in out.split('\n'):
        if line.find('Process ID') > 0:
            arr = line.split()
            pid = arr[-1]

    return pid

def gdb_analyze(command, out):

    pid = output_pid(out)
    if pid is None:
        return None

    # Check if a core dump file exists
    if out.find('[stack trace]') > 0 or out.find('backtrace'):
        fail_msg("Crash detected, trying to find some core dump for PID " + pid)
        if core_with_pid() is False:
            core_file = 'core'
        else:
            core_file = 'core.' + pid

        if os.path.isfile(core_file) is True:
            fail_msg('Core dump found: \'' + core_file + '\'')
            ret = gdb_trace(command, core_file)
            return ret
        else:
            fail_msg('No core dump was found:')

            # Check ulimit value
            ret = commands.getstatusoutput('ulimit -c')
            if ret[1] != 'unlimited':
                print ANSI_YELLOW + '    --'
                print ANSI_YELLOW + '    Enable core dumps with:'
                print
                print '        $ ulimit -c unlimited'
                print '    --' + ANSI_RESET

            return None

# Execute a command and print the output to stdout
def execute_stdout(header, command, head=True):
    if head is True:
        print "%s %-70s" % (MSG_NEW, header),
    else:
        print header

    sys.stdout.flush()
    os.system(command)

# Execute a command and trap the return value
def execute(header, command, status=True, crash_debug=False, head=True):
    if head is True:
        print "%s %-70s" % (MSG_NEW, header),
    else:
        print header

    sys.stdout.flush()

    ret = commands.getstatusoutput(command)
    if os.WEXITSTATUS(ret[0]) == 0:
        if status is True:
            print MSG_OK
        if ret[1].find('warning') > 0:
            print ANSI_BOLD + ANSI_RED + "--- Compiler Warnings ---" + ANSI_RESET

            lines = ret[1].split('\n')
            for l in lines:
                if l.find('warning') > 0:
                    print ANSI_GREEN + l + ANSI_RESET

            print ANSI_BOLD + ANSI_RED + "--- * --- * --- * --- * ---"
            print ANSI_RESET
    else:
        if status is True:
            print MSG_FAIL

        # The tricky part: what's the real process return status ?, according
        # to Python documentation the value or ret[0] represents the following:
        #
        # "The exit status for the command can be interpreted according to the
        #  rules for the C function wait()."
        #
        # what ?, back to C manpages:
        #
        #  This integer can be inspected with the following macros (which take
        #  the integer itself as  an  argument,  not a pointer to it, as is
        #  done in wait() and waitpid()!):
        #
        #  WIFEXITED(status)...
        #  WEXITSTATUS(status)...
        #  WIFSIGNALED(status)...
        #  WTERMSIG(status)...
        #  WCOREDUMP(status)...
        #  WIFSTOPPED(status)...
        #  WSTOPSIG(status)...
        #  WIFCONTINUED(status)...
        #
        # ok, so where are those macros on Python ??, Google -> Python WIFEXITED:
        #
        #   => os.WIFEXITED
        #
        # So everything i wanted to know was in the 'os' package, so why you tell
        # me to go to C man page ?..lovely Python...
        #

        print
        fail_msg("Command exit (status=%i): %s" % (os.WEXITSTATUS(ret[0]), command))


        status = ret[0]

        """
        print "WIFEXITED=", os.WIFEXITED(status)
        print "WEXITSTATUS=", os.WEXITSTATUS(status)
        print "WIFSIGNALED=", os.WIFSIGNALED(status)
        print "WTERMSIG=", os.WTERMSIG(status)
        print "WCOREDUMP=", os.WCOREDUMP(status)
        print "WIFSTOPPED=", os.WIFSTOPPED(status)
        print "WSTOPSIG=", os.WSTOPSIG(status)
        print "WIFCONTINUED=", os.WIFCONTINUED(status)
        """

        if os.WIFSIGNALED(ret[0]) is False:
            print ANSI_YELLOW + '-------------------------------' + ANSI_RESET
            print ret[1]
            print ANSI_YELLOW + '-------------------------------' + ANSI_RESET

        if os.WEXITSTATUS(status) < 134: # 128 base + 11 SIGSEV
            exit(1)

        crash_ret = None
        if crash_debug is True:
            crash_ret = gdb_analyze(command, ret[1])


        # Store outout to log file
        pid = output_pid(ret[1])
        if pid is not None:
            target = 'logs/crash_report.' + pid
        else:
            print ret[1]
            exit(1)

        report = time.strftime('%Y/%m/%d %H:%M:%S: ')
        report += 'Duda I/O Web Service Crash Report\n'
        report += '======================================================\n\n'
        report += '>>>>>>>>>>>>>>>> HTTP Server Output <<<<<<<<<<<<<<<<\n'
        report += '                 ^^^^^^^^^^^^^^^^^^\n\n'
        report += ret[1]

        report += '\n\n'
        report += '>>>>>>>>>>>>>>>> Stack Trace Analysis <<<<<<<<<<<<<<<<\n'
        report += '                 ^^^^^^^^^^^^^^^^^^^^\n\n'

        if crash_ret is None:
            report += 'Error: core dump file not found.\n'
        else:
            report += crash_ret
            report += '\n'

        f = open(target, 'wa+')
        f.write(report)
        f.close()

        fail_msg("Crash report saved at " + target)
        exit(1)

    return ret

def print_msg(msg, status = 0):
    print "[+] %-30s" % (msg),

    if status == 0:
        print "[FAILED]\n"
    else:
        print "[OK]"

def print_bold(msg):
    print ANSI_BOLD + msg + ANSI_RESET

def print_color(msg, color, is_bold=False):
    text = color + msg + ANSI_RESET
    if is_bold is True:
        text = ANSI_BOLD + text

    print text

def print_entry(header):
    print "%s %s" % (MSG_NEW, header)
