#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
| This file is part of the web2py Web Framework
| Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

The widget is called from web2py
----------------------------------
"""

import datetime
import sys
import cStringIO
import time
import thread
import threading
import os
import socket
import signal
import math
import logging
import newcron
import getpass
import gluon.main as main

from gluon.fileutils import read_file, write_file, create_welcome_w2p
from gluon.settings import global_settings
from gluon.shell import run, test
from gluon.utils import is_valid_ip_address, is_loopback_ip_address, getipaddrinfo


ProgramName = 'web2py Web Framework'
ProgramAuthor = 'Created by Massimo Di Pierro, Copyright 2007-' + str(
    datetime.datetime.now().year)
ProgramVersion = read_file('VERSION').strip()

ProgramInfo = '''%s
                 %s
                 %s''' % (ProgramName, ProgramAuthor, ProgramVersion)

if not sys.version[:3] in ['2.5', '2.6', '2.7']:
    msg = 'Warning: web2py requires Python 2.5, 2.6 or 2.7 but you are running:\n%s'
    msg = msg % sys.version
    sys.stderr.write(msg)

logger = logging.getLogger("web2py")


def run_system_tests(options):
    """
    Runs unittests for gluon.tests
    """
    import subprocess
    major_version = sys.version_info[0]
    minor_version = sys.version_info[1]
    if major_version == 2:
        if minor_version in (5, 6):
            sys.stderr.write("Python 2.5 or 2.6\n")
            ret = subprocess.call(['unit2', '-v', 'gluon.tests'])
        elif minor_version in (7,):
            call_args = [sys.executable, '-m', 'unittest', '-v', 'gluon.tests']
            if options.with_coverage:
                try:
                    import coverage
                    coverage_config = os.environ.get(
                        "COVERAGE_PROCESS_START",
                        os.path.join('gluon', 'tests', 'coverage.ini'))

                    call_args = ['coverage', 'run', '--rcfile=%s' %
                                 coverage_config,
                                 '-m', 'unittest', '-v', 'gluon.tests']
                except:
                    sys.stderr.write('Coverage was not installed, skipping\n')
            sys.stderr.write("Python 2.7\n")
            ret = subprocess.call(call_args)
        else:
            sys.stderr.write("unknown python 2.x version\n")
            ret = 256
    else:
        sys.stderr.write("Only Python 2.x supported.\n")
        ret = 256
    sys.exit(ret and 1)


class IO(object):
    """   """

    def __init__(self):
        """   """

        self.buffer = cStringIO.StringIO()

    def write(self, data):
        """   """

        sys.__stdout__.write(data)
        if hasattr(self, 'callback'):
            self.callback(data)
        else:
            self.buffer.write(data)


def get_url(host, path='/', proto='http', port=80):
    if ':' in host:
        host = '[%s]' % host
    else:
        host = host.replace('0.0.0.0', '127.0.0.1')
    if path.startswith('/'):
        path = path[1:]
    if proto.endswith(':'):
        proto = proto[:-1]
    if not port or port == 80:
        port = ''
    else:
        port = ':%s' % port
    return '%s://%s%s/%s' % (proto, host, port, path)


def start_browser(url, startup=False):
    if startup:
        print 'please visit:'
        print '\t', url
        print 'starting browser...'
    try:
        import webbrowser
        webbrowser.open(url)
    except:
        print 'warning: unable to detect your browser'


class web2pyDialog(object):
    """ Main window dialog """

    def __init__(self, root, options):
        """ web2pyDialog constructor  """

        import Tkinter
        import tkMessageBox

        bg_color = 'white'
        root.withdraw()

        self.root = Tkinter.Toplevel(root, bg=bg_color)
        self.root.resizable(0, 0)
        self.root.title(ProgramName)

        self.options = options
        self.scheduler_processes = {}
        self.menu = Tkinter.Menu(self.root)
        servermenu = Tkinter.Menu(self.menu, tearoff=0)
        httplog = os.path.join(self.options.folder, 'httpserver.log')
        iconphoto = os.path.join('extras', 'icons', 'web2py.gif')
        if os.path.exists(iconphoto):
            img = Tkinter.PhotoImage(file=iconphoto)
            self.root.tk.call('wm', 'iconphoto', self.root._w, img)
        # Building the Menu
        item = lambda: start_browser(httplog)
        servermenu.add_command(label='View httpserver.log',
                               command=item)

        servermenu.add_command(label='Quit (pid:%i)' % os.getpid(),
                               command=self.quit)

        self.menu.add_cascade(label='Server', menu=servermenu)

        self.pagesmenu = Tkinter.Menu(self.menu, tearoff=0)
        self.menu.add_cascade(label='Pages', menu=self.pagesmenu)

        #scheduler menu
        self.schedmenu = Tkinter.Menu(self.menu, tearoff=0)
        self.menu.add_cascade(label='Scheduler', menu=self.schedmenu)
        #start and register schedulers from options
        self.update_schedulers(start=True)

        helpmenu = Tkinter.Menu(self.menu, tearoff=0)

        # Home Page
        item = lambda: start_browser('http://www.web2py.com/')
        helpmenu.add_command(label='Home Page',
                             command=item)

        # About
        item = lambda: tkMessageBox.showinfo('About web2py', ProgramInfo)
        helpmenu.add_command(label='About',
                             command=item)

        self.menu.add_cascade(label='Info', menu=helpmenu)

        self.root.config(menu=self.menu)

        if options.taskbar:
            self.root.protocol('WM_DELETE_WINDOW',
                               lambda: self.quit(True))
        else:
            self.root.protocol('WM_DELETE_WINDOW', self.quit)

        sticky = Tkinter.NW

        # Prepare the logo area
        self.logoarea = Tkinter.Canvas(self.root,
                                background=bg_color,
                                width=300,
                                height=300)
        self.logoarea.grid(row=0, column=0, columnspan=4, sticky=sticky)
        self.logoarea.after(1000, self.update_canvas)

        logo = os.path.join('extras', 'icons', 'splashlogo.gif')
        if os.path.exists(logo):
            img = Tkinter.PhotoImage(file=logo)
            pnl = Tkinter.Label(self.logoarea, image=img, background=bg_color, bd=0)
            pnl.pack(side='top', fill='both', expand='yes')
            # Prevent garbage collection of img
            pnl.image = img

        # Prepare the banner area
        self.bannerarea = Tkinter.Canvas(self.root,
                                bg=bg_color,
                                width=300,
                                height=300)
        self.bannerarea.grid(row=1, column=1, columnspan=2, sticky=sticky)

        Tkinter.Label(self.bannerarea, anchor=Tkinter.N,
                      text=str(ProgramVersion + "\n" + ProgramAuthor),
                      font=('Helvetica', 11), justify=Tkinter.CENTER,
                      foreground='#195866', background=bg_color,
                      height=3).pack( side='top',
                                      fill='both',
                                      expand='yes')

        self.bannerarea.after(1000, self.update_canvas)

        # IP
        Tkinter.Label(self.root,
                      text='Server IP:', bg=bg_color,
                      justify=Tkinter.RIGHT).grid(row=4,
                                                  column=1,
                                                  sticky=sticky)
        self.ips = {}
        self.selected_ip = Tkinter.StringVar()
        row = 4
        ips = [('127.0.0.1', 'Local (IPv4)')] + \
            ([('::1', 'Local (IPv6)')] if socket.has_ipv6 else []) + \
            [(ip, 'Public') for ip in options.ips] + \
            [('0.0.0.0', 'Public')]
        for ip, legend in ips:
            self.ips[ip] = Tkinter.Radiobutton(
                self.root, bg=bg_color, highlightthickness=0,
                selectcolor='light grey', width=30,
                anchor=Tkinter.W, text='%s (%s)' % (legend, ip),
                justify=Tkinter.LEFT,
                variable=self.selected_ip, value=ip)
            self.ips[ip].grid(row=row, column=2, sticky=sticky)
            if row == 4:
                self.ips[ip].select()
            row += 1
        shift = row

        # Port
        Tkinter.Label(self.root,
                      text='Server Port:', bg=bg_color,
                      justify=Tkinter.RIGHT).grid(row=shift,
                                                  column=1, pady=10,
                                                  sticky=sticky)

        self.port_number = Tkinter.Entry(self.root)
        self.port_number.insert(Tkinter.END, self.options.port)
        self.port_number.grid(row=shift, column=2, sticky=sticky, pady=10)

        # Password
        Tkinter.Label(self.root,
                      text='Choose Password:', bg=bg_color,
                      justify=Tkinter.RIGHT).grid(row=shift + 1,
                                                  column=1,
                                                  sticky=sticky)

        self.password = Tkinter.Entry(self.root, show='*')
        self.password.bind('<Return>', lambda e: self.start())
        self.password.focus_force()
        self.password.grid(row=shift + 1, column=2, sticky=sticky)

        # Prepare the canvas
        self.canvas = Tkinter.Canvas(self.root,
                                     width=400,
                                     height=100,
                                     bg='black')
        self.canvas.grid(row=shift + 2, column=1, columnspan=2, pady=5,
                         sticky=sticky)
        self.canvas.after(1000, self.update_canvas)

        # Prepare the frame
        frame = Tkinter.Frame(self.root)
        frame.grid(row=shift + 3, column=1, columnspan=2, pady=5,
                   sticky=sticky)

        # Start button
        self.button_start = Tkinter.Button(frame,
                                           text='start server',
                                           command=self.start)

        self.button_start.grid(row=0, column=0, sticky=sticky)

        # Stop button
        self.button_stop = Tkinter.Button(frame,
                                          text='stop server',
                                          command=self.stop)

        self.button_stop.grid(row=0, column=1,  sticky=sticky)
        self.button_stop.configure(state='disabled')

        if options.taskbar:
            import gluon.contrib.taskbar_widget
            self.tb = gluon.contrib.taskbar_widget.TaskBarIcon()
            self.checkTaskBar()

            if options.password != '<ask>':
                self.password.insert(0, options.password)
                self.start()
                self.root.withdraw()
        else:
            self.tb = None

    def update_schedulers(self, start=False):
        apps = []
        available_apps = [arq for arq in os.listdir('applications/')]
        available_apps = [arq for arq in available_apps
                          if os.path.exists(
                'applications/%s/models/scheduler.py' % arq)]
        if start:
            # the widget takes care of starting the scheduler
            if self.options.scheduler and self.options.with_scheduler:
                apps = [app.strip() for app
                        in self.options.scheduler.split(',')
                        if app in available_apps]
        for app in apps:
            self.try_start_scheduler(app)

        # reset the menu
        self.schedmenu.delete(0, len(available_apps))
        for arq in available_apps:
            if arq not in self.scheduler_processes:
                item = lambda u = arq: self.try_start_scheduler(u)
                self.schedmenu.add_command(label="start %s" % arq,
                                           command=item)
            if arq in self.scheduler_processes:
                item = lambda u = arq: self.try_stop_scheduler(u)
                self.schedmenu.add_command(label="stop %s" % arq,
                                           command=item)

    def start_schedulers(self, app):
        try:
            from multiprocessing import Process
        except:
            sys.stderr.write('Sorry, -K only supported for python 2.6-2.7\n')
            return
        code = "from gluon import current;current._scheduler.loop()"
        print 'starting scheduler from widget for "%s"...' % app
        args = (app, True, True, None, False, code)
        logging.getLogger().setLevel(self.options.debuglevel)
        p = Process(target=run, args=args)
        self.scheduler_processes[app] = p
        self.update_schedulers()
        print "Currently running %s scheduler processes" % (
            len(self.scheduler_processes))
        p.start()
        print "Processes started"

    def try_stop_scheduler(self, app):
        if app in self.scheduler_processes:
            p = self.scheduler_processes[app]
            del self.scheduler_processes[app]
            p.terminate()
            p.join()
        self.update_schedulers()

    def try_start_scheduler(self, app):
        if app not in self.scheduler_processes:
            t = threading.Thread(target=self.start_schedulers, args=(app,))
            t.start()

    def checkTaskBar(self):
        """ Checks taskbar status """

        if self.tb.status:
            if self.tb.status[0] == self.tb.EnumStatus.QUIT:
                self.quit()
            elif self.tb.status[0] == self.tb.EnumStatus.TOGGLE:
                if self.root.state() == 'withdrawn':
                    self.root.deiconify()
                else:
                    self.root.withdraw()
            elif self.tb.status[0] == self.tb.EnumStatus.STOP:
                self.stop()
            elif self.tb.status[0] == self.tb.EnumStatus.START:
                self.start()
            elif self.tb.status[0] == self.tb.EnumStatus.RESTART:
                self.stop()
                self.start()
            del self.tb.status[0]

        self.root.after(1000, self.checkTaskBar)

    def update(self, text):
        """ Updates app text """

        try:
            self.text.configure(state='normal')
            self.text.insert('end', text)
            self.text.configure(state='disabled')
        except:
            pass  # ## this should only happen in case app is destroyed

    def connect_pages(self):
        """ Connects pages """
        # reset the menu
        available_apps = [arq for arq in os.listdir('applications/')
                          if os.path.exists(
                'applications/%s/__init__.py' % arq)]
        self.pagesmenu.delete(0, len(available_apps))
        for arq in available_apps:
            url = self.url + arq
            self.pagesmenu.add_command(
                label=url, command=lambda u=url: start_browser(u))

    def quit(self, justHide=False):
        """ Finishes the program execution """
        if justHide:
            self.root.withdraw()
        else:
            try:
                scheds = self.scheduler_processes.keys()
                for t in scheds:
                    self.try_stop_scheduler(t)
            except:
                pass
            try:
                newcron.stopcron()
            except:
                pass
            try:
                self.server.stop()
            except:
                pass
            try:
                self.tb.Destroy()
            except:
                pass

            self.root.destroy()
            sys.exit(0)

    def error(self, message):
        """ Shows error message """

        import tkMessageBox
        tkMessageBox.showerror('web2py start server', message)

    def start(self):
        """ Starts web2py server """

        password = self.password.get()

        if not password:
            self.error('no password, no web admin interface')

        ip = self.selected_ip.get()

        if not is_valid_ip_address(ip):
            return self.error('invalid host ip address')

        try:
            port = int(self.port_number.get())
        except:
            return self.error('invalid port number')

        # Check for non default value for ssl inputs
        if (len(self.options.ssl_certificate) > 0 or
            len(self.options.ssl_private_key) > 0):
            proto = 'https'
        else:
            proto = 'http'

        self.url = get_url(ip, proto=proto, port=port)
        self.connect_pages()
        self.button_start.configure(state='disabled')

        try:
            options = self.options
            req_queue_size = options.request_queue_size
            self.server = main.HttpServer(
                ip,
                port,
                password,
                pid_filename=options.pid_filename,
                log_filename=options.log_filename,
                profiler_dir=options.profiler_dir,
                ssl_certificate=options.ssl_certificate,
                ssl_private_key=options.ssl_private_key,
                ssl_ca_certificate=options.ssl_ca_certificate,
                min_threads=options.minthreads,
                max_threads=options.maxthreads,
                server_name=options.server_name,
                request_queue_size=req_queue_size,
                timeout=options.timeout,
                shutdown_timeout=options.shutdown_timeout,
                path=options.folder,
                interfaces=options.interfaces)

            thread.start_new_thread(self.server.start, ())
        except Exception, e:
            self.button_start.configure(state='normal')
            return self.error(str(e))

        if not self.server_ready():
            self.button_start.configure(state='normal')
            return

        self.button_stop.configure(state='normal')

        if not options.taskbar:
            thread.start_new_thread(
                start_browser, (get_url(ip, proto=proto, port=port), True))

        self.password.configure(state='readonly')
        [ip.configure(state='disabled') for ip in self.ips.values()]
        self.port_number.configure(state='readonly')

        if self.tb:
            self.tb.SetServerRunning()

    def server_ready(self):
        for listener in self.server.server.listeners:
            if listener.ready:
                return True

        return False

    def stop(self):
        """ Stops web2py server """

        self.button_start.configure(state='normal')
        self.button_stop.configure(state='disabled')
        self.password.configure(state='normal')
        [ip.configure(state='normal') for ip in self.ips.values()]
        self.port_number.configure(state='normal')
        self.server.stop()

        if self.tb:
            self.tb.SetServerStopped()

    def update_canvas(self):
        """ Updates canvas """

        try:
            t1 = os.path.getsize('httpserver.log')
        except:
            self.canvas.after(1000, self.update_canvas)
            return

        try:
            fp = open('httpserver.log', 'r')
            fp.seek(self.t0)
            data = fp.read(t1 - self.t0)
            fp.close()
            value = self.p0[1:] + [10 + 90.0 / math.sqrt(1 + data.count('\n'))]
            self.p0 = value

            for i in xrange(len(self.p0) - 1):
                c = self.canvas.coords(self.q0[i])
                self.canvas.coords(self.q0[i],
                                   (c[0],
                                    self.p0[i],
                                    c[2],
                                    self.p0[i + 1]))
            self.t0 = t1
        except BaseException:
            self.t0 = time.time()
            self.t0 = t1
            self.p0 = [100] * 400
            self.q0 = [self.canvas.create_line(i, 100, i + 1, 100,
                       fill='green') for i in xrange(len(self.p0) - 1)]

        self.canvas.after(1000, self.update_canvas)


def console():
    """ Defines the behavior of the console web2py execution """
    import optparse
    import textwrap

    usage = "python web2py.py"

    description = """\
    web2py Web Framework startup script.
    ATTENTION: unless a password is specified (-a 'passwd') web2py will
    attempt to run a GUI. In this case command line options are ignored."""

    description = textwrap.dedent(description)

    parser = optparse.OptionParser(
        usage, None, optparse.Option, ProgramVersion)

    parser.description = description

    msg = ('IP address of the server (e.g., 127.0.0.1 or ::1); '
           'Note: This value is ignored when using the \'interfaces\' option.')
    parser.add_option('-i',
                      '--ip',
                      default='127.0.0.1',
                      dest='ip',
                      help=msg)

    parser.add_option('-p',
                      '--port',
                      default='8000',
                      dest='port',
                      type='int',
                      help='port of server (8000)')

    parser.add_option('-G',
                      '--GAE',
                      default=None,
                      dest='gae',
                      help="'-G configure' will create app.yaml and gaehandler.py")

    msg = ('password to be used for administration '
           '(use -a "<recycle>" to reuse the last password))')
    parser.add_option('-a',
                      '--password',
                      default='<ask>',
                      dest='password',
                      help=msg)

    parser.add_option('-c',
                      '--ssl_certificate',
                      default='',
                      dest='ssl_certificate',
                      help='file that contains ssl certificate')

    parser.add_option('-k',
                      '--ssl_private_key',
                      default='',
                      dest='ssl_private_key',
                      help='file that contains ssl private key')

    msg = ('Use this file containing the CA certificate to validate X509 '
           'certificates from clients')
    parser.add_option('--ca-cert',
                      action='store',
                      dest='ssl_ca_certificate',
                      default=None,
                      help=msg)

    parser.add_option('-d',
                      '--pid_filename',
                      default='httpserver.pid',
                      dest='pid_filename',
                      help='file to store the pid of the server')

    parser.add_option('-l',
                      '--log_filename',
                      default='httpserver.log',
                      dest='log_filename',
                      help='file to log connections')

    parser.add_option('-n',
                      '--numthreads',
                      default=None,
                      type='int',
                      dest='numthreads',
                      help='number of threads (deprecated)')

    parser.add_option('--minthreads',
                      default=None,
                      type='int',
                      dest='minthreads',
                      help='minimum number of server threads')

    parser.add_option('--maxthreads',
                      default=None,
                      type='int',
                      dest='maxthreads',
                      help='maximum number of server threads')

    parser.add_option('-s',
                      '--server_name',
                      default=socket.gethostname(),
                      dest='server_name',
                      help='server name for the web server')

    msg = 'max number of queued requests when server unavailable'
    parser.add_option('-q',
                      '--request_queue_size',
                      default='5',
                      type='int',
                      dest='request_queue_size',
                      help=msg)

    parser.add_option('-o',
                      '--timeout',
                      default='10',
                      type='int',
                      dest='timeout',
                      help='timeout for individual request (10 seconds)')

    parser.add_option('-z',
                      '--shutdown_timeout',
                      default='5',
                      type='int',
                      dest='shutdown_timeout',
                      help='timeout on shutdown of server (5 seconds)')

    parser.add_option('--socket-timeout',
                      default=5,
                      type='int',
                      dest='socket_timeout',
                      help='timeout for socket (5 second)')

    parser.add_option('-f',
                      '--folder',
                      default=os.getcwd(),
                      dest='folder',
                      help='folder from which to run web2py')

    parser.add_option('-v',
                      '--verbose',
                      action='store_true',
                      dest='verbose',
                      default=False,
                      help='increase --test verbosity')

    parser.add_option('-Q',
                      '--quiet',
                      action='store_true',
                      dest='quiet',
                      default=False,
                      help='disable all output')

    parser.add_option('-e',
                      '--errors_to_console',
                      action='store_true',
                      dest='print_errors',
                      default=False,
                      help='log all errors to console')

    msg = ('set debug output level (0-100, 0 means all, 100 means none; '
           'default is 30)')
    parser.add_option('-D',
                      '--debug',
                      dest='debuglevel',
                      default=30,
                      type='int',
                      help=msg)

    msg = ('run web2py in interactive shell or IPython (if installed) with '
           'specified appname (if app does not exist it will be created). '
           'APPNAME like a/c/f (c,f optional)')
    parser.add_option('-S',
                      '--shell',
                      dest='shell',
                      metavar='APPNAME',
                      help=msg)

    msg = ('run web2py in interactive shell or bpython (if installed) with '
           'specified appname (if app does not exist it will be created).\n'
           'Use combined with --shell')
    parser.add_option('-B',
                      '--bpython',
                      action='store_true',
                      default=False,
                      dest='bpython',
                      help=msg)

    msg = 'only use plain python shell; should be used with --shell option'
    parser.add_option('-P',
                      '--plain',
                      action='store_true',
                      default=False,
                      dest='plain',
                      help=msg)

    msg = ('auto import model files; default is False; should be used '
           'with --shell option')
    parser.add_option('-M',
                      '--import_models',
                      action='store_true',
                      default=False,
                      dest='import_models',
                      help=msg)

    msg = ('run PYTHON_FILE in web2py environment; '
           'should be used with --shell option')
    parser.add_option('-R',
                      '--run',
                      dest='run',
                      metavar='PYTHON_FILE',
                      default='',
                      help=msg)

    msg = ('run scheduled tasks for the specified apps: expects a list of '
           'app names as -K app1,app2,app3 '
           'or a list of app:groups as -K app1:group1:group2,app2:group1 '
           'to override specific group_names. (only strings, no spaces '
           'allowed. Requires a scheduler defined in the models')
    parser.add_option('-K',
                      '--scheduler',
                      dest='scheduler',
                      default=None,
                      help=msg)

    msg = 'run schedulers alongside webserver, needs -K app1 and -a too'
    parser.add_option('-X',
                      '--with-scheduler',
                      action='store_true',
                      default=False,
                      dest='with_scheduler',
                      help=msg)

    msg = ('run doctests in web2py environment; '
           'TEST_PATH like a/c/f (c,f optional)')
    parser.add_option('-T',
                      '--test',
                      dest='test',
                      metavar='TEST_PATH',
                      default=None,
                      help=msg)

    msg = 'trigger a cron run manually; usually invoked from a system crontab'
    parser.add_option('-C',
                      '--cron',
                      action='store_true',
                      dest='extcron',
                      default=False,
                      help=msg)

    msg = 'triggers the use of softcron'
    parser.add_option('--softcron',
                      action='store_true',
                      dest='softcron',
                      default=False,
                      help=msg)

    parser.add_option('-Y',
                      '--run-cron',
                      action='store_true',
                      dest='runcron',
                      default=False,
                      help='start the background cron process')

    parser.add_option('-J',
                      '--cronjob',
                      action='store_true',
                      dest='cronjob',
                      default=False,
                      help='identify cron-initiated command')

    parser.add_option('-L',
                      '--config',
                      dest='config',
                      default='',
                      help='config file')

    parser.add_option('-F',
                      '--profiler',
                      dest='profiler_dir',
                      default=None,
                      help='profiler dir')

    parser.add_option('-t',
                      '--taskbar',
                      action='store_true',
                      dest='taskbar',
                      default=False,
                      help='use web2py gui and run in taskbar (system tray)')

    parser.add_option('',
                      '--nogui',
                      action='store_true',
                      default=False,
                      dest='nogui',
                      help='text-only, no GUI')

    msg = ('should be followed by a list of arguments to be passed to script, '
           'to be used with -S, -A must be the last option')
    parser.add_option('-A',
                      '--args',
                      action='store',
                      dest='args',
                      default=None,
                      help=msg)

    parser.add_option('--no-banner',
                      action='store_true',
                      default=False,
                      dest='nobanner',
                      help='Do not print header banner')

    msg = ('listen on multiple addresses: '
           '"ip1:port1:key1:cert1:ca_cert1;ip2:port2:key2:cert2:ca_cert2;..." '
           '(:key:cert:ca_cert optional; no spaces; IPv6 addresses must be in '
           'square [] brackets)')
    parser.add_option('--interfaces',
                      action='store',
                      dest='interfaces',
                      default=None,
                      help=msg)

    msg = 'runs web2py tests'
    parser.add_option('--run_system_tests',
                      action='store_true',
                      dest='run_system_tests',
                      default=False,
                      help=msg)

    msg = ('adds coverage reporting (needs --run_system_tests), '
           'python 2.7 and the coverage module installed. '
           'You can alter the default path setting the environmental '
           'var "COVERAGE_PROCESS_START". '
           'By default it takes gluon/tests/coverage.ini')
    parser.add_option('--with_coverage',
                      action='store_true',
                      dest='with_coverage',
                      default=False,
                      help=msg)

    if '-A' in sys.argv:
        k = sys.argv.index('-A')
    elif '--args' in sys.argv:
        k = sys.argv.index('--args')
    else:
        k = len(sys.argv)
    sys.argv, other_args = sys.argv[:k], sys.argv[k + 1:]
    (options, args) = parser.parse_args()
    options.args = [options.run] + other_args
    global_settings.cmd_options = options
    global_settings.cmd_args = args

    if options.gae:
        if not os.path.exists('app.yaml'):
            name = raw_input("Your GAE app name: ")
            content = open(os.path.join('examples', 'app.example.yaml'), 'rb').read()
            open('app.yaml', 'wb').write(content.replace("yourappname", name))
        else:
            print "app.yaml alreday exists in the web2py folder"
        if not os.path.exists('gaehandler.py'):
            content = open(os.path.join('handlers', 'gaehandler.py'), 'rb').read()
            open('gaehandler.py', 'wb').write(content)
        else:
            print "gaehandler.py alreday exists in the web2py folder"
        sys.exit(0)

    try:
        options.ips = list(set(  # no duplicates
            [addrinfo[4][0] for addrinfo in getipaddrinfo(socket.getfqdn())
             if not is_loopback_ip_address(addrinfo=addrinfo)]))
    except socket.gaierror:
        options.ips = []

    if options.run_system_tests:
        run_system_tests(options)

    if options.quiet:
        capture = cStringIO.StringIO()
        sys.stdout = capture
        logger.setLevel(logging.CRITICAL + 1)
    else:
        logger.setLevel(options.debuglevel)

    if options.config[-3:] == '.py':
        options.config = options.config[:-3]

    if options.cronjob:
        global_settings.cronjob = True  # tell the world
        options.plain = True    # cronjobs use a plain shell
        options.nobanner = True
        options.nogui = True

    options.folder = os.path.abspath(options.folder)

    #  accept --interfaces in the form
    #  "ip1:port1:key1:cert1:ca_cert1;[ip2]:port2;ip3:port3:key3:cert3"
    #  (no spaces; optional key:cert indicate SSL)
    if isinstance(options.interfaces, str):
        interfaces = options.interfaces.split(';')
        options.interfaces = []
        for interface in interfaces:
            if interface.startswith('['):  # IPv6
                ip, if_remainder = interface.split(']', 1)
                ip = ip[1:]
                if_remainder = if_remainder[1:].split(':')
                if_remainder[0] = int(if_remainder[0])  # numeric port
                options.interfaces.append(tuple([ip] + if_remainder))
            else:  # IPv4
                interface = interface.split(':')
                interface[1] = int(interface[1])  # numeric port
                options.interfaces.append(tuple(interface))

    #  accepts --scheduler in the form
    #  "app:group1,group2,app2:group1"
    scheduler = []
    options.scheduler_groups = None
    if isinstance(options.scheduler, str):
        if ':' in options.scheduler:
            for opt in options.scheduler.split(','):
                scheduler.append(opt.split(':'))
            options.scheduler = ','.join([app[0] for app in scheduler])
            options.scheduler_groups = scheduler

    if options.numthreads is not None and options.minthreads is None:
        options.minthreads = options.numthreads  # legacy

    create_welcome_w2p()

    if not options.cronjob:
        # If we have the applications package or if we should upgrade
        if not os.path.exists('applications/__init__.py'):
            write_file('applications/__init__.py', '')

    return options, args


def check_existent_app(options, appname):
    if os.path.isdir(os.path.join(options.folder, 'applications', appname)):
        return True


def get_code_for_scheduler(app, options):
    if len(app) == 1 or app[1] is None:
        code = "from gluon import current;current._scheduler.loop()"
    else:
        code = "from gluon import current;current._scheduler.group_names = ['%s'];"
        code += "current._scheduler.loop()"
        code = code % ("','".join(app[1:]))
    app_ = app[0]
    if not check_existent_app(options, app_):
        print "Application '%s' doesn't exist, skipping" % app_
        return None, None
    return app_, code


def start_schedulers(options):
    try:
        from multiprocessing import Process
    except:
        sys.stderr.write('Sorry, -K only supported for python 2.6-2.7\n')
        return
    processes = []
    apps = [(app.strip(), None) for app in options.scheduler.split(',')]
    if options.scheduler_groups:
        apps = options.scheduler_groups
    code = "from gluon import current;current._scheduler.loop()"
    logging.getLogger().setLevel(options.debuglevel)
    if len(apps) == 1 and not options.with_scheduler:
        app_, code = get_code_for_scheduler(apps[0], options)
        if not app_:
            return
        print 'starting single-scheduler for "%s"...' % app_
        run(app_, True, True, None, False, code)
        return
    for app in apps:
        app_, code = get_code_for_scheduler(app, options)
        if not app_:
            continue
        print 'starting scheduler for "%s"...' % app_
        args = (app_, True, True, None, False, code)
        p = Process(target=run, args=args)
        processes.append(p)
        print "Currently running %s scheduler processes" % (len(processes))
        p.start()
        ##to avoid bashing the db at the same time
        time.sleep(0.7)
        print "Processes started"
    for p in processes:
        try:
            p.join()
        except (KeyboardInterrupt, SystemExit):
            print "Processes stopped"
        except:
            p.terminate()
            p.join()


def start(cron=True):
    """ Starts server  """

    # ## get command line arguments

    (options, args) = console()

    if not options.nobanner:
        print ProgramName
        print ProgramAuthor
        print ProgramVersion

    from pydal.drivers import DRIVERS
    if not options.nobanner:
        print 'Database drivers available: %s' % ', '.join(DRIVERS)

    # ## if -L load options from options.config file
    if options.config:
        try:
            options2 = __import__(options.config, {}, {}, '')
        except Exception:
            try:
                # Jython doesn't like the extra stuff
                options2 = __import__(options.config)
            except Exception:
                print 'Cannot import config file [%s]' % options.config
                sys.exit(1)
        for key in dir(options2):
            if hasattr(options, key):
                setattr(options, key, getattr(options2, key))

    logfile0 = os.path.join('extras', 'examples', 'logging.example.conf')
    if not os.path.exists('logging.conf') and os.path.exists(logfile0):
        import shutil
        sys.stdout.write("Copying logging.conf.example to logging.conf ... ")
        shutil.copyfile('logging.example.conf', logfile0)
        sys.stdout.write("OK\n")

    # ## if -T run doctests (no cron)
    if hasattr(options, 'test') and options.test:
        test(options.test, verbose=options.verbose)
        return

    # ## if -S start interactive shell (also no cron)
    if options.shell:
        if options.folder:
            os.chdir(options.folder)
        if not options.args is None:
            sys.argv[:] = options.args
        run(options.shell, plain=options.plain, bpython=options.bpython,
            import_models=options.import_models, startfile=options.run,
            cronjob=options.cronjob)
        return

    # ## if -C start cron run (extcron) and exit
    # ##    -K specifies optional apps list (overloading scheduler)
    if options.extcron:
        logger.debug('Starting extcron...')
        global_settings.web2py_crontype = 'external'
        if options.scheduler:   # -K
            apps = [app.strip() for app in options.scheduler.split(
                ',') if check_existent_app(options, app.strip())]
        else:
            apps = None
        extcron = newcron.extcron(options.folder, apps=apps)
        extcron.start()
        extcron.join()
        return

    # ## if -K
    if options.scheduler and not options.with_scheduler:
        try:
            start_schedulers(options)
        except KeyboardInterrupt:
            pass
        return

    # ## if -H cron is enabled in this *process*
    # ## if --softcron use softcron
    # ## use hardcron in all other cases
    if cron and options.runcron and options.softcron:
        print 'Using softcron (but this is not very efficient)'
        global_settings.web2py_crontype = 'soft'
    elif cron and options.runcron:
        logger.debug('Starting hardcron...')
        global_settings.web2py_crontype = 'hard'
        newcron.hardcron(options.folder).start()

    # ## if no password provided and havetk start Tk interface
    # ## or start interface if we want to put in taskbar (system tray)

    try:
        options.taskbar
    except:
        options.taskbar = False

    if options.taskbar and os.name != 'nt':
        print 'Error: taskbar not supported on this platform'
        sys.exit(1)

    root = None

    if not options.nogui and options.password == '<ask>':
        try:
            import Tkinter
            havetk = True
            try:
                root = Tkinter.Tk()
            except:
                pass
        except (ImportError, OSError):
            logger.warn(
                'GUI not available because Tk library is not installed')
            havetk = False
            options.nogui = True

    if root:
        root.focus_force()

        # Mac OS X - make the GUI window rise to the top
        if os.path.exists("/usr/bin/osascript"):
            applescript = """
tell application "System Events"
    set proc to first process whose unix id is %d
    set frontmost of proc to true
end tell
""" % (os.getpid())
            os.system("/usr/bin/osascript -e '%s'" % applescript)

        master = web2pyDialog(root, options)
        signal.signal(signal.SIGTERM, lambda a, b: master.quit())

        try:
            root.mainloop()
        except:
            master.quit()

        sys.exit()

    # ## if no tk and no password, ask for a password

    if not root and options.password == '<ask>':
        options.password = getpass.getpass('choose a password:')

    if not options.password and not options.nobanner:
        print 'no password, no admin interface'

    # ##-X (if no tk, the widget takes care of it himself)
    if not root and options.scheduler and options.with_scheduler:
        t = threading.Thread(target=start_schedulers, args=(options,))
        t.start()

    # ## start server

    # Use first interface IP and port if interfaces specified, since the
    # interfaces option overrides the IP (and related) options.
    if not options.interfaces:
        (ip, port) = (options.ip, int(options.port))
    else:
        first_if = options.interfaces[0]
        (ip, port) = first_if[0], first_if[1]

    # Check for non default value for ssl inputs
    if (len(options.ssl_certificate) > 0) or (len(options.ssl_private_key) > 0):
        proto = 'https'
    else:
        proto = 'http'

    url = get_url(ip, proto=proto, port=port)

    if not options.nobanner:
        message = '\nplease visit:\n\t%s\n' % url
        if sys.platform.startswith('win'):
            message += 'use "taskkill /f /pid %i" to shutdown the web2py server\n\n' % os.getpid()
        else:
            message += 'use "kill -SIGTERM %i" to shutdown the web2py server\n\n' % os.getpid()
        print message

    # enhance linecache.getline (used by debugger) to look at the source file
    # if the line was not found (under py2exe & when file was modified)
    import linecache
    py2exe_getline = linecache.getline

    def getline(filename, lineno, *args, **kwargs):
        line = py2exe_getline(filename, lineno, *args, **kwargs)
        if not line:
            try:
                f = open(filename, "r")
                try:
                    for i, line in enumerate(f):
                        if lineno == i + 1:
                            break
                    else:
                        line = None
                finally:
                    f.close()
            except (IOError, OSError):
                line = None
        return line
    linecache.getline = getline

    server = main.HttpServer(ip=ip,
                             port=port,
                             password=options.password,
                             pid_filename=options.pid_filename,
                             log_filename=options.log_filename,
                             profiler_dir=options.profiler_dir,
                             ssl_certificate=options.ssl_certificate,
                             ssl_private_key=options.ssl_private_key,
                             ssl_ca_certificate=options.ssl_ca_certificate,
                             min_threads=options.minthreads,
                             max_threads=options.maxthreads,
                             server_name=options.server_name,
                             request_queue_size=options.request_queue_size,
                             timeout=options.timeout,
                             socket_timeout=options.socket_timeout,
                             shutdown_timeout=options.shutdown_timeout,
                             path=options.folder,
                             interfaces=options.interfaces)

    try:
        server.start()
    except KeyboardInterrupt:
        server.stop()
        try:
            t.join()
        except:
            pass
    logging.shutdown()
