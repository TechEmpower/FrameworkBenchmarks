# -*- coding: utf-8 -*-
#!/usr/bin/env python

"""
This file is part of the web2py Web Framework
Copyrighted by Massimo Di Pierro <mdip...@cs.depaul.edu>
License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

Attention: Requires Chrome or Safari. For IE of Firefox you need https://github.com/gimite/web-socket-js

1) install tornado (requires Tornado 3.0 or later)

   easy_install tornado

2) start this app:

   python gluon/contrib/websocket_messaging.py -k mykey -p 8888

3) from any web2py app you can post messages with

   from gluon.contrib.websocket_messaging import websocket_send
   websocket_send('http://127.0.0.1:8888', 'Hello World', 'mykey', 'mygroup')

4) from any template you can receive them with

   <script>
   $(document).ready(function(){
      if(!$.web2py.web2py_websocket('ws://127.0.0.1:8888/realtime/mygroup', function(e){alert(e.data)}))

         alert("html5 websocket not supported by your browser, try Google Chrome");
   });
   </script>

When the server posts a message, all clients connected to the page will popup an alert message
Or if you want to send json messages and store evaluated json in a var called data:

   <script>
   $(document).ready(function(){
      var data;
      $.web2py.web2py_websocket('ws://127.0.0.1:8888/realtime/mygroup', function(e){data=eval('('+e.data+')')});
   });
   </script>

- All communications between web2py and websocket_messaging will be digitally signed with hmac.
- All validation is handled on the web2py side and there is no need to modify websocket_messaging.py
- Multiple web2py instances can talk with one or more websocket_messaging servers.
- "ws://127.0.0.1:8888/realtime/" must be contain the IP of the websocket_messaging server.
- Via group='mygroup' name you can support multiple groups of clients (think of many chat-rooms)


Here is a complete sample web2py action:

    def index():
        form=LOAD('default', 'ajax_form', ajax=True)
        script=SCRIPT('''
            jQuery(document).ready(function(){
              var callback=function(e){alert(e.data)};
              if(!$.web2py.web2py_websocket('ws://127.0.0.1:8888/realtime/mygroup', callback))

                alert("html5 websocket not supported by your browser, try Google Chrome");
            });
        ''')
        return dict(form=form, script=script)

    def ajax_form():
        form=SQLFORM.factory(Field('message'))
        if form.accepts(request,session):
            from gluon.contrib.websocket_messaging import websocket_send
            websocket_send(
                'http://127.0.0.1:8888', form.vars.message, 'mykey', 'mygroup')
        return form

https is possible too using 'https://127.0.0.1:8888' instead of 'http://127.0.0.1:8888', but need to
be started with

   python gluon/contrib/websocket_messaging.py -k mykey -p 8888 -s keyfile.pem -c certfile.pem

for secure websocket do:

   web2py_websocket('wss://127.0.0.1:8888/realtime/mygroup',callback)

Acknowledgements:
Tornado code inspired by http://thomas.pelletier.im/2010/08/websocket-tornado-redis/

"""

import tornado.httpserver
import tornado.websocket
import tornado.ioloop
import tornado.web
import hmac
import sys
import optparse
import urllib
import time

listeners, names, tokens = {}, {}, {}


def websocket_send(url, message, hmac_key=None, group='default'):
    sig = hmac_key and hmac.new(hmac_key, message).hexdigest() or ''
    params = urllib.urlencode(
        {'message': message, 'signature': sig, 'group': group})
    f = urllib.urlopen(url, params)
    data = f.read()
    f.close()
    return data


class PostHandler(tornado.web.RequestHandler):
    """
    only authorized parties can post messages
    """
    def post(self):
        if hmac_key and not 'signature' in self.request.arguments:
            self.send_error(401)
        if 'message' in self.request.arguments:
            message = self.request.arguments['message'][0]
            group = self.request.arguments.get('group', ['default'])[0]
            print '%s:MESSAGE to %s:%s' % (time.time(), group, message)
            if hmac_key:
                signature = self.request.arguments['signature'][0]
                if not hmac.new(hmac_key, message).hexdigest() == signature:
                    self.send_error(401)
            for client in listeners.get(group, []):
                client.write_message(message)


class TokenHandler(tornado.web.RequestHandler):
    """
    if running with -t post a token to allow a client to join using the token
    the message here is the token (any uuid)
    allows only authorized parties to joins, for example, a chat
    """
    def post(self):
        if hmac_key and not 'message' in self.request.arguments:
            self.send_error(401)
        if 'message' in self.request.arguments:
            message = self.request.arguments['message'][0]
            if hmac_key:
                signature = self.request.arguments['signature'][0]
                if not hmac.new(hmac_key, message).hexdigest() == signature:
                    self.send_error(401)
            tokens[message] = None


class DistributeHandler(tornado.websocket.WebSocketHandler):
    def open(self, params):
        group, token, name = params.split('/') + [None, None]
        self.group = group or 'default'
        self.token = token or 'none'
        self.name = name or 'anonymous'
        # only authorized parties can join
        if DistributeHandler.tokens:
            if not self.token in tokens or not token[self.token] is None:
                self.close()
            else:
                tokens[self.token] = self
        if not self.group in listeners:
            listeners[self.group] = []
        # notify clients that a member has joined the groups
        for client in listeners.get(self.group, []):
            client.write_message('+' + self.name)
        listeners[self.group].append(self)
        names[self] = self.name
        print '%s:CONNECT to %s' % (time.time(), self.group)

    def on_message(self, message):
        pass

    def on_close(self):
        if self.group in listeners:
            listeners[self.group].remove(self)
        del names[self]
        # notify clients that a member has left the groups
        for client in listeners.get(self.group, []):
            client.write_message('-' + self.name)
        print '%s:DISCONNECT from %s' % (time.time(), self.group)

# if your webserver is different from tornado server uncomment this
# or override using something more restrictive:
# http://tornado.readthedocs.org/en/latest/websocket.html#tornado.websocket.WebSocketHandler.check_origin
# def check_origin(self, origin):
#    return True

if __name__ == "__main__":
    usage = __doc__
    version = ""
    parser = optparse.OptionParser(usage, None, optparse.Option, version)
    parser.add_option('-p',
                      '--port',
                      default='8888',
                      dest='port',
                      help='socket')
    parser.add_option('-l',
                      '--listen',
                      default='0.0.0.0',
                      dest='address',
                      help='listener address')
    parser.add_option('-k',
                      '--hmac_key',
                      default='',
                      dest='hmac_key',
                      help='hmac_key')
    parser.add_option('-t',
                      '--tokens',
                      action='store_true',
                      default=False,
                      dest='tokens',
                      help='require tockens to join')
    parser.add_option('-s',
                      '--sslkey',
                      default=False,
                      dest='keyfile',
                      help='require ssl keyfile full path')
    parser.add_option('-c',
                      '--sslcert',
                      default=False,
                      dest='certfile',
                      help='require ssl certfile full path')
    (options, args) = parser.parse_args()
    hmac_key = options.hmac_key
    DistributeHandler.tokens = options.tokens
    urls = [
        (r'/', PostHandler),
        (r'/token', TokenHandler),
        (r'/realtime/(.*)', DistributeHandler)]
    application = tornado.web.Application(urls, auto_reload=True)
    if options.keyfile and options.certfile:
        ssl_options = dict(certfile=options.certfile, keyfile=options.keyfile)
    else:
        ssl_options = None
    http_server = tornado.httpserver.HTTPServer(application, ssl_options=ssl_options)
    http_server.listen(int(options.port), address=options.address)
    tornado.ioloop.IOLoop.instance().start()
