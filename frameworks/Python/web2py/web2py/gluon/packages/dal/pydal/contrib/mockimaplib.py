# -*- encoding: utf-8 -*-

from imaplib import ParseFlags

# mockimaplib: A very simple mock server module for imap client APIs
#    Copyright (C) 2014  Alan Etkin <spametki@gmail.com>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU Lesser General Public License as
#    published by the Free Software Foundation, either version 3 of the
#    License, or(at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public
#    License along with this program. If not, see
#    <http://www.gnu.org/licenses/lgpl.html>

"""
mockimaplib allows you to test applications connecting to a dummy imap
service. For more details on the api subset implemented,
refer to the imaplib docs.

The client should configure a dictionary to map imap string queries to sets
of entries stored in a message dummy storage dictionary. The module includes
a small set of default message records (SPAM and MESSAGES), two mailboxes
(Draft and INBOX) and a list of query/resultset entries (RESULTS).

Usage:

>>> import mockimaplib
>>> connection = mockimaplib.IMAP4_SSL(<host>)
>>> connection.login(<user>, <password>)
None
>>> connection.select("INBOX")
("OK", ... <mailbox length>)
# fetch commands specifying single uid or message id
# will try to get messages recorded in SPAM
>>> connection.uid(...)
<search query or fetch result>
# returns a string list of matching message ids
>>> connection.search(<query>)
("OK", ... "1 2 ... n")
"""

MESSAGES = (
'MIME-Version: 1.0\r\nReceived: by 10.140.91.199 with HTTP; Mon, 27 Jan 2014 13:52:30 -0800 (PST)\r\nDate: Mon, 27 Jan 2014 19:52:30 -0200\r\nDelivered-To: nurse@example.com\r\nMessage-ID: <10101010101010010000010101010001010101001010010000001@mail.example.com>\r\nSubject: spam1\r\nFrom: Mr. Gumby <gumby@example.com>\r\nTo: The nurse <nurse@example.com>\r\nContent-Type: text/plain; charset=ISO-8859-1\r\n\r\nNurse!\r\n\r\n\r\n',
'MIME-Version: 1.0\r\nReceived: by 10.140.91.199 with HTTP; Mon, 27 Jan 2014 13:52:47 -0800 (PST)\r\nDate: Mon, 27 Jan 2014 19:52:47 -0200\r\nDelivered-To: nurse@example.com\r\nMessage-ID: <101010101010100100000101010100010101010010100100000010@mail.example.com>\r\nSubject: spam2\r\nFrom: Mr. Gumby <gumby@example.com>\r\nTo: The nurse <nurse@example.com>\r\nContent-Type: text/plain; charset=ISO-8859-1\r\n\r\nNurse, nurse!',
'MIME-Version: 1.0\r\nReceived: by 10.140.91.199 with HTTP; Mon, 27 Jan 2014 13:54:54 -0800 (PST)\r\nDate: Mon, 27 Jan 2014 19:54:54 -0200\r\nDelivered-To: nurse@example.com\r\nMessage-ID: <1010101010101001000001010101000101010100101001000000101@mail.example.com>\r\nSubject: spamalot1\r\nFrom: Mr. Gumby <gumby@example.com>\r\nTo: The nurse <nurse@example.com>\r\nContent-Type: text/plain; charset=ISO-8859-1\r\n\r\nNurse!\r\n\r\n\r\n',
'MIME-Version: 1.0\r\n\r\nReceived: by 10.140.91.199 with HTTP; Mon, 27 Jan 2014 13:54:54 -0800 (PST)\r\nDate: Mon, 27 Jan 2014 19:54:54 -0200\r\nDelivered-To: nurse@example.com\r\nMessage-ID: <101010101010100100000101010100010101010010100100000010101@mail.example.com>\r\nSubject: spamalot2\r\nFrom: Mr. Gumby <gumby@example.com>\r\nTo: The nurse <nurse@example.com>\r\nContent-Type: text/plain; charset=ISO-8859-1\r\n\r\nNurse! ... Nurse! ... Nurse!\r\n\r\n\r\n')

SPAM = {
    "INBOX": [
        {"uid": "483209",
         "headers": MESSAGES[0],
         "complete": MESSAGES[0],
         "flags": ""},
        {"uid": "483211",
         "headers": MESSAGES[1],
         "complete": MESSAGES[1],
         "flags": ""},
        {"uid": "483225",
         "headers": MESSAGES[2],
         "complete": MESSAGES[2],
         "flags": ""}],
    "Draft":[
        {"uid": "483432",
         "headers": MESSAGES[3],
         "complete": MESSAGES[3],
         "flags": ""},]
}

RESULTS = {
    # <query string>: [<str uid> | <long id>, ...]
    "INBOX": {
        "(ALL)": (1, 2, 3),
        "(1:3)": (1, 2, 3)},
    "Draft": {
        "(1:1)": (1,)},
}

class Connection(object):
    """Dummy connection object for the imap client.
    By default, uses the module SPAM and RESULT
    sets (use Connection.setup for custom values)"""
    def login(self, user, password):
        pass

    def __init__(self):
        self._readonly = False
        self._mailbox = None
        self.setup()

    def list(self):
        return ('OK', ['(\\HasNoChildren) "/" "%s"' % key for key in self.spam])

    def select(self, tablename, readonly=False):
        self._readonly = readonly
        """args: mailbox, boolean
        result[1][0] -> int last message id / mailbox lenght
        result[0] = 'OK'
        """
        self._mailbox = tablename
        return ('OK', (len(SPAM[self._mailbox]), None))

    def uid(self, command, uid, arg):
        """ args:
              command: "search" | "fetch"
              uid: None | uid
              parts: "(ALL)" | "(RFC822 FLAGS)" | "(RFC822.HEADER FLAGS)"

        "search", None, "(ALL)" -> ("OK", ("uid_1 uid_2 ... uid_<mailbox length>", None))
        "search", None, "<query>" -> ("OK", ("uid_1 uid_2 ... uid_n", None))
        "fetch", uid, parts -> ("OK", (("<id> ...", "<raw message as specified in parts>"), "<flags>")
                                [0]     [1][0][0]     [1][0][1]                              [1][1]
        """
        if command == "search":
            return self._search(arg)
        elif command == "fetch":
            return self._fetch(uid, arg)

    def _search(self, query):
        return ("OK", (" ".join([str(item["uid"]) for item in self._get_messages(query)]), None))

    def _fetch(self, value, arg):
        try:
            message = self.spam[self._mailbox][value - 1]
            message_id = value
        except TypeError:
            for x, item in enumerate(self.spam[self._mailbox]):
                if item["uid"] == value:
                    message = item
                    message_id = x + 1
                    break

        parts = "headers"
        if arg in ("(ALL)", "(RFC822 FLAGS)"):
            parts = "complete"

        return ("OK", (("%s " % message_id, message[parts]), message["flags"]))
 
    def _get_messages(self, query):
        if query.strip().isdigit():
            return [self.spam[self._mailbox][int(query.strip()) - 1],]
        elif query[1:-1].strip().isdigit():
            return [self.spam[self._mailbox][int(query[1:-1].strip()) -1],]
        elif query[1:-1].replace("UID", "").strip().isdigit():
            for item in self.spam[self._mailbox]:
                if item["uid"] == query[1:-1].replace("UID", "").strip():
                    return [item,]
        messages = []        
        try:
            for m in self.results[self._mailbox][query]:
                try:
                    self.spam[self._mailbox][m - 1]["id"] = m
                    messages.append(self.spam[self._mailbox][m - 1])
                except TypeError:
                    for x, item in enumerate(self.spam[self._mailbox]):
                        if item["uid"] == m:
                            item["id"] = x + 1
                            messages.append(item)
                            break
                except IndexError:
                    # message removed
                    pass
            return messages
        except KeyError:
            raise ValueError("The client issued an unexpected query: %s" % query)
        
    def setup(self, spam={}, results={}):
        """adds custom message and query databases or sets
        the values to the module defaults.
        """

        self.spam = spam
        self.results = results
        if not spam:
            for key in SPAM:
                self.spam[key] = []
                for d in SPAM[key]:
                    self.spam[key].append(d.copy())
        if not results:
            for key in RESULTS:
                self.results[key] = RESULTS[key].copy()


    def search(self, first, query):
        """ args:
             first: None
             query: string with mailbox query (flags, date, uid, id, ...)
                example: '2:15723 BEFORE 27-Jan-2014 FROM "gumby"'
        result[1][0] -> "id_1 id_2 ... id_n"
        """
        messages = self._get_messages(query)
        ids = " ".join([str(item["id"]) for item in messages])
        return ("OK", (ids, None))

    def append(self, mailbox, flags, struct_time, message):
        """
            result, data = self.connection.append(mailbox, flags, struct_time, message)
            if result == "OK":
                uid = int(re.findall("\d+", str(data))[-1])
        """
        last = self.spam[mailbox][-1]
        try:
            uid = int(last["uid"]) +1
        except ValueError:
            alluids = []
            for _mailbox in self.spam.keys():
                for item in self.spam[_mailbox]:
                    try:
                        alluids.append(int(item["uid"]))
                    except:
                        pass
            if len(alluids) > 0:
                uid = max(alluids) + 1
            else:
                uid = 1
        flags = "FLAGS " + flags
        item = {"uid": str(uid), "headers": message, "complete": message, "flags": flags}
        self.spam[mailbox].append(item)
        return ("OK", "spam spam %s spam" % uid)


    def store(self, *args):
        """
        implements some flag commands
        args: ("<id>", "<+|->FLAGS", "(\\Flag1 \\Flag2 ... \\Flagn)")
        """
        message = self.spam[self._mailbox][int(args[0] - 1)]
        old_flags = ParseFlags(message["flags"])
        flags = ParseFlags("FLAGS" + args[2])
        if args[1].strip().startswith("+"):
            message["flags"] = "FLAGS (%s)" % " ".join(set(flags + old_flags))
        elif args[1].strip().startswith("-"):
            message["flags"] = "FLAGS (%s)" % " ".join([flag for flag in old_flags if not flag in flags])

    def expunge(self):
        """implements removal of deleted flag messages"""
        for x, item in enumerate(self.spam[self._mailbox]):
            if "\\Deleted" in item["flags"]:
                self.spam[self._mailbox].pop(x)


class IMAP4(object):
    """>>> connection = IMAP4() # creates the dummy imap4 client object"""
    def __new__(self, *args, **kwargs):
        # args: (server, port)
        return Connection()

IMAP4_SSL = IMAP4

