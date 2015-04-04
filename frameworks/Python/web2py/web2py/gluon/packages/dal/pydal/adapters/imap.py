# -*- coding: utf-8 -*-
import datetime
import re
import sys

from .._globals import IDENTITY, GLOBAL_LOCKER
from ..connection import ConnectionPool
from ..objects import Field, Query, Expression
from ..helpers.classes import SQLALL
from ..helpers.methods import use_common_filters
from .base import NoSQLAdapter


class IMAPAdapter(NoSQLAdapter):

    """ IMAP server adapter

    This class is intended as an interface with
    email IMAP servers to perform simple queries in the
    web2py DAL query syntax, so email read, search and
    other related IMAP mail services (as those implemented
    by brands like Google(r), and Yahoo!(r)
    can be managed from web2py applications.

    The code uses examples by Yuji Tomita on this post:
    http://yuji.wordpress.com/2011/06/22/python-imaplib-imap-example-with-gmail/#comment-1137
    and is based in docs for Python imaplib, python email
    and email IETF's (i.e. RFC2060 and RFC3501)

    This adapter was tested with a small set of operations with Gmail(r). Other
    services requests could raise command syntax and response data issues.

    It creates its table and field names "statically",
    meaning that the developer should leave the table and field
    definitions to the DAL instance by calling the adapter's
    .define_tables() method. The tables are defined with the
    IMAP server mailbox list information.

    .define_tables() returns a dictionary mapping dal tablenames
    to the server mailbox names with the following structure:

    {<tablename>: str <server mailbox name>}

    Here is a list of supported fields:

    ===========   ============== ===========
    Field         Type           Description
    ===========   ============== ===========
    uid           string
    answered      boolean        Flag
    created       date
    content       list:string    A list of dict text or html parts
    to            string
    cc            string
    bcc           string
    size          integer        the amount of octets of the message*
    deleted       boolean        Flag
    draft         boolean        Flag
    flagged       boolean        Flag
    sender        string
    recent        boolean        Flag
    seen          boolean        Flag
    subject       string
    mime          string         The mime header declaration
    email         string         The complete RFC822 message (*)
    attachments   list           Each non text part as dict
    encoding      string         The main detected encoding
    ===========   ============== ===========

    (*) At the application side it is measured as the length of the RFC822
    message string

    WARNING: As row id's are mapped to email sequence numbers,
    make sure your imap client web2py app does not delete messages
    during select or update actions, to prevent
    updating or deleting different messages.
    Sequence numbers change whenever the mailbox is updated.
    To avoid this sequence numbers issues, it is recommended the use
    of uid fields in query references (although the update and delete
    in separate actions rule still applies).
    ::

        # This is the code recommended to start imap support
        # at the app's model:

        imapdb = DAL("imap://user:password@server:port", pool_size=1) # port 993 for ssl
        imapdb.define_tables()

    Here is an (incomplete) list of possible imap commands::

        # Count today's unseen messages
        # smaller than 6000 octets from the
        # inbox mailbox

        q = imapdb.INBOX.seen == False
        q &= imapdb.INBOX.created == datetime.date.today()
        q &= imapdb.INBOX.size < 6000
        unread = imapdb(q).count()

        # Fetch last query messages
        rows = imapdb(q).select()

        # it is also possible to filter query select results with limitby and
        # sequences of mailbox fields

        set.select(<fields sequence>, limitby=(<int>, <int>))

        # Mark last query messages as seen
        messages = [row.uid for row in rows]
        seen = imapdb(imapdb.INBOX.uid.belongs(messages)).update(seen=True)

        # Delete messages in the imap database that have mails from mr. Gumby

        deleted = 0
        for mailbox in imapdb.tables
            deleted += imapdb(imapdb[mailbox].sender.contains("gumby")).delete()

        # It is possible also to mark messages for deletion instead of ereasing them
        # directly with set.update(deleted=True)


        # This object give access
        # to the adapter auto mailbox
        # mapped names (which native
        # mailbox has what table name)

        imapdb.mailboxes <dict> # tablename, server native name pairs

        # To retrieve a table native mailbox name use:
        imapdb.<table>.mailbox

        ### New features v2.4.1:

        # Declare mailboxes statically with tablename, name pairs
        # This avoids the extra server names retrieval

        imapdb.define_tables({"inbox": "INBOX"})

        # Selects without content/attachments/email columns will only
        # fetch header and flags

        imapdb(q).select(imapdb.INBOX.sender, imapdb.INBOX.subject)

    """
    drivers = ('imaplib',)
    types = {
            'string': str,
            'text': str,
            'date': datetime.date,
            'datetime': datetime.datetime,
            'id': long,
            'boolean': bool,
            'integer': int,
            'bigint': long,
            'blob': str,
            'list:string': str
        }

    dbengine = 'imap'

    REGEX_URI = re.compile('^(?P<user>[^:]+)(\:(?P<password>[^@]*))?@(?P<host>[^\:@]+)(\:(?P<port>[0-9]+))?$')

    def __init__(self,
                 db,
                 uri,
                 pool_size=0,
                 folder=None,
                 db_codec ='UTF-8',
                 credential_decoder=IDENTITY,
                 driver_args={},
                 adapter_args={},
                 do_connect=True,
                 after_connection=None):

        # db uri: user@example.com:password@imap.server.com:123
        # TODO: max size adapter argument for preventing large mail transfers

        self.db = db
        self.uri = uri
        if do_connect: self.find_driver(adapter_args)
        self.pool_size=pool_size
        self.folder = folder
        self.db_codec = db_codec
        self._after_connection = after_connection
        self.credential_decoder = credential_decoder
        self.driver_args = driver_args
        self.adapter_args = adapter_args
        self.mailbox_size = None
        self.static_names = None
        self.charset = sys.getfilesystemencoding()
        # imap class
        self.imap4 = None
        uri = uri.split("://")[1]

        """ MESSAGE is an identifier for sequence number"""

        self.flags = {'deleted': '\\Deleted', 'draft': '\\Draft',
                      'flagged': '\\Flagged', 'recent': '\\Recent',
                      'seen': '\\Seen', 'answered': '\\Answered'}
        self.search_fields = {
            'id': 'MESSAGE', 'created': 'DATE',
            'uid': 'UID', 'sender': 'FROM',
            'to': 'TO', 'cc': 'CC',
            'bcc': 'BCC', 'content': 'TEXT',
            'size': 'SIZE', 'deleted': '\\Deleted',
            'draft': '\\Draft', 'flagged': '\\Flagged',
            'recent': '\\Recent', 'seen': '\\Seen',
            'subject': 'SUBJECT', 'answered': '\\Answered',
            'mime': None, 'email': None,
            'attachments': None
            }

        db['_lastsql'] = ''

        m = self.REGEX_URI.match(uri)
        user = m.group('user')
        password = m.group('password')
        host = m.group('host')
        port = int(m.group('port'))
        over_ssl = False
        if port==993:
            over_ssl = True

        driver_args.update(host=host,port=port, password=password, user=user)
        def connector(driver_args=driver_args):
            # it is assumed sucessful authentication alLways
            # TODO: support direct connection and login tests
            if over_ssl:
                self.imap4 = self.driver.IMAP4_SSL
            else:
                self.imap4 = self.driver.IMAP4
            connection = self.imap4(driver_args["host"], driver_args["port"])
            data = connection.login(driver_args["user"], driver_args["password"])

            # static mailbox list
            connection.mailbox_names = None

            # dummy cursor function
            connection.cursor = lambda : True

            return connection

        self.db.define_tables = self.define_tables
        self.connector = connector
        if do_connect: self.reconnect()

    def reconnect(self, f=None, cursor=True):
        """
        IMAP4 Pool connection method

        imap connection lacks of self cursor command.
        A custom command should be provided as a replacement
        for connection pooling to prevent uncaught remote session
        closing

        """
        if getattr(self, 'connection', None) is not None:
            return
        if f is None:
            f = self.connector

        if not self.pool_size:
            self.connection = f()
            self.cursor = cursor and self.connection.cursor()
        else:
            POOLS = ConnectionPool.POOLS
            uri = self.uri
            while True:
                GLOBAL_LOCKER.acquire()
                if not uri in POOLS:
                    POOLS[uri] = []
                if POOLS[uri]:
                    self.connection = POOLS[uri].pop()
                    GLOBAL_LOCKER.release()
                    self.cursor = cursor and self.connection.cursor()
                    if self.cursor and self.check_active_connection:
                        try:
                            # check if connection is alive or close it
                            result, data = self.connection.list()
                        except:
                            # Possible connection reset error
                            # TODO: read exception class
                            self.connection = f()
                    break
                else:
                    GLOBAL_LOCKER.release()
                    self.connection = f()
                    self.cursor = cursor and self.connection.cursor()
                    break
        self.after_connection_hook()

    def get_last_message(self, tablename):
        last_message = None
        # request mailbox list to the server if needed.
        if not isinstance(self.connection.mailbox_names, dict):
            self.get_mailboxes()
        try:
            result = self.connection.select(
                self.connection.mailbox_names[tablename])
            last_message = int(result[1][0])
            # Last message must be a positive integer
            if last_message == 0:
                last_message = 1
        except (IndexError, ValueError, TypeError, KeyError):
            e = sys.exc_info()[1]
            self.db.logger.debug("Error retrieving the last mailbox" +
                         " sequence number. %s" % str(e))
        return last_message

    def get_uid_bounds(self, tablename):
        if not isinstance(self.connection.mailbox_names, dict):
            self.get_mailboxes()
        # fetch first and last messages
        # return (first, last) messages uid's
        last_message = self.get_last_message(tablename)
        result, data = self.connection.uid("search", None, "(ALL)")
        uid_list = data[0].strip().split()
        if len(uid_list) <= 0:
            return None
        else:
            return (uid_list[0], uid_list[-1])

    def convert_date(self, date, add=None, imf=False):
        if add is None:
            add = datetime.timedelta()
        """ Convert a date object to a string
        with d-Mon-Y style for IMAP or the inverse
        case

        add <timedelta> adds to the date object
        """
        months = [None, "JAN","FEB","MAR","APR","MAY","JUN",
                  "JUL", "AUG","SEP","OCT","NOV","DEC"]
        if isinstance(date, basestring):
            # Prevent unexpected date response format
            try:
                if "," in date:
                    dayname, datestring = date.split(",")
                else:
                    dayname, datestring = None, date
                date_list = datestring.strip().split()
                year = int(date_list[2])
                month = months.index(date_list[1].upper())
                day = int(date_list[0])
                hms = map(int, date_list[3].split(":"))
                return datetime.datetime(year, month, day,
                    hms[0], hms[1], hms[2]) + add
            except (ValueError, AttributeError, IndexError), e:
                self.db.logger.error("Could not parse date text: %s. %s" %
                             (date, e))
                return None
        elif isinstance(date, (datetime.date, datetime.datetime)):
            if imf: date_format = "%a, %d %b %Y %H:%M:%S %z"
            else: date_format = "%d-%b-%Y"
            return (date + add).strftime(date_format)
        else:
            return None

    @staticmethod
    def header_represent(f, r):
        from email.header import decode_header
        text, encoding = decode_header(f)[0]
        if encoding:
            text = text.decode(encoding).encode('utf-8')
        return text

    def encode_text(self, text, charset, errors="replace"):
        """ convert text for mail to unicode"""
        if text is None:
            text = ""
        else:
            if isinstance(text, str):
                if charset is None:
                    text = unicode(text, "utf-8", errors)
                else:
                    text = unicode(text, charset, errors)
            else:
                raise Exception("Unsupported mail text type %s" % type(text))
        return text.encode("utf-8")

    def get_charset(self, message):
        charset = message.get_content_charset()
        return charset

    def get_mailboxes(self):
        """ Query the mail database for mailbox names """
        if self.static_names:
            # statically defined mailbox names
            self.connection.mailbox_names = self.static_names
            return self.static_names.keys()

        mailboxes_list = self.connection.list()
        self.connection.mailbox_names = dict()
        mailboxes = list()
        x = 0
        for item in mailboxes_list[1]:
            x = x + 1
            item = item.strip()
            if not "NOSELECT" in item.upper():
                sub_items = item.split("\"")
                sub_items = [sub_item for sub_item in sub_items \
                if len(sub_item.strip()) > 0]
                # mailbox = sub_items[len(sub_items) -1]
                mailbox = sub_items[-1].strip()
                # remove unwanted characters and store original names
                # Don't allow leading non alphabetic characters
                mailbox_name = re.sub('^[_0-9]*', '', re.sub('[^_\w]','',re.sub('[/ ]','_',mailbox)))
                mailboxes.append(mailbox_name)
                self.connection.mailbox_names[mailbox_name] = mailbox

        return mailboxes

    def get_query_mailbox(self, query):
        nofield = True
        tablename = None
        attr = query
        while nofield:
            if hasattr(attr, "first"):
                attr = attr.first
                if isinstance(attr, Field):
                    return attr.tablename
                elif isinstance(attr, Query):
                    pass
                else:
                    return None
            else:
                return None
        return tablename

    def is_flag(self, flag):
        if self.search_fields.get(flag, None) in self.flags.values():
            return True
        else:
            return False

    def define_tables(self, mailbox_names=None):
        """
        Auto create common IMAP fileds

        This function creates fields definitions "statically"
        meaning that custom fields as in other adapters should
        not be supported and definitions handled on a service/mode
        basis (local syntax for Gmail(r), Ymail(r)

        Returns a dictionary with tablename, server native mailbox name
        pairs.
        """
        if mailbox_names:
            # optional statically declared mailboxes
            self.static_names = mailbox_names
        else:
            self.static_names = None
        if not isinstance(self.connection.mailbox_names, dict):
            self.get_mailboxes()

        names = self.connection.mailbox_names.keys()

        for name in names:
            self.db.define_table("%s" % name,
                Field("uid", writable=False),
                Field("created", "datetime", writable=False),
                Field("content", "text", writable=False),
                Field("to", writable=False),
                Field("cc", writable=False),
                Field("bcc", writable=False),
                Field("sender", writable=False),
                Field("size", "integer", writable=False),
                Field("subject", writable=False),
                Field("mime", writable=False),
                Field("email", "text", writable=False, readable=False),
                Field("attachments", "text", writable=False, readable=False),
                Field("encoding", writable=False),
                Field("answered", "boolean"),
                Field("deleted", "boolean"),
                Field("draft", "boolean"),
                Field("flagged", "boolean"),
                Field("recent", "boolean", writable=False),
                Field("seen", "boolean")
                )

            # Set a special _mailbox attribute for storing
            # native mailbox names
            self.db[name].mailbox = \
                self.connection.mailbox_names[name]

            # decode quoted printable
            self.db[name].to.represent = self.db[name].cc.represent = \
            self.db[name].bcc.represent = self.db[name].sender.represent = \
            self.db[name].subject.represent = self.header_represent

        # Set the db instance mailbox collections
        self.db.mailboxes = self.connection.mailbox_names
        return self.db.mailboxes

    def create_table(self, *args, **kwargs):
        # not implemented
        # but required by DAL
        pass

    def select(self, query, fields, attributes):
        """  Searches and Fetches records and return web2py rows
        """
        # move this statement elsewhere (upper-level)
        if use_common_filters(query):
            query = self.common_filter(query, [self.get_query_mailbox(query),])

        import email
        # get records from imap server with search + fetch
        # convert results to a dictionary
        tablename = None
        fetch_results = list()

        if isinstance(query, Query):
            tablename = self.get_table(query)
            mailbox = self.connection.mailbox_names.get(tablename, None)
            if mailbox is None:
                raise ValueError("Mailbox name not found: %s" % mailbox)
            else:
                # select with readonly
                result, selected = self.connection.select(mailbox, True)
                if result != "OK":
                    raise Exception("IMAP error: %s" % selected)
                self.mailbox_size = int(selected[0])
                search_query = "(%s)" % str(query).strip()
                search_result = self.connection.uid("search", None, search_query)
                # Normal IMAP response OK is assumed (change this)
                if search_result[0] == "OK":
                    # For "light" remote server responses just get the first
                    # ten records (change for non-experimental implementation)
                    # However, light responses are not guaranteed with this
                    # approach, just fewer messages.
                    limitby = attributes.get('limitby', None)
                    messages_set = search_result[1][0].split()
                    # descending order
                    messages_set.reverse()
                    if limitby is not None:
                        # TODO: orderby, asc/desc, limitby from complete message set
                        messages_set = messages_set[int(limitby[0]):int(limitby[1])]

                    # keep the requests small for header/flags
                    if any([(field.name in ["content", "size",
                                            "attachments", "email"]) for
                           field in fields]):
                        imap_fields = "(RFC822 FLAGS)"
                    else:
                        imap_fields = "(RFC822.HEADER FLAGS)"

                    if len(messages_set) > 0:
                        # create fetch results object list
                        # fetch each remote message and store it in memmory
                        # (change to multi-fetch command syntax for faster
                        # transactions)
                        for uid in messages_set:
                            # fetch the RFC822 message body
                            typ, data = self.connection.uid("fetch", uid, imap_fields)
                            if typ == "OK":
                                fr = {"message": int(data[0][0].split()[0]),
                                      "uid": long(uid),
                                      "email": email.message_from_string(data[0][1]),
                                      "raw_message": data[0][1]}
                                fr["multipart"] = fr["email"].is_multipart()
                                # fetch flags for the message
                                fr["flags"] = self.driver.ParseFlags(data[1])
                                fetch_results.append(fr)
                            else:
                                # error retrieving the message body
                                raise Exception("IMAP error retrieving the body: %s" % data)
                else:
                    raise Exception("IMAP search error: %s" % search_result[1])
        elif isinstance(query, (Expression, basestring)):
            raise NotImplementedError()
        else:
            raise TypeError("Unexpected query type")

        imapqry_dict = {}
        imapfields_dict = {}

        if len(fields) == 1 and isinstance(fields[0], SQLALL):
            allfields = True
        elif len(fields) == 0:
            allfields = True
        else:
            allfields = False
        if allfields:
            colnames = ["%s.%s" % (tablename, field) for field in self.search_fields.keys()]
        else:
            colnames = ["%s.%s" % (tablename, field.name) for field in fields]

        for k in colnames:
            imapfields_dict[k] = k

        imapqry_list = list()
        imapqry_array = list()
        for fr in fetch_results:
            attachments = []
            content = []
            size = 0
            n = int(fr["message"])
            item_dict = dict()
            message = fr["email"]
            uid = fr["uid"]
            charset = self.get_charset(message)
            flags = fr["flags"]
            raw_message = fr["raw_message"]
            # Return messages data mapping static fields
            # and fetched results. Mapping should be made
            # outside the select function (with auxiliary
            # instance methods)

            # pending: search flags states trough the email message
            # instances for correct output

            # preserve subject encoding (ASCII/quoted printable)

            if "%s.id" % tablename in colnames:
                item_dict["%s.id" % tablename] = n
            if "%s.created" % tablename in colnames:
                item_dict["%s.created" % tablename] = self.convert_date(message["Date"])
            if "%s.uid" % tablename in colnames:
                item_dict["%s.uid" % tablename] = uid
            if "%s.sender" % tablename in colnames:
                # If there is no encoding found in the message header
                # force utf-8 replacing characters (change this to
                # module's defaults). Applies to .sender, .to, .cc and .bcc fields
                item_dict["%s.sender" % tablename] = message["From"]
            if "%s.to" % tablename in colnames:
                item_dict["%s.to" % tablename] = message["To"]
            if "%s.cc" % tablename in colnames:
                if "Cc" in message.keys():
                    item_dict["%s.cc" % tablename] = message["Cc"]
                else:
                    item_dict["%s.cc" % tablename] = ""
            if "%s.bcc" % tablename in colnames:
                if "Bcc" in message.keys():
                    item_dict["%s.bcc" % tablename] = message["Bcc"]
                else:
                    item_dict["%s.bcc" % tablename] = ""
            if "%s.deleted" % tablename in colnames:
                item_dict["%s.deleted" % tablename] = "\\Deleted" in flags
            if "%s.draft" % tablename in colnames:
                item_dict["%s.draft" % tablename] = "\\Draft" in flags
            if "%s.flagged" % tablename in colnames:
                item_dict["%s.flagged" % tablename] = "\\Flagged" in flags
            if "%s.recent" % tablename in colnames:
                item_dict["%s.recent" % tablename] = "\\Recent" in flags
            if "%s.seen" % tablename in colnames:
                item_dict["%s.seen" % tablename] = "\\Seen" in flags
            if "%s.subject" % tablename in colnames:
                item_dict["%s.subject" % tablename] = message["Subject"]
            if "%s.answered" % tablename in colnames:
                item_dict["%s.answered" % tablename] = "\\Answered" in flags
            if "%s.mime" % tablename in colnames:
                item_dict["%s.mime" % tablename] = message.get_content_type()
            if "%s.encoding" % tablename in colnames:
                item_dict["%s.encoding" % tablename] = charset

            # Here goes the whole RFC822 body as an email instance
            # for controller side custom processing
            # The message is stored as a raw string
            # >> email.message_from_string(raw string)
            # returns a Message object for enhanced object processing
            if "%s.email" % tablename in colnames:
                # WARNING: no encoding performed (raw message)
                item_dict["%s.email" % tablename] = raw_message

            # Size measure as suggested in a Velocity Reviews post
            # by Tim Williams: "how to get size of email attachment"
            # Note: len() and server RFC822.SIZE reports doesn't match
            # To retrieve the server size for representation would add a new
            # fetch transaction to the process
            for part in message.walk():
                maintype = part.get_content_maintype()
                if ("%s.attachments" % tablename in colnames) or \
                   ("%s.content" % tablename in colnames):
                    payload = part.get_payload(decode=True)
                    if payload:
                        filename = part.get_filename()
                        values = {"mime": part.get_content_type()}
                        if ((filename or not "text" in maintype) and
                            ("%s.attachments" % tablename in colnames)):
                            values.update({"payload": payload,
                                "filename": filename,
                                "encoding": part.get_content_charset(),
                                "disposition": part["Content-Disposition"]})
                            attachments.append(values)
                        elif (("text" in maintype) and
                              ("%s.content" % tablename in colnames)):
                            values.update({"text": self.encode_text(payload,
                                               self.get_charset(part))})
                            content.append(values)

                if "%s.size" % tablename in colnames:
                    if part is not None:
                        size += len(str(part))
            item_dict["%s.content" % tablename] = content
            item_dict["%s.attachments" % tablename] = attachments
            item_dict["%s.size" % tablename] = size
            imapqry_list.append(item_dict)

        # extra object mapping for the sake of rows object
        # creation (sends an array or lists)
        for item_dict in imapqry_list:
            imapqry_array_item = list()
            for fieldname in colnames:
                imapqry_array_item.append(item_dict[fieldname])
            imapqry_array.append(imapqry_array_item)

        # parse result and return a rows object
        colnames = colnames
        processor = attributes.get('processor',self.parse)
        return processor(imapqry_array, fields, colnames)

    def insert(self, table, fields):
        def add_payload(message, obj):
            payload = Message()
            encoding = obj.get("encoding", "utf-8")
            if encoding and (encoding.upper() in
                             ("BASE64", "7BIT", "8BIT", "BINARY")):
                payload.add_header("Content-Transfer-Encoding", encoding)
            else:
                payload.set_charset(encoding)
            mime = obj.get("mime", None)
            if mime:
                payload.set_type(mime)
            if "text" in obj:
                payload.set_payload(obj["text"])
            elif "payload" in obj:
                payload.set_payload(obj["payload"])
            if "filename" in obj and obj["filename"]:
                payload.add_header("Content-Disposition",
                    "attachment", filename=obj["filename"])
            message.attach(payload)

        mailbox = table.mailbox
        d = dict(((k.name, v) for k, v in fields))
        date_time = d.get("created") or datetime.datetime.now()
        struct_time = date_time.timetuple()
        if len(d) > 0:
            message = d.get("email", None)
            attachments = d.get("attachments", [])
            content = d.get("content", [])
            flags = " ".join(["\\%s" % flag.capitalize() for flag in
                     ("answered", "deleted", "draft", "flagged",
                      "recent", "seen") if d.get(flag, False)])
            if not message:
                from email.message import Message
                mime = d.get("mime", None)
                charset = d.get("encoding", None)
                message = Message()
                message["from"] = d.get("sender", "")
                message["subject"] = d.get("subject", "")
                message["date"] = self.convert_date(date_time, imf=True)

                if mime:
                    message.set_type(mime)
                if charset:
                    message.set_charset(charset)
                for item in ("to", "cc", "bcc"):
                    value = d.get(item, "")
                    if isinstance(value, basestring):
                        message[item] = value
                    else:
                        message[item] = ";".join([i for i in
                            value])
                if (not message.is_multipart() and
                   (not message.get_content_type().startswith(
                        "multipart"))):
                    if isinstance(content, basestring):
                        message.set_payload(content)
                    elif len(content) > 0:
                        message.set_payload(content[0]["text"])
                else:
                    [add_payload(message, c) for c in content]
                    [add_payload(message, a) for a in attachments]
                message = message.as_string()

            result, data = self.connection.append(mailbox, flags, struct_time, message)
            if result == "OK":
                uid = int(re.findall("\d+", str(data))[-1])
                return self.db(table.uid==uid).select(table.id).first().id
            else:
                raise Exception("IMAP message append failed: %s" % data)
        else:
            raise NotImplementedError("IMAP empty insert is not implemented")

    def update(self, tablename, query, fields):
        # TODO: the adapter should implement an .expand method
        commands = list()
        rowcount = 0
        if use_common_filters(query):
            query = self.common_filter(query, [tablename,])
        mark = []
        unmark = []
        if query:
            for item in fields:
                field = item[0]
                name = field.name
                value = item[1]
                if self.is_flag(name):
                    flag = self.search_fields[name]
                    if (value is not None) and (flag != "\\Recent"):
                        if value:
                            mark.append(flag)
                        else:
                            unmark.append(flag)
            result, data = self.connection.select(
                self.connection.mailbox_names[tablename])
            string_query = "(%s)" % query
            result, data = self.connection.search(None, string_query)
            store_list = [item.strip() for item in data[0].split()
                          if item.strip().isdigit()]
            # build commands for marked flags
            for number in store_list:
                result = None
                if len(mark) > 0:
                    commands.append((number, "+FLAGS", "(%s)" % " ".join(mark)))
                if len(unmark) > 0:
                    commands.append((number, "-FLAGS", "(%s)" % " ".join(unmark)))

        for command in commands:
            result, data = self.connection.store(*command)
            if result == "OK":
                rowcount += 1
            else:
                raise Exception("IMAP storing error: %s" % data)
        return rowcount

    def count(self,query,distinct=None):
        counter = 0
        tablename = self.get_query_mailbox(query)
        if query and tablename is not None:
            if use_common_filters(query):
                query = self.common_filter(query, [tablename,])
            result, data = self.connection.select(self.connection.mailbox_names[tablename])
            string_query = "(%s)" % query
            result, data = self.connection.search(None, string_query)
            store_list = [item.strip() for item in data[0].split() if item.strip().isdigit()]
            counter = len(store_list)
        return counter

    def delete(self, tablename, query):
        counter = 0
        if query:
            if use_common_filters(query):
                query = self.common_filter(query, [tablename,])
            result, data = self.connection.select(self.connection.mailbox_names[tablename])
            string_query = "(%s)" % query
            result, data = self.connection.search(None, string_query)
            store_list = [item.strip() for item in data[0].split() if item.strip().isdigit()]
            for number in store_list:
                result, data = self.connection.store(number, "+FLAGS", "(\\Deleted)")
                if result == "OK":
                    counter += 1
                else:
                    raise Exception("IMAP store error: %s" % data)
            if counter > 0:
                result, data = self.connection.expunge()
        return counter

    def BELONGS(self, first, second):
        result = None
        name = self.search_fields[first.name]
        if name == "MESSAGE":
            values = [str(val) for val in second if str(val).isdigit()]
            result = "%s" % ",".join(values).strip()

        elif name == "UID":
            values = [str(val) for val in second if str(val).isdigit()]
            result = "UID %s" % ",".join(values).strip()

        else:
            raise Exception("Operation not supported")
        # result = "(%s %s)" % (self.expand(first), self.expand(second))
        return result

    def CONTAINS(self, first, second, case_sensitive=False):
        # silently ignore, only case sensitive
        result = None
        name = self.search_fields[first.name]

        if name in ("FROM", "TO", "SUBJECT", "TEXT"):
            result = "%s \"%s\"" % (name, self.expand(second))
        else:
            if first.name in ("cc", "bcc"):
                result = "%s \"%s\"" % (first.name.upper(), self.expand(second))
            elif first.name == "mime":
                result = "HEADER Content-Type \"%s\"" % self.expand(second)
            else:
                raise Exception("Operation not supported")
        return result

    def GT(self, first, second):
        result = None
        name = self.search_fields[first.name]
        if name == "MESSAGE":
            last_message = self.get_last_message(first.tablename)
            result = "%d:%d" % (int(self.expand(second)) + 1, last_message)
        elif name == "UID":
            # GT and LT may not return
            # expected sets depending on
            # the uid format implemented
            try:
                pedestal, threshold = self.get_uid_bounds(first.tablename)
            except TypeError:
                e = sys.exc_info()[1]
                self.db.logger.debug("Error requesting uid bounds: %s", str(e))
                return ""
            try:
                lower_limit = int(self.expand(second)) + 1
            except (ValueError, TypeError):
                e = sys.exc_info()[1]
                raise Exception("Operation not supported (non integer UID)")
            result = "UID %s:%s" % (lower_limit, threshold)
        elif name == "DATE":
            result = "SINCE %s" % self.convert_date(second, add=datetime.timedelta(1))
        elif name == "SIZE":
            result = "LARGER %s" % self.expand(second)
        else:
            raise Exception("Operation not supported")
        return result

    def GE(self, first, second):
        result = None
        name = self.search_fields[first.name]
        if name == "MESSAGE":
            last_message = self.get_last_message(first.tablename)
            result = "%s:%s" % (self.expand(second), last_message)
        elif name == "UID":
            # GT and LT may not return
            # expected sets depending on
            # the uid format implemented
            try:
                pedestal, threshold = self.get_uid_bounds(first.tablename)
            except TypeError:
                e = sys.exc_info()[1]
                self.db.logger.debug("Error requesting uid bounds: %s", str(e))
                return ""
            lower_limit = self.expand(second)
            result = "UID %s:%s" % (lower_limit, threshold)
        elif name == "DATE":
            result = "SINCE %s" % self.convert_date(second)
        else:
            raise Exception("Operation not supported")
        return result

    def LT(self, first, second):
        result = None
        name = self.search_fields[first.name]
        if name == "MESSAGE":
            result = "%s:%s" % (1, int(self.expand(second)) - 1)
        elif name == "UID":
            try:
                pedestal, threshold = self.get_uid_bounds(first.tablename)
            except TypeError:
                e = sys.exc_info()[1]
                self.db.logger.debug("Error requesting uid bounds: %s", str(e))
                return ""
            try:
                upper_limit = int(self.expand(second)) - 1
            except (ValueError, TypeError):
                e = sys.exc_info()[1]
                raise Exception("Operation not supported (non integer UID)")
            result = "UID %s:%s" % (pedestal, upper_limit)
        elif name == "DATE":
            result = "BEFORE %s" % self.convert_date(second)
        elif name == "SIZE":
            result = "SMALLER %s" % self.expand(second)
        else:
            raise Exception("Operation not supported")
        return result

    def LE(self, first, second):
        result = None
        name = self.search_fields[first.name]
        if name == "MESSAGE":
            result = "%s:%s" % (1, self.expand(second))
        elif name == "UID":
            try:
                pedestal, threshold = self.get_uid_bounds(first.tablename)
            except TypeError:
                e = sys.exc_info()[1]
                self.db.logger.debug("Error requesting uid bounds: %s", str(e))
                return ""
            upper_limit = int(self.expand(second))
            result = "UID %s:%s" % (pedestal, upper_limit)
        elif name == "DATE":
            result = "BEFORE %s" % self.convert_date(second, add=datetime.timedelta(1))
        else:
            raise Exception("Operation not supported")
        return result

    def NE(self, first, second=None):
        if (second is None) and isinstance(first, Field):
            # All records special table query
            if first.type == "id":
                return self.GE(first, 1)
        result = self.NOT(self.EQ(first, second))
        result =  result.replace("NOT NOT", "").strip()
        return result

    def EQ(self,first,second):
        name = self.search_fields[first.name]
        result = None
        if name is not None:
            if name == "MESSAGE":
                # query by message sequence number
                result = "%s" % self.expand(second)
            elif name == "UID":
                result = "UID %s" % self.expand(second)
            elif name == "DATE":
                result = "ON %s" % self.convert_date(second)

            elif name in self.flags.values():
                if second:
                    result = "%s" % (name.upper()[1:])
                else:
                    result = "NOT %s" % (name.upper()[1:])
            else:
                raise Exception("Operation not supported")
        else:
            raise Exception("Operation not supported")
        return result

    def AND(self, first, second):
        result = "%s %s" % (self.expand(first), self.expand(second))
        return result

    def OR(self, first, second):
        result = "OR %s %s" % (self.expand(first), self.expand(second))
        return "%s" % result.replace("OR OR", "OR")

    def NOT(self, first):
        result = "NOT %s" % self.expand(first)
        return result
