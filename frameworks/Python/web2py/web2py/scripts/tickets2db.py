#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import os
import time
import stat
import datetime

from gluon.utils import md5_hash
from gluon.restricted import RestrictedError, TicketStorage
from gluon import DAL

SLEEP_MINUTES = 5

errors_path = os.path.join(request.folder, 'errors')
try:
    db_string = open(os.path.join(request.folder, 'private', 'ticket_storage.txt')).read().replace('\r', '').replace('\n', '').strip()
except:
    db_string = 'sqlite://storage.db'

db_path = os.path.join(request.folder, 'databases')

tk_db = DAL(db_string, folder=db_path, auto_import=True)
ts = TicketStorage(db=tk_db)
tk_table = ts._get_table(
    db=tk_db, tablename=ts.tablename, app=request.application)


hashes = {}

while 1:
    if request.tickets_db:
        print "You're storing tickets yet in database"
        sys.exit(1)

    for file in os.listdir(errors_path):
        filename = os.path.join(errors_path, file)

        modified_time = os.stat(filename)[stat.ST_MTIME]
        modified_time = datetime.datetime.fromtimestamp(modified_time)
        ticket_id = file
        ticket_data = open(filename).read()
        tk_table.insert(ticket_id=ticket_id,
                        ticket_data=ticket_data,
                        created_datetime=modified_time
                        )
        tk_db.commit()
        os.unlink(filename)

    time.sleep(SLEEP_MINUTES * 60)
