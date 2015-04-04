#!/usr/bin/env python

import time
from hashlib import md5
from gluon.dal import DAL


def motp_auth(db=DAL('sqlite://storage.sqlite'),
              time_offset=60):

    """
    motp allows you to login with a one time password(OTP) generated on a motp client,
    motp clients are available for practically all platforms.
    to know more about OTP visit http://en.wikipedia.org/wiki/One-time_password
    to know more visit http://motp.sourceforge.net


    Written by Madhukar R Pai (madspai@gmail.com)
    License : MIT or GPL v2

    thanks and credits to the web2py community

    to use motp_auth:
    motp_auth.py has to be located in gluon/contrib/login_methods/ folder
    first auth_user has to have 2 extra fields - motp_secret and motp_pin
    for that define auth like shown below:

    ## after auth = Auth(db)
    db.define_table(
        auth.settings.table_user_name,
        Field('first_name', length=128, default=''),
        Field('last_name', length=128, default=''),
        Field('email', length=128, default='', unique=True), # required
        Field('password', 'password', length=512,            # required
              readable=False, label='Password'),
        Field('motp_secret',length=512,default='',
              label='MOTP Seceret'),
        Field('motp_pin',length=128,default='',
              label='MOTP PIN'),
        Field('registration_key', length=512,                # required
              writable=False, readable=False, default=''),
        Field('reset_password_key', length=512,              # required
              writable=False, readable=False, default=''),
        Field('registration_id', length=512,                 # required
              writable=False, readable=False, default=''))

    ##validators
    custom_auth_table = db[auth.settings.table_user_name]
        # get the custom_auth_table
    custom_auth_table.first_name.requires = \
      IS_NOT_EMPTY(error_message=auth.messages.is_empty)
    custom_auth_table.last_name.requires = \
      IS_NOT_EMPTY(error_message=auth.messages.is_empty)
    custom_auth_table.password.requires = CRYPT()
    custom_auth_table.email.requires = [
      IS_EMAIL(error_message=auth.messages.invalid_email),
      IS_NOT_IN_DB(db, custom_auth_table.email)]

    auth.settings.table_user = custom_auth_table # tell auth to use custom_auth_table
    ## before auth.define_tables()

    ##after that:

    from gluon.contrib.login_methods.motp_auth import motp_auth
    auth.settings.login_methods.append(motp_auth(db=db))

    ##Instructions for using MOTP
    - after configuring motp for web2py, Install a MOTP client on your phone (android,IOS, java, windows phone, etc)
    - initialize the motp client (to reset a motp secret type in #**#),
      During user creation enter the secret generated during initialization into the motp_secret field in auth_user and
      similarly enter a pre-decided pin into the motp_pin
    - done.. to login, just generate a fresh OTP by typing in the pin and use the OTP as password

    ###To Dos###
    - both motp_secret and pin are stored in plain text! need to have some way of encrypting
    - web2py stores the password in db on successful login (should not happen)
    - maybe some utility or page to check the otp would be useful
    - as of now user field is hardcoded to email. Some way of selecting user table and user field.
    """

    def verify_otp(otp, pin, secret, offset=60):
        epoch_time = int(time.time())
        time_start = int(str(epoch_time - offset)[:-1])
        time_end = int(str(epoch_time + offset)[:-1])
        for t in range(time_start - 1, time_end + 1):
            to_hash = str(t) + secret + pin
            hash = md5(to_hash).hexdigest()[:6]
            if otp == hash:
                return True
        return False

    def motp_auth_aux(email,
                      password,
                      db=db,
                      offset=time_offset):
        if db:
            user_data = db(db.auth_user.email == email).select().first()
            if user_data:
                if user_data['motp_secret'] and user_data['motp_pin']:
                    motp_secret = user_data['motp_secret']
                    motp_pin = user_data['motp_pin']
                    otp_check = verify_otp(
                        password, motp_pin, motp_secret, offset=offset)
                    if otp_check:
                        return True
                    else:
                        return False
                else:
                    return False
        return False
    return motp_auth_aux
