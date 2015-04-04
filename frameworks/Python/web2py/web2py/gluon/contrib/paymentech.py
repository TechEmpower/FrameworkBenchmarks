#!/usr/bin/env python
# -*- coding: utf-8 -*-

# This module provides a simple API for Paymentech(c) payments
# The original code was taken from this web2py issue post
# http://code.google.com/p/web2py/issues/detail?id=1170 by Adnan Smajlovic
#
#    Copyright (C) <2012>  Alan Etkin <spametki@gmail.com>
#    License: BSD
#

import sys, httplib, urllib, urllib2
from xml.dom.minidom import parseString

# TODO: input validation, test, debugging output

class PaymenTech(object):
    """
    The base class for connecting to the Paymentech service

    Format notes
    ============

    - Credit card expiration date (exp argument) must be of mmyyyy form
    - The amount is an all integers string with two decimal places:
      For example, $2.15 must be formatted as "215"

    Point of sale and service options (to be passed on initialization)
    ==================================================================

    user
    password
    industry
    message
    bin_code
    merchant
    terminal

    (WARNING!: this is False by default)
    development <bool>

    (the following arguments have default values)
    target
    host
    api_url


    Testing
    =======

    As this module consumes webservice methods, it should be tested
    with particular user data with the paymentech development environment

    The simplest test would be running something like the following:

    from paymentech import PaymenTech
    # Read the basic point of sale argument list required above
    # Remember to use development = True!
    pos_data = {'user': <username>, ...}

    # The data arguments are documented in the .charge() method help
    charge_test = {'account': <account>, ...}
    mypayment = PaymentTech(**pos_data)
    result = mypayment.charge(**charge_test)

    print "##################################"
    print "#       Charge test result       #"
    print "##################################"
    print result

    #################################################################
    #               Notes for web2py implementations                #
    #################################################################

    # A recommended model for handling payments

    # Store this constants in a private model file (i.e. 0_private.py)

    PAYMENTECH_USER = <str>
    PAYMENTECH_PASSWORD = <str>
    PAYMENTECH_INDUSTRY = <str>
    PAYMENTECH_MESSAGE = <str>
    PAYMENTECH_BIN_CODE= <str>
    PAYMENTECH_MERCHANT = <str>
    PAYMENTECH_terminal = <str>
    DEVELOPMENT = True
    PAYMENTECH_TARGET = <str>
    PAYMENTECH_HOST = <str>
    PAYMENTECH_API_URL = <str>

    # The following table would allow passing data with web2py and to
    # update records with the webservice authorization output by using
    # the DAL
    #
    #   For example:
    #
    #   # Create a PaymenTech instance
    #   mypaymentech = paymentech.PaymenTech(user=PAYMENTECH_USER, ...)
    #
    #   # Fetch a payment inserted within the app
    #   myrow = db.paymentech[<id>]
    #
    #   # Send the authorization request to the webservice
    #   result = mypaymentech.charge(myrow.as_dict())
    #
    #   # Update the db record with the webservice response
    #   myrow.update_record(**result)

    db.define_table("paymentech",
        Field("account"),
        Field("exp", comment="Must be of the mmyyyy form"),
        Field("currency_code"),
        Field("currency_exponent"),
        Field("card_sec_val_ind"),
        Field("card_sec_val"),
        Field("avs_zip"),
        Field("avs_address_1"),
        Field("avs_address_2"),
        Field("avs_city"),
        Field("avs_state"),
        Field("avs_phone"),
        Field("avs_country"),
        Field("profile_from_order_ind"),
        Field("profile_order_override_ind"),
        Field("order_id"),
        Field("amount",
              comment="all integers with two decimal digits, \
                                 without dot separation"),
        Field("header"),
        Field("status_code"),
        Field("status_message"),
        Field("resp_code"),
        Field("tx_ref_num"),
        format="%(order_id)s")

    TODO: add model form validators (for exp date and amount)
    """

    charge_xml = """
        <?xml version="1.0" encoding="UTF-8"?>
        <Request>
            <NewOrder>
                <OrbitalConnectionUsername>%(user)s</OrbitalConnectionUsername>
                <OrbitalConnectionPassword>%(password)s</OrbitalConnectionPassword>
                <IndustryType>%(industry)s</IndustryType>
                <MessageType>%(message)s</MessageType>
                <BIN>%(bin)s</BIN>
                <MerchantID>%(merchant)s</MerchantID>
                <TerminalID>%(terminal)s</TerminalID>
                <AccountNum>%(account)s</AccountNum>
                <Exp>%(exp)s</Exp>
                <CurrencyCode>%(currency_code)s</CurrencyCode>
                <CurrencyExponent>%(currency_exponent)s</CurrencyExponent>
                <CardSecValInd>%(card_sec_val_ind)s</CardSecValInd>
                <CardSecVal>%(card_sec_val)s</CardSecVal>
                <AVSzip>%(avs_zip)s</AVSzip>
                <AVSaddress1>%(avs_address_1)s</AVSaddress1>
                <AVSaddress2>%(avs_address_2)s</AVSaddress2>
                <AVScity>%(avs_city)s</AVScity>
                <AVSstate>%(avs_state)s</AVSstate>
                <AVSphoneNum>%(avs_phone)s</AVSphoneNum>
                <AVScountryCode>%(avs_country)s</AVScountryCode>
                <CustomerProfileFromOrderInd>%(profile_from_order_ind)s</CustomerProfileFromOrderInd>
                <CustomerProfileOrderOverrideInd>%(profile_order_override_ind)s</CustomerProfileOrderOverrideInd>
                <OrderID>%(order_id)s</OrderID>
                <Amount>%(amount)s</Amount>
            </NewOrder>
        </Request>
    """

    def __init__(self, development=False, user=None, password=None,
               industry=None, message=None, api_url=None,
               bin_code=None, merchant=None, host=None,
               terminal=None, target=None):

        # PaymenTech point of sales data
        self.user = user
        self.password = password
        self.industry = industry
        self.message = message
        self.bin_code = bin_code
        self.merchant = merchant
        self.terminal = terminal

        # Service options
        self.development = development
        self.target = target
        self.host = host
        self.api_url = api_url

        # dev: https://orbitalvar1.paymentech.net/authorize:443
        # prod: https://orbital1.paymentech.net/authorize

        if self.development is False:
            if not self.target:
                # production
                self.target = "https://orbital1.paymentech.net/authorize"

            self.host, self.api_url = \
                urllib2.splithost(urllib2.splittype(self.target)[1])

        else:
            if not self.target:
                # development
                self.target = "https://orbitalvar1.paymentech.net/authorize"
            if not self.host:
                self.host = "orbitalvar1.paymentech.net/authorize:443"
            if not self.api_url:
                self.api_url = "/"

    def charge(self, raw=None, **kwargs):
        """
        Post an XML request to Paymentech
        This is an example of a call with raw xml data:

             from paymentech import PaymenTech

             # Note: user/password/etc data is not mandatory as it
             # is retrieved from instance attributes (set on init)

             pt = PaymenTech(user="<myuser>",
                             password="<mypassword>",
                             ...) # see basic user in the class help
             result = pt.charge(raw=xml_string)

        A better way to make a charge request is to unpack a dict object
        with the operation data:

             ...
             # The complete input values are listed below in
             # "Transacion data..."

             charge_data = dict(account=<str>, exp=<str mmyyyy>, ...)
             result = pt.charge(**charge_data)


        Variable xml_string contains all details about the order,
        plus we are sending username/password in there too...


        Transaction data (to be passed to the charge() method)
        ======================================================

        (Note that it is possible to override the class user,
        pass, etc. passing those arguments to the .charge() method,
        which are documented in the class help)

        account
        exp <str mmyyyy>
        currency_code
        currency_exponent
        card_sec_val_ind
        card_sec_val
        avs_zip
        avs_address_1
        avs_address_2
        avs_city
        avs_state
        avs_phone
        avs_country
        profile_from_order_ind
        profile_order_override_ind
        order_id
        amount <str> (all integers with two decimal digits, without dot
                     separation)

        Request header example
        ======================

        Request: sent as POST to https://orbitalvar1.paymentech.net/authorize:443
        from 127.0.0.1
        request headers:
        Content-Type: application/PTI45
        Content-Type: application/PTI46
        Content-transfer-encoding: text
        Request-number: 1
        Document-type: Request
        Trace-number: 1234556446
        <?xml version="1.0" encoding="UTF-8"?>
        """

        # default charge data
        data = dict(user=self.user, password=self.password,
                    industry=self.industry, message=self.message,
                    bin_code=self.bin_code, merchant=self.merchant,
                    terminal=self.terminal, account="", exp="",
                    currency_code="", currency_exponent="",
                    card_sec_val_ind="", card_sec_val="", avs_zip="",
                    avs_address_1="", avs_address_2="", avs_city="",
                    avs_state="", avs_phone="", avs_country="",
                    profile_from_order_ind="",
                    profile_order_override_ind="", order_id="",
                    amount="")

        result = dict()

        # Complete the charge request with the method kwargs
        for k, v in kwargs.iteritems():
            data[k] = v

        status_code = status_message = header = resp_code = \
        tx_ref_num = order_id = None
        conn = httplib.HTTPS(self.host)
        conn.putrequest('POST', self.api_url)

        if self.development:
            content_type = "PTI56"
        else:
            content_type = "PTI46"

        if raw is None:
            xml_string = self.charge_xml % data
        else:
            xml_string = raw

        conn.putheader("Content-Type",
                       "application/%s") % content_type
        conn.putheader("Content-transfer-encoding", "text")
        conn.putheader("Request-number", "1")
        conn.putheader("Content-length", str(len(xml_string)))
        conn.putheader("Document-type", "Request")
        conn.putheader("Trace-number", str(data["order_id"]))
        conn.putheader("MIME-Version", "1.0")
        conn.endheaders()
        conn.send(xml_string)

        result["status_code"], result["status_message"], \
        result["header"] = conn.getreply()

        fp = conn.getfile()
        output = fp.read()
        fp.close()

        dom = parseString(output)
        result["resp_code"] = \
            dom.getElementsByTagName('RespCode')[0].firstChild.data
        result["tx_ref_num"] = \
            dom.getElementsByTagName('TxRefNum')[0].firstChild.data
        result["order_id"] = \
            dom.getElementsByTagName('CustomerRefNum')[0].firstChild.data

        return result
