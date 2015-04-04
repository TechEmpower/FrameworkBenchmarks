"""
AIM class to credit card payment with authorize.net

Fork of authnet code written by John Conde
http://www.johnconde.net/blog/integrate-the-authorizenet-aim-api-with-python-3-2/
BSDv3 License

Modifed by Massimo Di Pierro

- ported from Python 3.x run on Python 2.4+
- fixed a couple of bugs
- merged with test so single file
- namedtuple from http://code.activestate.com/recipes/500261/

"""

__all__ = ['AIM']

from operator import itemgetter
import urllib

_known_tuple_types = {}


class NamedTupleBase(tuple):
    """Base class for named tuples with the __new__ operator set, named tuples
       yielded by the namedtuple() function will subclass this and add
       properties."""
    def __new__(cls, *args, **kws):
        """Create a new instance of this fielded tuple"""
        # May need to unpack named field values here
        if kws:
            values = list(args) + [None] * (len(cls._fields) - len(args))
            fields = dict((val, idx) for idx, val in enumerate(cls._fields))
            for kw, val in kws.iteritems():
                assert kw in kws, "%r not in field list" % kw
                values[fields[kw]] = val
            args = tuple(values)
        return tuple.__new__(cls, args)


def namedtuple(typename, fieldnames):
    """
    >>> import namedtuples
    >>> tpl = namedtuples.namedtuple(['a', 'b', 'c'])
    >>> tpl(1, 2, 3)
    (1, 2, 3)
    >>> tpl(1, 2, 3).b
    2
    >>> tpl(c=1, a=2, b=3)
    (2, 3, 1)
    >>> tpl(c=1, a=2, b=3).b
    3
    >>> tpl(c='pads with nones')
    (None, None, 'pads with nones')
    >>> tpl(b='pads with nones')
    (None, 'pads with nones', None)
    >>>
    """
    # Split up a string, some people do this
    if isinstance(fieldnames, basestring):
        fieldnames = fieldnames.replace(',', ' ').split()
    # Convert anything iterable that enumerates fields to a tuple now
    fieldname_tuple = tuple(str(field) for field in fieldnames)
    # See if we've cached this
    if fieldname_tuple in _known_tuple_types:
        return _known_tuple_types[fieldname_tuple]
    # Make the type
    new_tuple_type = type(typename, (NamedTupleBase,), {})
    # Set the hidden field
    new_tuple_type._fields = fieldname_tuple
    # Add the getters
    for i, field in enumerate(fieldname_tuple):
        setattr(new_tuple_type, field, property(itemgetter(i)))
    # Cache
    _known_tuple_types[fieldname_tuple] = new_tuple_type
    # Done
    return new_tuple_type


class AIM:

    class AIMError(Exception):
        def __init__(self, value):
            self.parameter = value

        def __str__(self):
            return str(self.parameter)

    def __init__(self, login, transkey, testmode=False):
        if str(login).strip() == '' or login is None:
            raise AIM.AIMError('No login name provided')
        if str(transkey).strip() == '' or transkey is None:
            raise AIM.AIMError('No transaction key provided')
        if testmode != True and testmode != False:
            raise AIM.AIMError('Invalid value for testmode. Must be True or False. "{0}" given.'.format(testmode))

        self.testmode = testmode
        self.proxy = None
        self.delimiter = '|'
        self.results = []
        self.error = True
        self.success = False
        self.declined = False

        self.parameters = {}
        self.setParameter('x_delim_data', 'true')
        self.setParameter('x_delim_char', self.delimiter)
        self.setParameter('x_relay_response', 'FALSE')
        self.setParameter('x_url', 'FALSE')
        self.setParameter('x_version', '3.1')
        self.setParameter('x_method', 'CC')
        self.setParameter('x_type', 'AUTH_CAPTURE')
        self.setParameter('x_login', login)
        self.setParameter('x_tran_key', transkey)

    def process(self):
        encoded_args = urllib.urlencode(self.parameters)
        if self.testmode == True:
            url = 'https://test.authorize.net/gateway/transact.dll'
        else:
            url = 'https://secure.authorize.net/gateway/transact.dll'

        if self.proxy is None:
            self.results += str(urllib.urlopen(
                url, encoded_args).read()).split(self.delimiter)
        else:
            opener = urllib.FancyURLopener(self.proxy)
            opened = opener.open(url, encoded_args)
            try:
                self.results += str(opened.read()).split(self.delimiter)
            finally:
                opened.close()
        Results = namedtuple('Results', 'ResultResponse ResponseSubcode ResponseCode ResponseText AuthCode \
                                          AVSResponse TransactionID InvoiceNumber Description Amount PaymentMethod \
                                          TransactionType CustomerID CHFirstName CHLastName Company BillingAddress \
                                          BillingCity BillingState BillingZip BillingCountry Phone Fax Email ShippingFirstName \
                                          ShippingLastName ShippingCompany ShippingAddress ShippingCity ShippingState \
                                          ShippingZip ShippingCountry TaxAmount DutyAmount FreightAmount TaxExemptFlag \
                                          PONumber MD5Hash CVVResponse CAVVResponse')
        self.response = Results(*tuple(r for r in self.results)[0:40])

        if self.getResultResponseFull() == 'Approved':
            self.error = False
            self.success = True
            self.declined = False
        elif self.getResultResponseFull() == 'Declined':
            self.error = False
            self.success = False
            self.declined = True
        else:
            raise AIM.AIMError(self.response.ResponseText)

    def setTransaction(self, creditcard, expiration, total, cvv=None, tax=None, invoice=None):
        if str(creditcard).strip() == '' or creditcard is None:
            raise AIM.AIMError('No credit card number passed to setTransaction(): {0}'.format(creditcard))
        if str(expiration).strip() == '' or expiration is None:
            raise AIM.AIMError('No expiration number to setTransaction(): {0}'.format(expiration))
        if str(total).strip() == '' or total is None:
            raise AIM.AIMError('No total amount passed to setTransaction(): {0}'.format(total))

        self.setParameter('x_card_num', creditcard)
        self.setParameter('x_exp_date', expiration)
        self.setParameter('x_amount', total)
        if cvv is not None:
            self.setParameter('x_card_code', cvv)
        if tax is not None:
            self.setParameter('x_tax', tax)
        if invoice is not None:
            self.setParameter('x_invoice_num', invoice)

    def setTransactionType(self, transtype=None):
        types = ['AUTH_CAPTURE', 'AUTH_ONLY', 'PRIOR_AUTH_CAPTURE',
                 'CREDIT', 'CAPTURE_ONLY', 'VOID']
        if transtype.upper() not in types:
            raise AIM.AIMError('Incorrect Transaction Type passed to setTransactionType(): {0}'.format(transtype))
        self.setParameter('x_type', transtype.upper())

    def setProxy(self, proxy=None):
        if str(proxy).strip() == '' or proxy is None:
            raise AIM.AIMError('No proxy passed to setProxy()')
        self.proxy = {'http': str(proxy).strip()}

    def setParameter(self, key=None, value=None):
        if key is not None and value is not None and str(key).strip() != '' and str(value).strip() != '':
            self.parameters[key] = str(value).strip()
        else:
            raise AIM.AIMError('Incorrect parameters passed to setParameter(): {0}:{1}'.format(key, value))

    def isApproved(self):
        return self.success

    def isDeclined(self):
        return self.declined

    def isError(self):
        return self.error

    def getResultResponseFull(self):
        responses = ['', 'Approved', 'Declined', 'Error']
        return responses[int(self.results[0])]


def process(creditcard, expiration, total, cvv=None, tax=None, invoice=None,
            login='cnpdev4289', transkey='SR2P8g4jdEn7vFLQ', testmode=True):
    payment = AIM(login, transkey, testmode)
    expiration = expiration.replace('/', '')
    payment.setTransaction(creditcard, expiration, total, cvv, tax, invoice)
    try:
        payment.process()
        return payment.isApproved()
    except AIM.AIMError:
        return False


def test():
    import socket
    import sys
    from time import time

    creditcard = '4427802641004797'
    expiration = '122012'
    total = '1.00'
    cvv = '123'
    tax = '0.00'
    invoice = str(time())[4:10]  # get a random invoice number

    try:
        payment = AIM('cnpdev4289', 'SR2P8g4jdEn7vFLQ', True)
        payment.setTransaction(
            creditcard, expiration, total, cvv, tax, invoice)
        payment.setParameter(
            'x_duplicate_window', 180)  # three minutes duplicate windows
        payment.setParameter('x_cust_id', '1324')       # customer ID
        payment.setParameter('x_first_name', 'John')
        payment.setParameter('x_last_name', 'Conde')
        payment.setParameter('x_company', 'Test Company')
        payment.setParameter('x_address', '1234 Main Street')
        payment.setParameter('x_city', 'Townsville')
        payment.setParameter('x_state', 'NJ')
        payment.setParameter('x_zip', '12345')
        payment.setParameter('x_country', 'US')
        payment.setParameter('x_phone', '800-555-1234')
        payment.setParameter('x_description', 'Test Transaction')
        payment.setParameter(
            'x_customer_ip', socket.gethostbyname(socket.gethostname()))
        payment.setParameter('x_email', 'john@example.com')
        payment.setParameter('x_email_customer', False)
        payment.process()
        if payment.isApproved():
            print 'Response Code: ', payment.response.ResponseCode
            print 'Response Text: ', payment.response.ResponseText
            print 'Response: ', payment.getResultResponseFull()
            print 'Transaction ID: ', payment.response.TransactionID
            print 'CVV Result: ', payment.response.CVVResponse
            print 'Approval Code: ', payment.response.AuthCode
            print 'AVS Result: ', payment.response.AVSResponse
        elif payment.isDeclined():
            print 'Your credit card was declined by your bank'
        elif payment.isError():
            raise AIM.AIMError('An uncaught error occurred')
    except AIM.AIMError, e:
        print "Exception thrown:", e
        print 'An error occured'
    print 'approved', payment.isApproved()
    print 'declined', payment.isDeclined()
    print 'error', payment.isError()

if __name__ == '__main__':
    test()
