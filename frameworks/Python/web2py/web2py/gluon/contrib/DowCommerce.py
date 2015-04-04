"""
DowCommerce class to process credit card payments with DowCommerce.com

Modifications to support Dow Commerce API from code originally written by John Conde
http://www.johnconde.net/blog/integrate-the-authorizenet-aim-api-with-python-3-2/
BSDv3 License

Modifed by Dave Stoll dave.stoll@gmail.com

- modifed to support Dow Commerce API
"""

__all__ = ['DowCommerce']

from operator import itemgetter
import urllib


class DowCommerce:

    class DowCommerceError(Exception):
        def __init__(self, value):
            self.parameter = value

        def __str__(self):
            return str(self.parameter)

    def __init__(self, username=None, password=None, demomode=False):
        if not demomode:
            if str(username).strip() == '' or username is None:
                raise DowCommerce.DowCommerceError('No username provided')
            if str(password).strip() == '' or password is None:
                raise DowCommerce.DowCommerceError('No password provided')
        else:
            username = 'demo'
            password = 'password'

        self.proxy = None
        self.delimiter = '&'
        self.results = {}
        self.error = True
        self.success = False
        self.declined = False
        self.url = 'https://secure.dowcommerce.net/api/transact.php'

        self.parameters = {}
        self.setParameter('username', username)
        self.setParameter('password', password)

    def process(self):
        encoded_args = urllib.urlencode(self.parameters)
        if self.proxy is None:
            results = str(urllib.urlopen(
                self.url, encoded_args).read()).split(self.delimiter)
        else:
            opener = urllib.FancyURLopener(self.proxy)
            opened = opener.open(self.url, encoded_args)
            try:
                results = str(opened.read()).split(self.delimiter)
            finally:
                opened.close()

        for result in results:
            (key, val) = result.split('=')
            self.results[key] = val

        if self.results['response'] == '1':
            self.error = False
            self.success = True
            self.declined = False
        elif self.results['response'] == '2':
            self.error = False
            self.success = False
            self.declined = True
        elif self.results['response'] == '3':
            self.error = True
            self.success = False
            self.declined = False
        else:
            self.error = True
            self.success = False
            self.declined = False
            raise DowCommerce.DowCommerceError(self.results)

    def setTransaction(
        self, creditcard, expiration, total, cvv=None, orderid=None, orderdescription=None,
        ipaddress=None, tax=None, shipping=None,
        firstname=None, lastname=None, company=None, address1=None, address2=None, city=None, state=None, zipcode=None,
        country=None, phone=None, fax=None, emailaddress=None, website=None,
        shipping_firstname=None, shipping_lastname=None, shipping_company=None, shipping_address1=None, shipping_address2=None,
            shipping_city=None, shipping_state=None, shipping_zipcode=None, shipping_country=None, shipping_emailaddress=None):
        if str(creditcard).strip() == '' or creditcard is None:
            raise DowCommerce.DowCommerceError('No credit card number passed to setTransaction(): {0}'.format(creditcard))
        if str(expiration).strip() == '' or expiration is None:
            raise DowCommerce.DowCommerceError('No expiration number passed to setTransaction(): {0}'.format(expiration))
        if str(total).strip() == '' or total is None:
            raise DowCommerce.DowCommerceError('No total amount passed to setTransaction(): {0}'.format(total))

        self.setParameter('ccnumber', creditcard)
        self.setParameter('ccexp', expiration)
        self.setParameter('amount', total)

        if cvv:
            self.setParameter('cvv', cvv)
        if orderid:
            self.setParameter('orderid', orderid)
        if orderdescription:
            self.setParameter('orderdescription', orderdescription)
        if ipaddress:
            self.setParameter('ipaddress', ipaddress)
        if tax:
            self.setParameter('tax', tax)
        if shipping:
            self.setParameter('shipping', shipping)

        ## billing info
        if firstname:
            self.setParameter('firstname', firstname)
        if lastname:
            self.setParameter('lastname', lastname)
        if company:
            self.setParameter('company', company)
        if address1:
            self.setParameter('address1', address1)
        if address2:
            self.setParameter('address2', address2)
        if city:
            self.setParameter('city', city)
        if state:
            self.setParameter('state', state)
        if zipcode:
            self.setParameter('zip', zipcode)
        if country:
            self.setParameter('country', country)
        if phone:
            self.setParameter('phone', phone)
        if fax:
            self.setParameter('fax', fax)
        if emailaddress:
            self.setParameter('email', emailaddress)
        if website:
            self.setParameter('website', website)

        ## shipping info
        if shipping_firstname:
            self.setParameter('shipping_firstname', shipping_firstname)
        if shipping_lastname:
            self.setParameter('shipping_lastname', shipping_lastname)
        if shipping_company:
            self.setParameter('shipping_company', shipping_company)
        if shipping_address1:
            self.setParameter('shipping_address1', shipping_address1)
        if shipping_address2:
            self.setParameter('shipping_address2', shipping_address2)
        if shipping_city:
            self.setParameter('shipping_city', shipping_city)
        if shipping_state:
            self.setParameter('shipping_state', shipping_state)
        if shipping_zipcode:
            self.setParameter('shipping_zip', shipping_zipcode)
        if shipping_country:
            self.setParameter('shipping_country', shipping_country)

    def setTransactionType(self, transtype=None):
        types = ['sale', 'auth', 'credit']
        if transtype.lower() not in types:
            raise DowCommerce.DowCommerceError('Incorrect Transaction Type passed to setTransactionType(): {0}'.format(transtype))
        self.setParameter('type', transtype.lower())

    def setProxy(self, proxy=None):
        if str(proxy).strip() == '' or proxy is None:
            raise DowCommerce.DowCommerceError('No proxy passed to setProxy()')
        self.proxy = {'http': str(proxy).strip()}

    def setParameter(self, key=None, value=None):
        if key is not None and value is not None and str(key).strip() != '' and str(value).strip() != '':
            self.parameters[key] = str(value).strip()
        else:
            raise DowCommerce.DowCommerceError('Incorrect parameters passed to setParameter(): {0}:{1}'.format(key, value))

    def isApproved(self):
        return self.success

    def isDeclined(self):
        return self.declined

    def isError(self):
        return self.error

    def getResultResponseShort(self):
        responses = ['', 'Approved', 'Declined', 'Error']
        return responses[int(self.results['response'])]

    def getFullResponse(self):
        return self.results

    def getResponseText(self):
        return self.results['responsetext']


def test():
    import socket
    import sys
    from time import time

    ## TEST VALUES FROM API DOC:
    # Visa:             4111111111111111
    # MasterCard        5431111111111111
    # DiscoverCard:     6011601160116611
    # American Express: 341111111111111
    # Expiration:       10/10
    # Amount:           > 1.00  (( passing less than $1.00 will cause it to be declined ))
    # CVV:              999
    creditcard = '4111111111111111'
    expiration = '1010'
    total = '1.00'
    cvv = '999'
    tax = '0.00'
    orderid = str(time())[4:10]  # get a random invoice number

    try:
        payment = DowCommerce(demomode=True)
        payment.setTransaction(
            creditcard, expiration, total, cvv=cvv, tax=tax, orderid=orderid, orderdescription='Test Transaction',
            firstname='John', lastname='Doe', company='Acme', address1='123 Min Street', city='Hometown', state='VA',
            zipcode='12345', country='US', phone='888-555-1212', emailaddress='john@noemail.local', ipaddress='192.168.1.1')

        payment.process()
        if payment.isApproved():
            print 'Payment approved!'
            print payment.getFullResponse()
        elif payment.isDeclined():
            print 'Your credit card was declined by your bank'
        elif payment.isError():
            raise DowCommerce.DowCommerceError('An uncaught error occurred')
    except DowCommerce.DowCommerceError, e:
        print "Exception thrown:", e
        print 'An error occured'
    print 'approved', payment.isApproved()
    print 'declined', payment.isDeclined()
    print 'error', payment.isError()

if __name__ == '__main__':
    test()
