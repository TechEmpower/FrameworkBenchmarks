import urllib
import simplejson
from hashlib import sha1

class Stripe:
    """
    Usage:
    key='<api key>'
    d = Stripe(key).charge(
               amount=100, # 1 dollar!!!!
               currency='usd',
               card_number='4242424242424242',
               card_exp_month='5',
               card_exp_year='2012',
               card_cvc_check='123',
               description='test charge')
    print d
    print Stripe(key).check(d['id'])
    print Stripe(key).refund(d['id'])

    Sample output (python dict):
    {u'fee': 0, u'description': u'test charge', u'created': 1321242072, u'refunded': False, u'livemode': False, u'object': u'charge', u'currency': u'usd', u'amount': 100, u'paid': True, u'id': u'ch_sdjasgfga83asf', u'card': {u'exp_month': 5, u'country': u'US', u'object': u'card', u'last4': u'4242', u'exp_year': 2012, u'type': u'Visa'}}
    if paid is True than transaction was processed

    Use in WEB2PY (guaranteed PCI compliant)

def pay():
    from gluon.contrib.stripe import StripeForm
    form = StripeForm(
        pk=STRIPE_PUBLISHABLE_KEY,
        sk=STRIPE_SECRET_KEY,
        amount=150, # $1.5 (amount is in cents)
        description="Nothing").process()
    if form.accepted:
        payment_id = form.response['id']
        redirect(URL('thank_you'))
    elif form.errors:
        redirect(URL('pay_error'))
    return dict(form=form)

    """

    URL_CHARGE = 'https://%s:@api.stripe.com/v1/charges'
    URL_CHECK = 'https://%s:@api.stripe.com/v1/charges/%s'
    URL_REFUND = 'https://%s:@api.stripe.com/v1/charges/%s/refund'

    def __init__(self, key):
        self.key = key

    def charge(self,
               amount, # in cents
               currency='usd',
               card_number='4242424242424242',
               card_exp_month='5',
               card_exp_year='2012',
               card_cvc_check='123',
               token=None,
               description='test charge',
               more=None):
        if token:
            d = {'amount': amount,
                 'currency': currency,
                 'card': token,
                 'description': description}
        else:
            d = {'amount': amount,
                 'currency': currency,
                 'card[number]': card_number,
                 'card[exp_month]': card_exp_month,
                 'card[exp_year]': card_exp_year,
                 'card[cvc_check]': card_cvc_check,
                 'description': description}
        if more:
            d.update(mode)
        params = urllib.urlencode(d)
        u = urllib.urlopen(self.URL_CHARGE % self.key, params)
        return simplejson.loads(u.read())

    def check(self, charge_id):
        u = urllib.urlopen(self.URL_CHECK % (self.key, charge_id))
        return simplejson.loads(u.read())

    def refund(self, charge_id):
        params = urllib.urlencode({})
        u = urllib.urlopen(self.URL_REFUND % (self.key, charge_id),
                           params)
        return simplejson.loads(u.read())

class StripeForm(object):
    def __init__(self,
                 pk, sk,
                 amount, # in cents
                 description,
                 currency = 'usd',
                 currency_symbol = '$',
                 security_notice = True,
                 disclosure_notice = True,
                 template = None):
        from gluon import current, redirect, URL
        if not (current.request.is_local or current.request.is_https):
            redirect(URL(args=current.request.args,scheme='https'))
        self.pk = pk
        self.sk = sk
        self.amount = amount
        self.description = description
        self.currency = currency
        self.currency_symbol = currency_symbol
        self.security_notice = security_notice
        self.disclosure_notice = disclosure_notice
        self.template = template or TEMPLATE
        self.accepted = None
        self.errors = None
        self.signature = sha1(repr((self.amount,self.description))).hexdigest()

    def process(self):
        from gluon import current
        request = current.request
        if request.post_vars:
            if self.signature == request.post_vars.signature:
                self.response = Stripe(self.sk).charge(
                    token=request.post_vars.stripeToken,
                    amount=self.amount,
                    description=self.description,
                    currency=self.currency)
                if self.response.get('paid',False):
                    self.accepted = True
                    return self
            self.errors = True
        return self

    def xml(self):
        from gluon.template import render
        if self.accepted:
            return "Your payment was processed successfully"
        elif self.errors:
            return "There was an processing error"
        else:
            context = dict(amount=self.amount,
                           signature=self.signature, pk=self.pk,
                           currency_symbol=self.currency_symbol,
                           security_notice=self.security_notice,
                           disclosure_notice=self.disclosure_notice)
            return render(content=self.template, context=context)


TEMPLATE = """
<script type="text/javascript" src="https://js.stripe.com/v2/"></script>
<script>
jQuery(function(){
    // This identifies your website in the createToken call below
    Stripe.setPublishableKey('{{=pk}}');

    var stripeResponseHandler = function(status, response) {
      var jQueryform = jQuery('#payment-form');

      if (response.error) {
        // Show the errors on the form
        jQuery('.payment-errors').text(response.error.message).show();
        jQueryform.find('button').prop('disabled', false);
      } else {
        // token contains id, last4, and card type
        var token = response.id;
        // Insert the token into the form so it gets submitted to the server
        var tokenInput = jQuery('<input type="hidden" name="stripeToken" />');
        jQueryform.append(tokenInput.val(token));
        // and re-submit
        jQueryform.get(0).submit();
      }
    };

    jQuery(function(jQuery) {
      jQuery('#payment-form').submit(function(e) {

        var jQueryform = jQuery(this);

        // Disable the submit button to prevent repeated clicks
        jQueryform.find('button').prop('disabled', true);

        Stripe.createToken(jQueryform, stripeResponseHandler);

        // Prevent the form from submitting with the default action
        return false;
      });
    });
});
</script>

<h3>Payment Amount: {{=currency_symbol}} {{="%.2f" % (0.01*amount)}}</h3>
<form action="" method="POST" id="payment-form" class="form-horizontal">

  <div class="form-row control-group">
    <label class="control-label">Card Number</label>
    <div class="controls">
      <input type="text" size="20" data-stripe="number"
             placeholder="4242424242424242"/>
    </div>
  </div>

  <div class="form-row control-group">
    <label class="control-label">CVC</label>
    <div class="controls">
      <input type="text" size="4" style="width:80px" data-stripe="cvc"
             placeholder="XXX"/>
      <a href="http://en.wikipedia.org/wiki/Card_Verification_Code" target="_blank">What is this?</a>
    </div>
  </div>

  <div class="form-row control-group">
    <label class="control-label">Expiration</label>
    <div class="controls">
      <input type="text" size="2" style="width:40px" data-stripe="exp-month"
             placeholder="MM"/>
      /
      <input type="text" size="4" style="width:80px" data-stripe="exp-year"
             placeholder="YYYY"/>
    </div>
  </div>


  <div class="control-group">
    <div class="controls">
      <button type="submit" class="btn btn-primary">Submit Payment</button>
      <div class="payment-errors error hidden"></div>
    </div>
  </div>
  <input type="hidden" name="signature" value="{{=signature}}" />
</form>

{{if security_notice or disclosure_notice:}}
<div class="well">
  {{if security_notice:}}
  <h3>Security Notice</h3>
  <p>For your security we process all payments using a service called <a href="http://stripe.com">Stripe</a>. Thanks to <a href="http://stripe.com">Stripe</a> your credit card information is communicated directly between your Web Browser and the payment processor, <a href="http://stripe.com">Stripe</a>, without going through our server. Since we never see your card information nobody can steal it through us. Stripe is <a href="https://stripe.com/us/help/faq#security-and-pci">PCI compliant</a> and so are we.</p>
  {{pass}}
  {{if disclosure_notice:}}
  <h3>Disclosure Notice</h3>

  <p>We do store other information about your purchase including your name, a description of the purchase, the time when it was processed, and the amount paid. This information is necessary to provide our services and for accounting purposes. We do not disclose this information to third parties unless required to operate our services or accounting purposes.</p>
  {{pass}}
</div>
{{pass}}
"""

if __name__ == '__main__':
    key = raw_input('user>')
    d = Stripe(key).charge(100)
    print 'charged', d['paid']
    s = Stripe(key).check(d[u'id'])
    print 'paid', s['paid'], s['amount'], s['currency']
    s = Stripe(key).refund(d[u'id'])
    print 'refunded', s['refunded']
