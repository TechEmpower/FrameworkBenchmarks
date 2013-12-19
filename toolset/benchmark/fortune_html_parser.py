# -*- coding: utf-8
from HTMLParser import HTMLParser

class FortuneHTMLParser(HTMLParser):
  body = []

  valid = '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr><tr><td>11</td><td>&lt;script&gt;alert(&quot;This should not be displayed in a browser alert box.&quot;);&lt;/script&gt;</td></tr><tr><td>4</td><td>A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1</td></tr><tr><td>5</td><td>A computer program does what you tell it to do, not what you want it to do.</td></tr><tr><td>2</td><td>A computer scientist is someone who fixes things that aren\'t broken.</td></tr><tr><td>8</td><td>A list is only as strong as its weakest link. — Donald Knuth</td></tr><tr><td>0</td><td>Additional fortune added at request time.</td></tr><tr><td>3</td><td>After enough decimal places, nobody gives a damn.</td></tr><tr><td>7</td><td>Any program that runs right is obsolete.</td></tr><tr><td>10</td><td>Computers make very fast, very accurate mistakes.</td></tr><tr><td>6</td><td>Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen</td></tr><tr><td>9</td><td>Feature: A bug with seniority.</td></tr><tr><td>1</td><td>fortune: No such file or directory</td></tr><tr><td>12</td><td>フレームワークのベンチマーク</td></tr></table></body></html>'

  # Is called when a doctype or other such tag is read in.
  # For our purposes, we assume this is only going to be
  # "DOCTYPE html", so we will surround it with "<!" and ">".
  def handle_decl(self, decl):
    self.body.append("<!{d}>".format(d=decl))

  # This is called when an HTML character is parsed (i.e. 
  # &quot;). There are a number of issues to be resolved 
  # here. For instance, some tests choose to leave the
  # "+" character as-is, which should be fine as far as
  # character escaping goes, but others choose to use the
  # character reference of "&#43;", which is also fine.
  # Therefore, this method looks for all possible character
  # references and normalizes them so that we can 
  # validate the input against a single valid spec string.
  # Another example problem: "&quot;" is valid, but so is
  # "&#34;"
  def handle_charref(self, name):
    # "&#34;" is a valid escaping, but we are normalizing
    # it so that our final parse can just be checked for
    # equality.
    norm = name.replace("34", "quot")
    # Again, "&#43;" is a valid escaping of the "+", but
    # it is not required, so we need to normalize for out
    # final parse and equality check.
    norm = norm.replace("43", "+")
    # Again, "&#62;" is a valid escaping of ">", but we
    # need to normalize to "&gt;" for equality checking.
    norm = norm.replace("62", "gt")
    # Again, "&#60;" is a valid escaping of "<", but we
    # need to nromalize to "&lt;" for equality checking.
    norm = norm.replace("60", "lt")
    # Append our normalized entity reference to our body.
    self.body.append("&{n};".format(n=norm))

  def handle_entityref(self, name):
    self.body.append("&{n};".format(n=norm))

  # This is called every time a tag is opened. We append
  # each one wrapped in "<" and ">".
  def handle_starttag(self, tag, attrs):
    self.body.append("<{t}>".format(t=tag))

  # This is called whenever data is presented inside of a
  # start and end tag. Generally, this will only ever be
  # the contents inside of "<td>" and "</td>", but there
  # are also the "<title>" and "</title>" tags.
  # Note: The data is stripped of leading and trailing 
  # white-space.
  def handle_data (self, data):
    self.body.append("{d}".format(d=data.strip()))

  # This is called every time a tag is closed. We append
  # each one wrapped in "</" and ">".
  def handle_endtag(self, tag):
    self.body.append("</{t}>".format(t=tag))

  # Returns whether the HTML input parsed by this parser
  # is valid against our known "fortune" spec.
  # The parsed data in 'body' is joined on empty strings
  # and checked for equality against our spec.
  def isValidFortune(self):
    return self.valid == ''.join(self.body)