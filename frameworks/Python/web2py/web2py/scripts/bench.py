import time
import sys
import urllib2
import urllib2

n = int(sys.argv[1])
url = sys.argv[2]
headers = {"Accept-Language": "en"}
req = urllib2.Request(url, None, headers)

t0 = time.time()
for k in xrange(n):
    data = urllib2.urlopen(req).read()
print (time.time() - t0) / n
if n == 1:
    print data
