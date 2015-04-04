EXPIRATION_MINUTES=60
DIGITS=('0','1','2','3','4','5','6','7','8','9')
import os, time, stat, cPickle, logging
path = os.path.join(request.folder,'sessions')
if not os.path.exists(path):
    os.mkdir(path)
now = time.time()
for filename in os.listdir(path):
    fullpath=os.path.join(path,filename)
    if os.path.isfile(fullpath) and filename.startswith(DIGITS):
        try:
            filetime = os.stat(fullpath)[stat.ST_MTIME] # get it before our io
            try:
                session_data = cPickle.load(open(fullpath, 'rb+'))
                expiration = session_data['auth']['expiration']
            except:
                expiration = EXPIRATION_MINUTES * 60
            if (now - filetime) > expiration:
                os.unlink(fullpath)
        except:
            logging.exception('failure to check %s' % fullpath)
