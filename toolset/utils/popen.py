from logging import error
from subprocess import Popen
from threading import Event
from threading import Thread

class PopenTimeout(Popen):
    '''
    Popen class with timeout for Python 2.7
    see https://stackoverflow.com/questions/1191374/using-module-subprocess-with-timeout
    '''
    def __init__(self, *args, **kwargs):
        self.timeout = kwargs.pop('timeout', 0)
        self.timer = None
        self.done = Event()

        Popen.__init__(self, *args, **kwargs)

    def __tkill(self):
        timeout = self.timeout
        if not self.done.wait(timeout):
            error('Terminating process {} by timeout of {} secs.'.format(self.pid, timeout))
            self.kill()

    def expirable(func):
        def wrapper(self, *args, **kwargs):
            # zero timeout means call of parent method
            if self.timeout == 0:
                return func(self, *args, **kwargs)

            # if timer is None, need to start it
            if self.timer is None:
                self.timer = thr = Thread(target=self.__tkill)
                thr.daemon = True
                thr.start()

            result = func(self, *args, **kwargs)
            self.done.set()

            return result
        return wrapper

    wait = expirable(Popen.wait)
    communicate = expirable(Popen.communicate)
