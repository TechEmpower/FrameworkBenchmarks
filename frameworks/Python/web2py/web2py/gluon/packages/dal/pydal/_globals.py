import threading

GLOBAL_LOCKER = threading.RLock()
THREAD_LOCAL = threading.local()

DEFAULT = lambda: None

def IDENTITY(x): return x
def OR(a,b): return a|b
def AND(a,b): return a&b

GLOBALS = {}
