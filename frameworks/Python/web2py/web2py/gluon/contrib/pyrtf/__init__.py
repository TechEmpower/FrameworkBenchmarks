from    PropertySets    import  *
from    Elements    import  *
from    Styles      import  *
from    Renderer    import  *

def dumps(doc):
    import cStringIO
    s=cStringIO.StringIO()
    r=Renderer()
    r.Write(doc,s)
    return s.getvalue()

