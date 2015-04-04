# Template helpers

import os


def A_button(*a, **b):
    b['_data-role'] = 'button'
    b['_data-inline'] = 'true'
    return A(*a, **b)

def button(href, label):
    if is_mobile:
        ret = A_button(SPAN(label), _href=href)
    else:
        ret = A(SPAN(label), _class='button btn', _href=href)
    return ret

def button_enable(href, app):
    if os.path.exists(os.path.join(apath(app, r=request), 'DISABLED')):
        label = SPAN(T('Enable'), _style='color:red')
    else:
        label = SPAN(T('Disable'), _style='color:green')
    id = 'enable_' + app
    return A(label, _class='button btn', _id=id, callback=href, target=id)

def sp_button(href, label):
    if request.user_agent().get('is_mobile'):
        ret = A_button(SPAN(label), _href=href)
    else:
        ret = A(SPAN(label), _class='button special btn btn-inverse', _href=href)
    return ret

def helpicon():
    return IMG(_src=URL('static', 'images/help.png'), _alt='help')

def searchbox(elementid):
    return SPAN(LABEL(IMG(_id="search_start", _src=URL('static', 'images/search.png'), _alt=T('filter')),
                    _class='icon', _for=elementid), ' ',
                INPUT(_id=elementid, _type='text', _size=12, _class="input-medium"),
            _class="searchbox")
