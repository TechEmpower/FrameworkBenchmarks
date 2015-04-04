response.files.append(URL('static','plugin_statebutton/js/bootstrap-switch.js'))
response.files.append(URL('static','plugin_statebutton/css/bootstrap-switch.css'))

def stateWidget(field, value, data={'on-label':'Enabled', 'off-label':'Disabled', 'on':"primary", 'off':"default" }):
    try:
        fieldName = str(field).split('.')[1]
    except:
        fieldName = field

    div = DIV(INPUT( _type='checkbox', _name='%s' % fieldName, _checked= 'checked' if value == 'true' else None, _value='true'),
              _class='make-bootstrap-switch',
              data=data)
    script = SCRIPT("""
        jQuery(".make-bootstrap-switch input[name='%s']").parent().bootstrapSwitch();
    """ % fieldName)
    return DIV(div, script)
