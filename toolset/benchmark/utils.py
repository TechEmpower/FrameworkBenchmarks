
def header(message, top='-', bottom='-'):
    '''
    Generates a clean header
    '''
    topheader = (top * 80)[:80]
    bottomheader = (bottom * 80)[:80]
    return "%s\n  %s\n%s" % (topheader, message, bottomheader)
