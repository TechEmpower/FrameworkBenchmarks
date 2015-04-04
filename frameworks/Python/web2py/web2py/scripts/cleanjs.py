import re


def cleanjs(text):
    text = re.sub('\s*}\s*', '\n}\n', text)
    text = re.sub('\s*{\s*', ' {\n', text)
    text = re.sub('\s*;\s*', ';\n', text)
    text = re.sub('\s*,\s*', ', ', text)
    text = re.sub('\s*(?P<a>[\+\-\*/\=]+)\s*', ' \g<a> ', text)
    lines = text.split('\n')
    text = ''
    indent = 0
    for line in lines:
        rline = line.strip()
        if rline:
            pass
    return text
