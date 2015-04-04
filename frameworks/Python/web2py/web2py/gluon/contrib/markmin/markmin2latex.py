#!/usr/bin/env python
# created my Massimo Di Pierro
# license MIT/BSD/GPL
import re
import cgi
import sys
import doctest
from optparse import OptionParser

__all__ = ['render','markmin2latex']

META = 'META'
regex_newlines = re.compile('(\n\r)|(\r\n)')
regex_dd=re.compile('\$\$(?P<latex>.*?)\$\$')
regex_code = re.compile('('+META+')|(``(?P<t>.*?)``(:(?P<c>\w+))?)',re.S)
regex_title = re.compile('^#{1} (?P<t>[^\n]+)',re.M)
regex_maps = [
    (re.compile('[ \t\r]+\n'),'\n'),
    (re.compile('\*\*(?P<t>[^\s\*]+( +[^\s\*]+)*)\*\*'),'{\\\\bf \g<t>}'),
    (re.compile("''(?P<t>[^\s']+( +[^\s']+)*)''"),'{\\it \g<t>}'),
    (re.compile('^#{5,6}\s*(?P<t>[^\n]+)',re.M),'\n\n{\\\\bf \g<t>}\n'),
    (re.compile('^#{4}\s*(?P<t>[^\n]+)',re.M),'\n\n\\\\goodbreak\\subsubsection{\g<t>}\n'),
    (re.compile('^#{3}\s*(?P<t>[^\n]+)',re.M),'\n\n\\\\goodbreak\\subsection{\g<t>}\n'),
    (re.compile('^#{2}\s*(?P<t>[^\n]+)',re.M),'\n\n\\\\goodbreak\\section{\g<t>}\n'),
    (re.compile('^#{1}\s*(?P<t>[^\n]+)',re.M),''),
    (re.compile('^\- +(?P<t>.*)',re.M),'\\\\begin{itemize}\n\\item \g<t>\n\\end{itemize}'),
    (re.compile('^\+ +(?P<t>.*)',re.M),'\\\\begin{itemize}\n\\item \g<t>\n\\end{itemize}'),
    (re.compile('\\\\end\{itemize\}\s+\\\\begin\{itemize\}'),'\n'),
    (re.compile('\n\s+\n'),'\n\n')]
regex_table = re.compile('^\-{4,}\n(?P<t>.*?)\n\-{4,}(:(?P<c>\w+))?\n',re.M|re.S)

regex_anchor = re.compile('\[\[(?P<t>\S+)\]\]')
regex_bibitem = re.compile('\-\s*\[\[(?P<t>\S+)\]\]')
regex_image_width = re.compile('\[\[(?P<t>[^\]]*?) +(?P<k>\S+) +(?P<p>left|right|center) +(?P<w>\d+px)\]\]')
regex_image = re.compile('\[\[(?P<t>[^\]]*?) +(?P<k>\S+) +(?P<p>left|right|center)\]\]')
#regex_video = re.compile('\[\[(?P<t>[^\]]*?) +(?P<k>\S+) +video\]\]')
#regex_audio = re.compile('\[\[(?P<t>[^\]]*?) +(?P<k>\S+) +audio\]\]')
regex_link = re.compile('\[\[(?P<t>[^\]]*?) +(?P<k>\S+)\]\]')
regex_auto = re.compile('(?<!["\w])(?P<k>\w+://[\w\.\-\?&%\:]+)',re.M)
regex_commas = re.compile('[ ]+(?P<t>[,;\.])')
regex_noindent = re.compile('\n\n(?P<t>[a-z])')
#regex_quote_left = re.compile('"(?=\w)')
#regex_quote_right = re.compile('(?=\w\.)"')

def latex_escape(text,pound=True):
    text=text.replace('\\','{\\textbackslash}')
    for c in '^_&$%{}': text=text.replace(c,'\\'+c)
    text=text.replace('\\{\\textbackslash\\}','{\\textbackslash}')
    if pound: text=text.replace('#','\\#')
    return text

def render(text,
           extra={},
           allowed={},
           sep='p',
           image_mapper=lambda x:x,
           chapters=False):
    #############################################################
    # replace all blocks marked with ``...``:class with META
    # store them into segments they will be treated as code
    #############################################################
    text = str(text or '')
    segments, i = [], 0
    text = regex_dd.sub('``\g<latex>``:latex ',text)
    text = regex_newlines.sub('\n',text)
    while True:
        item = regex_code.search(text,i)
        if not item: break
        if item.group()==META:
            segments.append((None,None))
            text = text[:item.start()]+META+text[item.end():]
        else:
            c = item.group('c') or ''
            if 'code' in allowed and not c in allowed['code']: c = ''
            code = item.group('t').replace('!`!','`')
            segments.append((code,c))
            text = text[:item.start()]+META+text[item.end():]
        i=item.start()+3


    #############################################################
    # do h1,h2,h3,h4,h5,h6,b,i,ol,ul and normalize spaces
    #############################################################

    title = regex_title.search(text)
    if not title: title='Title'
    else: title=title.group('t')

    text = latex_escape(text,pound=False)

    texts = text.split('## References',1)
    text = regex_anchor.sub('\\label{\g<t>}', texts[0])
    if len(texts)==2:
        text += '\n\\begin{thebibliography}{999}\n'
        text += regex_bibitem.sub('\n\\\\bibitem{\g<t>}', texts[1])
        text += '\n\\end{thebibliography}\n'

    text = '\n'.join(t.strip() for t in text.split('\n'))
    for regex, sub in regex_maps:
        text = regex.sub(sub,text)
    text=text.replace('#','\\#')
    text=text.replace('`',"'")

    #############################################################
    # process tables and blockquotes
    #############################################################
    while True:
        item = regex_table.search(text)
        if not item: break
        c = item.group('c') or ''
        if 'table' in allowed and not c in allowed['table']: c = ''
        content = item.group('t')
        if ' | ' in content:
            rows = content.replace('\n','\\\\\n').replace(' | ',' & ')
            row0,row2 = rows.split('\\\\\n',1)
            cols=row0.count(' & ')+1
            cal='{'+''.join('l' for j in range(cols))+'}'
            tabular = '\\begin{center}\n{\\begin{tabular}'+cal+'\\hline\n' + row0+'\\\\ \\hline\n'+row2 + ' \\\\ \\hline\n\\end{tabular}}\n\\end{center}'
            if row2.count('\n')>20: tabular='\\newpage\n'+tabular
            text = text[:item.start()] + tabular + text[item.end():]
        else:
            text = text[:item.start()] + '\\begin{quote}' + content + '\\end{quote}' + text[item.end():]

    #############################################################
    # deal with images, videos, audios and links
    #############################################################

    def sub(x):
        f=image_mapper(x.group('k'))
        if not f: return None
        return '\n\\begin{center}\\includegraphics[width=8cm]{%s}\\end{center}\n' % (f)
    text = regex_image_width.sub(sub,text)
    text = regex_image.sub(sub,text)

    text = regex_link.sub('{\\\\footnotesize\\href{\g<k>}{\g<t>}}', text)
    text = regex_commas.sub('\g<t>',text)
    text = regex_noindent.sub('\n\\\\noindent \g<t>',text)

    ### fix paths in images
    regex=re.compile('\\\\_\w*\.(eps|png|jpg|gif)')
    while True:
        match=regex.search(text)
        if not match: break
        text=text[:match.start()]+text[match.start()+1:]
    #text = regex_quote_left.sub('``',text)
    #text = regex_quote_right.sub("''",text)

    if chapters:
        text=text.replace(r'\section*{',r'\chapter*{')
        text=text.replace(r'\section{',r'\chapter{')
        text=text.replace(r'subsection{',r'section{')

    #############################################################
    # process all code text
    #############################################################
    parts = text.split(META)
    text = parts[0]
    authors = []
    for i,(code,b) in enumerate(segments):
        if code==None:
            html = META
        else:
            if b=='hidden':
                html=''
            elif b=='author':
                author = latex_escape(code.strip())
                authors.append(author)
                html=''
            elif b=='inxx':
                html='\inxx{%s}' % latex_escape(code)
            elif b=='cite':
                html='~\cite{%s}' % latex_escape(code.strip())
            elif b=='ref':
                html='~\ref{%s}' % latex_escape(code.strip())
            elif b=='latex':
                if '\n' in code:
                    html='\n\\begin{equation}\n%s\n\\end{equation}\n' % code.strip()
                else:
                    html='$%s$' % code.strip()
            elif b=='latex_eqnarray':
                code=code.strip()
                code='\\\\'.join(x.replace('=','&=&',1) for x in code.split('\\\\'))
                html='\n\\begin{eqnarray}\n%s\n\\end{eqnarray}\n' % code
            elif b.startswith('latex_'):
                key=b[6:]
                html='\\begin{%s}%s\\end{%s}' % (key,code,key)
            elif b in extra:
                if code[:1]=='\n': code=code[1:]
                if code[-1:]=='\n': code=code[:-1]
                html = extra[b](code)
            elif code[:1]=='\n' or code[:-1]=='\n':
                if code[:1]=='\n': code=code[1:]
                if code[-1:]=='\n': code=code[:-1]
                if code.startswith('<') or code.startswith('{{') or code.startswith('http'):
                    html = '\\begin{lstlisting}[keywords={}]\n%s\n\\end{lstlisting}' % code
                else:
                    html = '\\begin{lstlisting}\n%s\n\\end{lstlisting}' % code
            else:
                if code[:1]=='\n': code=code[1:]
                if code[-1:]=='\n': code=code[:-1]
                html = '{\\ft %s}' % latex_escape(code)
        try:
            text = text+html+parts[i+1]
        except:
            text = text + '... WIKI PROCESSING ERROR ...'
            break
    text =  text.replace(' ~\\cite','~\\cite')
    return text, title, authors

WRAPPER = """
\\documentclass[12pt]{article}
\\usepackage{hyperref}
\\usepackage{listings}
\\usepackage{upquote}
\\usepackage{color}
\\usepackage{graphicx}
\\usepackage{grffile}
\\usepackage[utf8x]{inputenc}
\\definecolor{lg}{rgb}{0.9,0.9,0.9}
\\definecolor{dg}{rgb}{0.3,0.3,0.3}
\\def\\ft{\\small\\tt}
\\lstset{
   basicstyle=\\footnotesize,
   breaklines=true, basicstyle=\\ttfamily\\color{black}\\footnotesize,
   keywordstyle=\\bf\\ttfamily,
   commentstyle=\\it\\ttfamily,
   stringstyle=\\color{dg}\\it\\ttfamily,
   numbers=left, numberstyle=\\color{dg}\\tiny, stepnumber=1, numbersep=5pt,
   backgroundcolor=\\color{lg}, tabsize=4, showspaces=false,
   showstringspaces=false
}
\\title{%(title)s}
\\author{%(author)s}
\\begin{document}
\\maketitle
\\tableofcontents
\\newpage
%(body)s
\\end{document}
"""

def markmin2latex(data, image_mapper=lambda x:x, extra={},
                  wrapper=WRAPPER):
    body, title, authors = render(data, extra=extra, image_mapper=image_mapper)
    author = '\n\\and\n'.join(a.replace('\n','\\\\\n\\footnotesize ') for a in authors)
    return wrapper % dict(title=title, author=author, body=body)

if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("-i", "--info", dest="info",
                      help="markmin help")
    parser.add_option("-t", "--test", dest="test", action="store_true",
                      default=False)
    parser.add_option("-n", "--no_wrapper", dest="no_wrapper",
                      action="store_true",default=False)
    parser.add_option("-c", "--chapters", dest="chapters",action="store_true",
                      default=False,help="switch section for chapter")
    parser.add_option("-w", "--wrapper", dest="wrapper", default=False,
                      help="latex file containing header and footer")

    (options, args) = parser.parse_args()
    if options.info:
        import markmin2html
        markmin2latex(markmin2html.__doc__)
    elif options.test:
        doctest.testmod()
    else:
        if options.wrapper:
            fwrapper = open(options.wrapper,'rb')
            try:
                wrapper = fwrapper.read()
            finally:
                fwrapper.close()
        elif options.no_wrapper:
            wrapper  = '%(body)s'
        else:
            wrapper = WRAPPER
        for f in args:
            fargs = open(f,'r')
            content_data = []
            try:
                content_data.append(fargs.read())
            finally:
                fargs.close()
        content = '\n'.join(content_data)
        output= markmin2latex(content,
                              wrapper=wrapper,
                              chapters=options.chapters)
        print output


