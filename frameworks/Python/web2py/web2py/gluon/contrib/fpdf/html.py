# -*- coding: latin-1 -*-

"HTML Renderer for FPDF.py"

__author__ = "Mariano Reingart <reingart@gmail.com>"
__copyright__ = "Copyright (C) 2010 Mariano Reingart"
__license__ = "LGPL 3.0"

# Inspired by tuto5.py and several examples from fpdf.org, html2fpdf, etc.

from fpdf import FPDF
from HTMLParser import HTMLParser

DEBUG = False

def px2mm(px):
    return int(px)*25.4/72.0

def hex2dec(color = "#000000"):
    if color:
        r = int(color[1:3], 16)
        g = int(color[3:5], 16)
        b = int(color[5:7], 16)
        return r, g, b

class HTML2FPDF(HTMLParser):
    "Render basic HTML to FPDF"

    def __init__(self, pdf, image_map=None):
        HTMLParser.__init__(self)
        self.style = {}
        self.pre = False
        self.href = ''
        self.align = ''
        self.page_links = {}
        self.font = None
        self.font_stack = [] 
        self.pdf = pdf
        self.image_map = image_map or (lambda src: src)
        self.r = self.g = self.b = 0
        self.indent = 0
        self.bullet = []
        self.set_font("times", 12)
        self.font_face = "times"    # initialize font
        self.color = 0              #initialize font color
        self.table = None           # table attributes
        self.table_col_width = None # column (header) widths
        self.table_col_index = None # current column index
        self.td = None              # cell attributes
        self.th = False             # header enabled
        self.tr = None
        self.theader = None           # table header cells
        self.tfooter = None           # table footer cells
        self.thead = None
        self.tfoot = None
        self.theader_out = self.tfooter_out = False
        self.hsize = dict(h1=2, h2=1.5, h3=1.17, h4=1, h5=0.83, h6=0.67)
        
    def width2mm(self, length):
        if length[-1]=='%':
            total = self.pdf.w - self.pdf.r_margin - self.pdf.l_margin
            if self.table['width'][-1]=='%':
                total *= int(self.table['width'][:-1])/100.0
            return int(length[:-1]) * total / 101.0
        else:
            return int(length) / 6.0

    def handle_data(self, txt):
        if self.td is not None: # drawing a table?
            if 'width' not in self.td and 'colspan' not in self.td:
                try:
                    l = [self.table_col_width[self.table_col_index]]
                except IndexError:
                    raise RuntimeError("Table column/cell width not specified, unable to continue")
            elif 'colspan' in self.td:
                i = self.table_col_index
                colspan = int(self.td['colspan'])
                l = self.table_col_width[i:i+colspan]
            else:
                l = [self.td.get('width','240')]
            w = sum([self.width2mm(lenght) for lenght in l])
            h = int(self.td.get('height', 0)) / 4 or self.h*1.30
            self.table_h = h
            border = int(self.table.get('border', 0))
            if not self.th:
                align = self.td.get('align', 'L')[0].upper()
                border = border and 'LR'
            else:
                self.set_style('B',True)
                border = border or 'B'
                align = self.td.get('align', 'C')[0].upper()
            bgcolor = hex2dec(self.td.get('bgcolor', self.tr.get('bgcolor', '')))
            # parsing table header/footer (drawn later):
            if self.thead is not None:
                self.theader.append(((w,h,txt,border,0,align), bgcolor))
            if self.tfoot is not None:
                self.tfooter.append(((w,h,txt,border,0,align), bgcolor))
            # check if reached end of page, add table footer and header:
            height = h + (self.tfooter and self.tfooter[0][0][1] or 0)
            if self.pdf.y+height>self.pdf.page_break_trigger and not self.th:
                self.output_table_footer()
                self.pdf.add_page()
                self.theader_out = self.tfooter_out = False
            if self.tfoot is None and self.thead is None:
                if not self.theader_out: 
                    self.output_table_header()
                self.box_shadow(w, h, bgcolor)
                if DEBUG: print "td cell", self.pdf.x, w, txt, "*"
                self.pdf.cell(w,h,txt,border,0,align)
        elif self.table is not None:
            # ignore anything else than td inside a table 
            pass
        elif self.align:
            if DEBUG: print "cell", txt, "*"
            self.pdf.cell(0,self.h,txt,0,1,self.align[0].upper(), self.href)
        else:
            txt = txt.replace("\n"," ")
            if self.href:
                self.put_link(self.href,txt)
            else:
                if DEBUG: print "write", txt, "*"
                self.pdf.write(self.h,txt)

    def box_shadow(self, w, h, bgcolor):
        if DEBUG: print "box_shadow", w, h, bgcolor
        if bgcolor:
            fill_color = self.pdf.fill_color
            self.pdf.set_fill_color(*bgcolor)
            self.pdf.rect(self.pdf.x, self.pdf.y, w, h, 'F')
            self.pdf.fill_color = fill_color

    def output_table_header(self):
        if self.theader:
            b = self.b
            x = self.pdf.x
            self.pdf.set_x(self.table_offset)
            self.set_style('B',True)
            for cell, bgcolor in self.theader:
                self.box_shadow(cell[0], cell[1], bgcolor)
                self.pdf.cell(*cell)
            self.set_style('B',b)
            self.pdf.ln(self.theader[0][0][1])
            self.pdf.set_x(self.table_offset)
            #self.pdf.set_x(x)
        self.theader_out = True
        
    def output_table_footer(self):
        if self.tfooter:
            x = self.pdf.x
            self.pdf.set_x(self.table_offset)
            #TODO: self.output_table_sep()
            for cell, bgcolor in self.tfooter:
                self.box_shadow(cell[0], cell[1], bgcolor)
                self.pdf.cell(*cell)
            self.pdf.ln(self.tfooter[0][0][1])
            self.pdf.set_x(x)
        if int(self.table.get('border', 0)):
            self.output_table_sep()
        self.tfooter_out = True
            
    def output_table_sep(self):
        self.pdf.set_x(self.table_offset)
        x1 = self.pdf.x
        y1 = self.pdf.y
        w = sum([self.width2mm(lenght) for lenght in self.table_col_width])
        self.pdf.line(x1,y1,x1+w,y1)


    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if DEBUG: print "STARTTAG", tag, attrs
        if tag=='b' or tag=='i' or tag=='u':
            self.set_style(tag,1)
        if tag=='a':
            self.href=attrs['href']
        if tag=='br':
            self.pdf.ln(5)
        if tag=='p':
            self.pdf.ln(5)
            if attrs:
                if attrs: self.align = attrs.get('align')
        if tag in self.hsize:
            k = self.hsize[tag]
            self.pdf.ln(5*k)
            self.pdf.set_text_color(150,0,0)
            self.pdf.set_font_size(12 * k)
            if attrs: self.align = attrs.get('align')
        if tag=='hr':
            self.put_line()
        if tag=='pre':
            self.pdf.set_font('Courier','',11)
            self.pdf.set_font_size(11)
            self.set_style('B',False)
            self.set_style('I',False)
            self.pre = True
        if tag=='blockquote':
            self.set_text_color(100,0,45)
            self.pdf.ln(3)
        if tag=='ul':
            self.indent+=1
            self.bullet.append('\x95')
        if tag=='ol':
            self.indent+=1
            self.bullet.append(0)
        if tag=='li':
            self.pdf.ln(self.h+2)
            self.pdf.set_text_color(190,0,0)
            bullet = self.bullet[self.indent-1]
            if not isinstance(bullet, basestring):
                bullet += 1
                self.bullet[self.indent-1] = bullet
                bullet = "%s. " % bullet
            self.pdf.write(self.h,'%s%s ' % (' '*5*self.indent, bullet))
            self.set_text_color()
        if tag=='font':
            # save previous font state:
            self.font_stack.append((self.font_face, self.font_size, self.color))
            if 'color' in attrs:
                self.color = hex2dec(attrs['color'])
                self.set_text_color(*color)
                self.color = color
            if 'face' in attrs:
                face = attrs.get('face').lower()
                self.pdf.set_font(face)
                self.font_face = face
            if 'size' in attrs:
                size = int(attrs.get('size'))
                self.pdf.set_font(self.font_face, size=int(size))
                self.font_size = size
        if tag=='table':
            self.table = dict([(k.lower(), v) for k,v in attrs.items()])
            if not 'width' in self.table:
                self.table['width'] = '100%'
            if self.table['width'][-1]=='%':
                w = self.pdf.w - self.pdf.r_margin - self.pdf.l_margin
                w *= int(self.table['width'][:-1])/100.0
                self.table_offset = (self.pdf.w-w)/2.0
            self.table_col_width = []
            self.theader_out = self.tfooter_out = False
            self.theader = []
            self.tfooter = []
            self.thead = None
            self.tfoot = None
            self.table_h = 0
            self.pdf.ln()
        if tag=='tr':
            self.tr = dict([(k.lower(), v) for k,v in attrs.items()])
            self.table_col_index = 0
            self.pdf.set_x(self.table_offset)
        if tag=='td':
            self.td = dict([(k.lower(), v) for k,v in attrs.items()])
        if tag=='th':
            self.td = dict([(k.lower(), v) for k,v in attrs.items()])
            self.th = True
            if 'width' in self.td:
                self.table_col_width.append(self.td['width'])
        if tag=='thead':
            self.thead = {}
        if tag=='tfoot':
            self.tfoot = {}
        if tag=='img':
            if 'src' in attrs:
                x = self.pdf.get_x()
                y = self.pdf.get_y()
                w = px2mm(attrs.get('width', 0))
                h = px2mm(attrs.get('height',0))
                if self.align and self.align[0].upper() == 'C':
                    x = (self.pdf.w-x)/2.0 - w/2.0
                self.pdf.image(self.image_map(attrs['src']),
                               x, y, w, h, link=self.href)
                self.pdf.set_x(x+w)
                self.pdf.set_y(y+h)
        if tag=='b' or tag=='i' or tag=='u':
            self.set_style(tag, True)
        if tag=='center':
            self.align = 'Center'

    def handle_endtag(self, tag):
        #Closing tag
        if DEBUG: print "ENDTAG", tag
        if tag=='h1' or tag=='h2' or tag=='h3' or tag=='h4':
            self.pdf.ln(6)
            self.set_font()
            self.set_style()
            self.align = None
        if tag=='pre':
            self.pdf.set_font(self.font or 'Times','',12)
            self.pdf.set_font_size(12)
            self.pre=False
        if tag=='blockquote':
            self.set_text_color(0,0,0)
            self.pdf.ln(3)
        if tag=='strong':
            tag='b'
        if tag=='em':
            tag='i'
        if tag=='b' or tag=='i' or tag=='u':
            self.set_style(tag, False)
        if tag=='a':
            self.href=''
        if tag=='p':
            self.align=''
        if tag in ('ul', 'ol'):
            self.indent-=1
            self.bullet.pop()
        if tag=='table':
            if not self.tfooter_out:
                self.output_table_footer()
            self.table = None
            self.th = False
            self.theader = None
            self.tfooter = None
            self.pdf.ln()
        if tag=='thead':
            self.thead = None
        if tag=='tfoot':
            self.tfoot = None
        if tag=='tbody':
            # draw a line separator between table bodies
            self.pdf.set_x(self.table_offset)
            self.output_table_sep()
        if tag=='tr':
            h = self.table_h
            if self.tfoot is None:
                self.pdf.ln(h)
            self.tr = None
        if tag=='td' or tag=='th':
            if self.th:
                if DEBUG: print "revert style"
                self.set_style('B', False) # revert style
            self.table_col_index += int(self.td.get('colspan','1'))
            self.td = None
            self.th = False
        if tag=='font':
            # recover last font state
            face, size, color = self.font_stack.pop()
            if face:
                self.pdf.set_text_color(0,0,0)
                self.color = None
            self.set_font(face, size)                
            self.font = None
        if tag=='center':
            self.align = None

    def set_font(self, face=None, size=None):
        if face:
            self.font_face = face
        if size:
            self.font_size = size
            self.h = size / 72.0*25.4
            if DEBUG: print "H", self.h
        self.pdf.set_font(self.font_face or 'times','',12)
        self.pdf.set_font_size(self.font_size or 12)
        self.set_style('u', False)
        self.set_style('b', False)
        self.set_style('i', False)
        self.set_text_color()        

    def set_style(self, tag=None, enable=None):
        #Modify style and select corresponding font
        if tag:
            t = self.style.get(tag.lower())
            self.style[tag.lower()] = enable
        style=''
        for s in ('b','i','u'):
            if self.style.get(s):
                style+=s
        if DEBUG: print "SET_FONT_STYLE", style
        self.pdf.set_font('',style)

    def set_text_color(self, r=None, g=0, b=0):
        if r is None:
            self.pdf.set_text_color(self.r,self.g,self.b)
        else:
            self.pdf.set_text_color(r, g, b)
            self.r = r
            self.g = g
            self.b = b
    
    def put_link(self, url, txt):
        #Put a hyperlink
        self.set_text_color(0,0,255)
        self.set_style('u', True)
        self.pdf.write(5,txt,url)
        self.set_style('u', False)
        self.set_text_color(0)

    def put_line(self):
        self.pdf.ln(2)
        self.pdf.line(self.pdf.get_x(),self.pdf.get_y(),self.pdf.get_x()+187,self.pdf.get_y())
        self.pdf.ln(3)

class HTMLMixin(object):
    def write_html(self, text, image_map=None):
        "Parse HTML and convert it to PDF"
        h2p = HTML2FPDF(self, image_map)
        h2p.feed(text)

