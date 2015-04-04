#!/usr/bin/env python
# -*- coding: latin-1 -*-
# ****************************************************************************
# * Software: FPDF for python                                                *
# * Version:  1.7.1                                                          *
# * Date:     2010-09-10                                                     *
# * Last update: 2012-08-16                                                  *
# * License:  LGPL v3.0                                                      *
# *                                                                          *
# * Original Author (PHP):  Olivier PLATHEY 2004-12-31                       *
# * Ported to Python 2.4 by Max (maxpat78@yahoo.it) on 2006-05               *
# * Maintainer:  Mariano Reingart (reingart@gmail.com) et al since 2008 est. *
# * NOTE: 'I' and 'D' destinations are disabled, and simply print to STDOUT  *
# ****************************************************************************

from datetime import datetime
import math
import errno
import os, sys, zlib, struct, re, tempfile, struct

try:
    import cPickle as pickle
except ImportError:
    import pickle

# Check if PIL is available (tries importing both pypi version and corrected or manually installed versions).
# Necessary for JPEG and GIF support.
try:
    try:
        import Image
    except:
        from PIL import Image
except ImportError:
    Image = None


from ttfonts import TTFontFile
from fonts import fpdf_charwidths
from php import substr, sprintf, print_r, UTF8ToUTF16BE, UTF8StringToArray


# Global variables
FPDF_VERSION = '1.7.1'
FPDF_FONT_DIR = os.path.join(os.path.dirname(__file__),'font')
SYSTEM_TTFONTS = None

PY3K = sys.version_info >= (3, 0)

def set_global(var, val):
    globals()[var] = val


class FPDF(object):
    "PDF Generation class"

    def __init__(self, orientation='P',unit='mm',format='A4'):
        # Some checks
        self._dochecks()
        # Initialization of properties
        self.offsets={}                 # array of object offsets
        self.page=0                     # current page number
        self.n=2                        # current object number
        self.buffer=''                  # buffer holding in-memory PDF
        self.pages={}                   # array containing pages
        self.orientation_changes={}     # array indicating orientation changes
        self.state=0                    # current document state
        self.fonts={}                   # array of used fonts
        self.font_files={}              # array of font files
        self.diffs={}                   # array of encoding differences
        self.images={}                  # array of used images
        self.page_links={}              # array of links in pages
        self.links={}                   # array of internal links
        self.in_footer=0                # flag set when processing footer
        self.lastw=0
        self.lasth=0                    # height of last cell printed
        self.font_family=''             # current font family
        self.font_style=''              # current font style
        self.font_size_pt=12            # current font size in points
        self.underline=0                # underlining flag
        self.draw_color='0 G'
        self.fill_color='0 g'
        self.text_color='0 g'
        self.color_flag=0               # indicates whether fill and text colors are different
        self.ws=0                       # word spacing
        self.angle=0
        # Standard fonts
        self.core_fonts={'courier':'Courier','courierB':'Courier-Bold','courierI':'Courier-Oblique','courierBI':'Courier-BoldOblique',
            'helvetica':'Helvetica','helveticaB':'Helvetica-Bold','helveticaI':'Helvetica-Oblique','helveticaBI':'Helvetica-BoldOblique',
            'times':'Times-Roman','timesB':'Times-Bold','timesI':'Times-Italic','timesBI':'Times-BoldItalic',
            'symbol':'Symbol','zapfdingbats':'ZapfDingbats'}
        # Scale factor
        if(unit=='pt'):
            self.k=1
        elif(unit=='mm'):
            self.k=72/25.4
        elif(unit=='cm'):
            self.k=72/2.54
        elif(unit=='in'):
            self.k=72
        else:
            self.error('Incorrect unit: '+unit)
        # Page format
        if(isinstance(format,basestring)):
            format=format.lower()
            if(format=='a3'):
                format=(841.89,1190.55)
            elif(format=='a4'):
                format=(595.28,841.89)
            elif(format=='a5'):
                format=(420.94,595.28)
            elif(format=='letter'):
                format=(612,792)
            elif(format=='legal'):
                format=(612,1008)
            else:
                self.error('Unknown page format: '+format)
            self.fw_pt=format[0]
            self.fh_pt=format[1]
        else:
            self.fw_pt=format[0]*self.k
            self.fh_pt=format[1]*self.k
        self.fw=self.fw_pt/self.k
        self.fh=self.fh_pt/self.k
        # Page orientation
        orientation=orientation.lower()
        if(orientation=='p' or orientation=='portrait'):
            self.def_orientation='P'
            self.w_pt=self.fw_pt
            self.h_pt=self.fh_pt
        elif(orientation=='l' or orientation=='landscape'):
            self.def_orientation='L'
            self.w_pt=self.fh_pt
            self.h_pt=self.fw_pt
        else:
            self.error('Incorrect orientation: '+orientation)
        self.cur_orientation=self.def_orientation
        self.w=self.w_pt/self.k
        self.h=self.h_pt/self.k
        # Page margins (1 cm)
        margin=28.35/self.k
        self.set_margins(margin,margin)
        # Interior cell margin (1 mm)
        self.c_margin=margin/10.0
        # line width (0.2 mm)
        self.line_width=.567/self.k
        # Automatic page break
        self.set_auto_page_break(1,2*margin)
        # Full width display mode
        self.set_display_mode('fullwidth')
        # Enable compression
        self.set_compression(1)
        # Set default PDF version number
        self.pdf_version='1.3'

    def set_margins(self, left,top,right=-1):
        "Set left, top and right margins"
        self.l_margin=left
        self.t_margin=top
        if(right==-1):
            right=left
        self.r_margin=right

    def set_left_margin(self, margin):
        "Set left margin"
        self.l_margin=margin
        if(self.page>0 and self.x<margin):
            self.x=margin

    def set_top_margin(self, margin):
        "Set top margin"
        self.t_margin=margin

    def set_right_margin(self, margin):
        "Set right margin"
        self.r_margin=margin

    def set_auto_page_break(self, auto,margin=0):
        "Set auto page break mode and triggering margin"
        self.auto_page_break=auto
        self.b_margin=margin
        self.page_break_trigger=self.h-margin

    def set_display_mode(self, zoom,layout='continuous'):
        "Set display mode in viewer"
        if(zoom=='fullpage' or zoom=='fullwidth' or zoom=='real' or zoom=='default' or not isinstance(zoom,basestring)):
            self.zoom_mode=zoom
        else:
            self.error('Incorrect zoom display mode: '+zoom)
        if(layout=='single' or layout=='continuous' or layout=='two' or layout=='default'):
            self.layout_mode=layout
        else:
            self.error('Incorrect layout display mode: '+layout)

    def set_compression(self, compress):
        "Set page compression"
        self.compress=compress

    def set_title(self, title):
        "Title of document"
        self.title=title

    def set_subject(self, subject):
        "Subject of document"
        self.subject=subject

    def set_author(self, author):
        "Author of document"
        self.author=author

    def set_keywords(self, keywords):
        "Keywords of document"
        self.keywords=keywords

    def set_creator(self, creator):
        "Creator of document"
        self.creator=creator

    def alias_nb_pages(self, alias='{nb}'):
        "Define an alias for total number of pages"
        self.str_alias_nb_pages=alias
        return alias

    def error(self, msg):
        "Fatal error"
        raise RuntimeError('FPDF error: '+msg)

    def open(self):
        "Begin document"
        self.state=1

    def close(self):
        "Terminate document"
        if(self.state==3):
            return
        if(self.page==0):
            self.add_page()
        #Page footer
        self.in_footer=1
        self.footer()
        self.in_footer=0
        #close page
        self._endpage()
        #close document
        self._enddoc()

    def add_page(self, orientation=''):
        "Start a new page"
        if(self.state==0):
            self.open()
        family=self.font_family
        if self.underline:
            style = self.font_style + 'U'
        else:
            style = self.font_style
        size=self.font_size_pt
        lw=self.line_width
        dc=self.draw_color
        fc=self.fill_color
        tc=self.text_color
        cf=self.color_flag
        if(self.page>0):
            #Page footer
            self.in_footer=1
            self.footer()
            self.in_footer=0
            #close page
            self._endpage()
        #Start new page
        self._beginpage(orientation)
        #Set line cap style to square
        self._out('2 J')
        #Set line width
        self.line_width=lw
        self._out(sprintf('%.2f w',lw*self.k))
        #Set font
        if(family):
            self.set_font(family,style,size)
        #Set colors
        self.draw_color=dc
        if(dc!='0 G'):
            self._out(dc)
        self.fill_color=fc
        if(fc!='0 g'):
            self._out(fc)
        self.text_color=tc
        self.color_flag=cf
        #Page header
        self.header()
        #Restore line width
        if(self.line_width!=lw):
            self.line_width=lw
            self._out(sprintf('%.2f w',lw*self.k))
        #Restore font
        if(family):
            self.set_font(family,style,size)
        #Restore colors
        if(self.draw_color!=dc):
            self.draw_color=dc
            self._out(dc)
        if(self.fill_color!=fc):
            self.fill_color=fc
            self._out(fc)
        self.text_color=tc
        self.color_flag=cf

    def header(self):
        "Header to be implemented in your own inherited class"
        pass

    def footer(self):
        "Footer to be implemented in your own inherited class"
        pass

    def page_no(self):
        "Get current page number"
        return self.page

    def set_draw_color(self, r,g=-1,b=-1):
        "Set color for all stroking operations"
        if((r==0 and g==0 and b==0) or g==-1):
            self.draw_color=sprintf('%.3f G',r/255.0)
        else:
            self.draw_color=sprintf('%.3f %.3f %.3f RG',r/255.0,g/255.0,b/255.0)
        if(self.page>0):
            self._out(self.draw_color)

    def set_fill_color(self,r,g=-1,b=-1):
        "Set color for all filling operations"
        if((r==0 and g==0 and b==0) or g==-1):
            self.fill_color=sprintf('%.3f g',r/255.0)
        else:
            self.fill_color=sprintf('%.3f %.3f %.3f rg',r/255.0,g/255.0,b/255.0)
        self.color_flag=(self.fill_color!=self.text_color)
        if(self.page>0):
            self._out(self.fill_color)

    def set_text_color(self, r,g=-1,b=-1):
        "Set color for text"
        if((r==0 and g==0 and b==0) or g==-1):
            self.text_color=sprintf('%.3f g',r/255.0)
        else:
            self.text_color=sprintf('%.3f %.3f %.3f rg',r/255.0,g/255.0,b/255.0)
        self.color_flag=(self.fill_color!=self.text_color)

    def get_string_width(self, s):
        "Get width of a string in the current font"
        cw=self.current_font['cw']
        w=0
        l=len(s)
        if self.unifontsubset:
            for char in s:
                char = ord(char)
                if len(cw) > char:
                    w += cw[char] # ord(cw[2*char])<<8 + ord(cw[2*char+1])
                #elif (char>0 and char<128 and isset($cw[chr($char)])) { $w += $cw[chr($char)]; }
                elif (self.current_font['desc']['MissingWidth']) :
                    w += self.current_font['desc']['MissingWidth']
                #elif (isset($this->CurrentFont['MissingWidth'])) { $w += $this->CurrentFont['MissingWidth']; }
                else:
                    w += 500
        else:
            for i in xrange(0, l):
                w += cw.get(s[i],0)
        return w*self.font_size/1000.0

    def set_line_width(self, width):
        "Set line width"
        self.line_width=width
        if(self.page>0):
            self._out(sprintf('%.2f w',width*self.k))

    def line(self, x1,y1,x2,y2):
        "Draw a line"
        self._out(sprintf('%.2f %.2f m %.2f %.2f l S',x1*self.k,(self.h-y1)*self.k,x2*self.k,(self.h-y2)*self.k))

    def _set_dash(self, dash_length=False, space_length=False):
        if(dash_length and space_length):
            s = sprintf('[%.3f %.3f] 0 d', dash_length*self.k, space_length*self.k)
        else:
            s = '[] 0 d'
        self._out(s)

    def dashed_line(self, x1,y1,x2,y2, dash_length=1, space_length=1):
        """Draw a dashed line. Same interface as line() except:
           - dash_length: Length of the dash
           - space_length: Length of the space between dashes"""
        self._set_dash(dash_length, space_length)
        self.line(x1, y1, x2, y2)
        self._set_dash()

    def rect(self, x,y,w,h,style=''):
        "Draw a rectangle"
        if(style=='F'):
            op='f'
        elif(style=='FD' or style=='DF'):
            op='B'
        else:
            op='S'
        self._out(sprintf('%.2f %.2f %.2f %.2f re %s',x*self.k,(self.h-y)*self.k,w*self.k,-h*self.k,op))

    def add_font(self, family, style='', fname='', uni=False):
        "Add a TrueType or Type1 font"
        family = family.lower()
        if (fname == ''):
            fname = family.replace(' ','') + style.lower() + '.pkl'
        if (family == 'arial'):
            family = 'helvetica'
        style = style.upper()
        if (style == 'IB'):
            style = 'BI'
        fontkey = family+style
        if fontkey in self.fonts:
            # Font already added!
            return
        if (uni):
            global SYSTEM_TTFONTS
            if os.path.exists(fname):
                ttffilename = fname
            elif (FPDF_FONT_DIR and
                os.path.exists(os.path.join(FPDF_FONT_DIR, fname))):
                ttffilename = os.path.join(FPDF_FONT_DIR, fname)
            elif (SYSTEM_TTFONTS and
                os.path.exists(os.path.join(SYSTEM_TTFONTS, fname))):
                ttffilename = os.path.join(SYSTEM_TTFONTS, fname)
            else:
                raise RuntimeError("TTF Font file not found: %s" % fname)
            unifilename = os.path.splitext(ttffilename)[0] + '.pkl'
            name = ''
            if os.path.exists(unifilename):
                fh = open(unifilename)
                try:
                    font_dict = pickle.load(fh)
                finally:
                    fh.close()
            else:
                ttf = TTFontFile()
                ttf.getMetrics(ttffilename)
                desc = {
                    'Ascent': int(round(ttf.ascent, 0)),
                    'Descent': int(round(ttf.descent, 0)),
                    'CapHeight': int(round(ttf.capHeight, 0)),
                    'Flags': ttf.flags,
                    'FontBBox': "[%s %s %s %s]" % (
                        int(round(ttf.bbox[0], 0)),
                        int(round(ttf.bbox[1], 0)),
                        int(round(ttf.bbox[2], 0)),
                        int(round(ttf.bbox[3], 0))),
                    'ItalicAngle': int(ttf.italicAngle),
                    'StemV': int(round(ttf.stemV, 0)),
                    'MissingWidth': int(round(ttf.defaultWidth, 0)),
                    }
                # Generate metrics .pkl file
                font_dict = {
                    'name': re.sub('[ ()]', '', ttf.fullName),
                    'type': 'TTF',
                    'desc': desc,
                    'up': round(ttf.underlinePosition),
                    'ut': round(ttf.underlineThickness),
                    'ttffile': ttffilename,
                    'fontkey': fontkey,
                    'originalsize': os.stat(ttffilename).st_size,
                    'cw': ttf.charWidths,
                    }
                try:
                    fh = open(unifilename, "w")
                    pickle.dump(font_dict, fh)
                    fh.close()
                except IOError, e:
                    if not e.errno == errno.EACCES:
                        raise  # Not a permission error.
                del ttf
            if hasattr(self,'str_alias_nb_pages'):
                sbarr = range(0,57)   # include numbers in the subset!
            else:
                sbarr = range(0,32)
            self.fonts[fontkey] = {
                'i': len(self.fonts)+1, 'type': font_dict['type'],
                'name': font_dict['name'], 'desc': font_dict['desc'],
                'up': font_dict['up'], 'ut': font_dict['ut'],
                'cw': font_dict['cw'],
                'ttffile': font_dict['ttffile'], 'fontkey': fontkey,
                'subset': sbarr, 'unifilename': unifilename,
                }
            self.font_files[fontkey] = {'length1': font_dict['originalsize'],
                                        'type': "TTF", 'ttffile': ttffilename}
            self.font_files[fname] = {'type': "TTF"}
        else:
            fontfile = open(fname)
            try:
                font_dict = pickle.load(fontfile)
            finally:
                fontfile.close()
            self.fonts[fontkey] = {'i': len(self.fonts)+1}
            self.fonts[fontkey].update(font_dict)
            if (diff):
                #Search existing encodings
                d = 0
                nb = len(self.diffs)
                for i in xrange(1, nb+1):
                    if(self.diffs[i] == diff):
                        d = i
                        break
                if (d == 0):
                    d = nb + 1
                    self.diffs[d] = diff
                self.fonts[fontkey]['diff'] = d
            filename = font_dict.get('filename')
            if (filename):
                if (type == 'TrueType'):
                    self.font_files[filename]={'length1': originalsize}
                else:
                    self.font_files[filename]={'length1': size1,
                                               'length2': size2}

    def set_font(self, family,style='',size=0):
        "Select a font; size given in points"
        family=family.lower()
        if(family==''):
            family=self.font_family
        if(family=='arial'):
            family='helvetica'
        elif(family=='symbol' or family=='zapfdingbats'):
            style=''
        style=style.upper()
        if('U' in style):
            self.underline=1
            style=style.replace('U','')
        else:
            self.underline=0
        if(style=='IB'):
            style='BI'
        if(size==0):
            size=self.font_size_pt
        #Test if font is already selected
        if(self.font_family==family and self.font_style==style and self.font_size_pt==size):
            return
        #Test if used for the first time
        fontkey=family+style
        if fontkey not in self.fonts:
            #Check if one of the standard fonts
            if fontkey in self.core_fonts:
                if fontkey not in fpdf_charwidths:
                    #Load metric file
                    name=os.path.join(FPDF_FONT_DIR,family)
                    if(family=='times' or family=='helvetica'):
                        name+=style.lower()
                    execfile(name+'.font')
                    if fontkey not in fpdf_charwidths:
                        self.error('Could not include font metric file for'+fontkey)
                i=len(self.fonts)+1
                self.fonts[fontkey]={'i':i,'type':'core','name':self.core_fonts[fontkey],'up':-100,'ut':50,'cw':fpdf_charwidths[fontkey]}
            else:
                self.error('Undefined font: '+family+' '+style)
        #Select it
        self.font_family=family
        self.font_style=style
        self.font_size_pt=size
        self.font_size=size/self.k
        self.current_font=self.fonts[fontkey]
        self.unifontsubset = (self.fonts[fontkey]['type'] == 'TTF')
        if(self.page>0):
            self._out(sprintf('BT /F%d %.2f Tf ET',self.current_font['i'],self.font_size_pt))

    def set_font_size(self, size):
        "Set font size in points"
        if(self.font_size_pt==size):
            return
        self.font_size_pt=size
        self.font_size=size/self.k
        if(self.page>0):
            self._out(sprintf('BT /F%d %.2f Tf ET',self.current_font['i'],self.font_size_pt))

    def add_link(self):
        "Create a new internal link"
        n=len(self.links)+1
        self.links[n]=(0,0)
        return n

    def set_link(self, link,y=0,page=-1):
        "Set destination of internal link"
        if(y==-1):
            y=self.y
        if(page==-1):
            page=self.page
        self.links[link]=[page,y]

    def link(self, x,y,w,h,link):
        "Put a link on the page"
        if not self.page in self.page_links:
            self.page_links[self.page] = []
        self.page_links[self.page] += [(x*self.k,self.h_pt-y*self.k,w*self.k,h*self.k,link),]

    def text(self, x, y, txt=''):
        "Output a string"
        txt = self.normalize_text(txt)
        if (self.unifontsubset):
            txt2 = self._escape(UTF8ToUTF16BE(txt, False))
            for uni in UTF8StringToArray(txt):
                self.current_font['subset'].append(uni)
        else:
            txt2 = self._escape(txt)
        s=sprintf('BT %.2f %.2f Td (%s) Tj ET',x*self.k,(self.h-y)*self.k, txt2)
        if(self.underline and txt!=''):
            s+=' '+self._dounderline(x,y,txt)
        if(self.color_flag):
            s='q '+self.text_color+' '+s+' Q'
        self._out(s)

    def rotate(self, angle, x=None, y=None):
        if x is None:
            x = self.x
        if y is None:
            y = self.y;
        if self.angle!=0:
            self._out('Q')
        self.angle = angle
        if angle!=0:
            angle *= math.pi/180;
            c = math.cos(angle);
            s = math.sin(angle);
            cx = x*self.k;
            cy = (self.h-y)*self.k
            s = sprintf('q %.5F %.5F %.5F %.5F %.2F %.2F cm 1 0 0 1 %.2F %.2F cm',c,s,-s,c,cx,cy,-cx,-cy)
            self._out(s)

    def accept_page_break(self):
        "Accept automatic page break or not"
        return self.auto_page_break

    def cell(self, w,h=0,txt='',border=0,ln=0,align='',fill=0,link=''):
        "Output a cell"
        txt = self.normalize_text(txt)
        k=self.k
        if(self.y+h>self.page_break_trigger and not self.in_footer and self.accept_page_break()):
            #Automatic page break
            x=self.x
            ws=self.ws
            if(ws>0):
                self.ws=0
                self._out('0 Tw')
            self.add_page(self.cur_orientation)
            self.x=x
            if(ws>0):
                self.ws=ws
                self._out(sprintf('%.3f Tw',ws*k))
        if(w==0):
            w=self.w-self.r_margin-self.x
        s=''
        if(fill==1 or border==1):
            if(fill==1):
                if border==1:
                    op='B'
                else:
                    op='f'
            else:
                op='S'
            s=sprintf('%.2f %.2f %.2f %.2f re %s ',self.x*k,(self.h-self.y)*k,w*k,-h*k,op)
        if(isinstance(border,basestring)):
            x=self.x
            y=self.y
            if('L' in border):
                s+=sprintf('%.2f %.2f m %.2f %.2f l S ',x*k,(self.h-y)*k,x*k,(self.h-(y+h))*k)
            if('T' in border):
                s+=sprintf('%.2f %.2f m %.2f %.2f l S ',x*k,(self.h-y)*k,(x+w)*k,(self.h-y)*k)
            if('R' in border):
                s+=sprintf('%.2f %.2f m %.2f %.2f l S ',(x+w)*k,(self.h-y)*k,(x+w)*k,(self.h-(y+h))*k)
            if('B' in border):
                s+=sprintf('%.2f %.2f m %.2f %.2f l S ',x*k,(self.h-(y+h))*k,(x+w)*k,(self.h-(y+h))*k)
        if(txt!=''):
            if(align=='R'):
                dx=w-self.c_margin-self.get_string_width(txt)
            elif(align=='C'):
                dx=(w-self.get_string_width(txt))/2.0
            else:
                dx=self.c_margin
            if(self.color_flag):
                s+='q '+self.text_color+' '

            # If multibyte, Tw has no effect - do word spacing using an adjustment before each space
            if (self.ws and self.unifontsubset):
                for uni in UTF8StringToArray(txt):
                    self.current_font['subset'].append(uni)
                space = self._escape(UTF8ToUTF16BE(' ', False))
                s += sprintf('BT 0 Tw %.2F %.2F Td [',(self.x + dx) * k,(self.h - (self.y + 0.5*h+ 0.3 * self.font_size)) * k)
                t = txt.split(' ')
                numt = len(t)
                for i in range(numt):
                    tx = t[i]
                    tx = '(' + self._escape(UTF8ToUTF16BE(tx, False)) + ')'
                    s += sprintf('%s ', tx);
                    if ((i+1)<numt):
                        adj = -(self.ws * self.k) * 1000 / self.font_size_pt
                        s += sprintf('%d(%s) ', adj, space)
                s += '] TJ'
                s += ' ET'
            else:
                if (self.unifontsubset):
                    txt2 = self._escape(UTF8ToUTF16BE(txt, False))
                    for uni in UTF8StringToArray(txt):
                        self.current_font['subset'].append(uni)
                else:
                    txt2 = self._escape(txt)
                s += sprintf('BT %.2f %.2f Td (%s) Tj ET',(self.x+dx)*k,(self.h-(self.y+.5*h+.3*self.font_size))*k,txt2)

            if(self.underline):
                s+=' '+self._dounderline(self.x+dx,self.y+.5*h+.3*self.font_size,txt)
            if(self.color_flag):
                s+=' Q'
            if(link):
                self.link(self.x+dx,self.y+.5*h-.5*self.font_size,self.get_string_width(txt),self.font_size,link)
        if(s):
            self._out(s)
        self.lasth=h
        if(ln>0):
            #Go to next line
            self.y+=h
            if(ln==1):
                self.x=self.l_margin
        else:
            self.x+=w

    def multi_cell(self, w, h, txt='', border=0, align='J', fill=0, split_only=False):
        "Output text with automatic or explicit line breaks"
        txt = self.normalize_text(txt)
        ret = [] # if split_only = True, returns splited text cells
        cw=self.current_font['cw']
        if(w==0):
            w=self.w-self.r_margin-self.x
        wmax=(w-2*self.c_margin)*1000.0/self.font_size
        s=txt.replace("\r",'')
        nb=len(s)
        if(nb>0 and s[nb-1]=="\n"):
            nb-=1
        b=0
        if(border):
            if(border==1):
                border='LTRB'
                b='LRT'
                b2='LR'
            else:
                b2=''
                if('L' in border):
                    b2+='L'
                if('R' in border):
                    b2+='R'
                if ('T' in border):
                    b=b2+'T'
                else:
                    b=b2
        sep=-1
        i=0
        j=0
        l=0
        ns=0
        nl=1
        while(i<nb):
            #Get next character
            c=s[i]
            if(c=="\n"):
                #Explicit line break
                if(self.ws>0):
                    self.ws=0
                    if not split_only:
                        self._out('0 Tw')
                if not split_only:
                    self.cell(w,h,substr(s,j,i-j),b,2,align,fill)
                else:
                    ret.append(substr(s,j,i-j))
                i+=1
                sep=-1
                j=i
                l=0
                ns=0
                nl+=1
                if(border and nl==2):
                    b=b2
                continue
            if(c==' '):
                sep=i
                ls=l
                ns+=1
            if self.unifontsubset:
                l += self.get_string_width(c) / self.font_size*1000.0
            else:
                l += cw.get(c,0)
            if(l>wmax):
                #Automatic line break
                if(sep==-1):
                    if(i==j):
                        i+=1
                    if(self.ws>0):
                        self.ws=0
                        if not split_only:
                            self._out('0 Tw')
                    if not split_only:
                        self.cell(w,h,substr(s,j,i-j),b,2,align,fill)
                    else:
                        ret.append(substr(s,j,i-j))
                else:
                    if(align=='J'):
                        if ns>1:
                            self.ws=(wmax-ls)/1000.0*self.font_size/(ns-1)
                        else:
                            self.ws=0
                        if not split_only:
                            self._out(sprintf('%.3f Tw',self.ws*self.k))
                    if not split_only:
                        self.cell(w,h,substr(s,j,sep-j),b,2,align,fill)
                    else:
                        ret.append(substr(s,j,sep-j))
                    i=sep+1
                sep=-1
                j=i
                l=0
                ns=0
                nl+=1
                if(border and nl==2):
                    b=b2
            else:
                i+=1
        #Last chunk
        if(self.ws>0):
            self.ws=0
            if not split_only:
                self._out('0 Tw')
        if(border and 'B' in border):
            b+='B'
        if not split_only:
            self.cell(w,h,substr(s,j,i-j),b,2,align,fill)
            self.x=self.l_margin
        else:
            ret.append(substr(s,j,i-j))
        return ret

    def write(self, h, txt='', link=''):
        "Output text in flowing mode"
        txt = self.normalize_text(txt)
        cw=self.current_font['cw']
        w=self.w-self.r_margin-self.x
        wmax=(w-2*self.c_margin)*1000.0/self.font_size
        s=txt.replace("\r",'')
        nb=len(s)
        sep=-1
        i=0
        j=0
        l=0
        nl=1
        while(i<nb):
            #Get next character
            c=s[i]
            if(c=="\n"):
                #Explicit line break
                self.cell(w,h,substr(s,j,i-j),0,2,'',0,link)
                i+=1
                sep=-1
                j=i
                l=0
                if(nl==1):
                    self.x=self.l_margin
                    w=self.w-self.r_margin-self.x
                    wmax=(w-2*self.c_margin)*1000.0/self.font_size
                nl+=1
                continue
            if(c==' '):
                sep=i
            if self.unifontsubset:
                l += self.get_string_width(c) / self.font_size*1000.0
            else:
                l += cw.get(c,0)
            if(l>wmax):
                #Automatic line break
                if(sep==-1):
                    if(self.x>self.l_margin):
                        #Move to next line
                        self.x=self.l_margin
                        self.y+=h
                        w=self.w-self.r_margin-self.x
                        wmax=(w-2*self.c_margin)*1000.0/self.font_size
                        i+=1
                        nl+=1
                        continue
                    if(i==j):
                        i+=1
                    self.cell(w,h,substr(s,j,i-j),0,2,'',0,link)
                else:
                    self.cell(w,h,substr(s,j,sep-j),0,2,'',0,link)
                    i=sep+1
                sep=-1
                j=i
                l=0
                if(nl==1):
                    self.x=self.l_margin
                    w=self.w-self.r_margin-self.x
                    wmax=(w-2*self.c_margin)*1000.0/self.font_size
                nl+=1
            else:
                i+=1
        #Last chunk
        if(i!=j):
            self.cell(l/1000.0*self.font_size,h,substr(s,j),0,0,'',0,link)

    def image(self, name, x=None, y=None, w=0,h=0,type='',link=''):
        "Put an image on the page"
        if not name in self.images:
            #First use of image, get info
            if(type==''):
                pos=name.rfind('.')
                if(not pos):
                    self.error('image file has no extension and no type was specified: '+name)
                type=substr(name,pos+1)
            type=type.lower()
            if(type=='jpg' or type=='jpeg'):
                info=self._parsejpg(name)
            elif(type=='png'):
                info=self._parsepng(name)
            else:
                #Allow for additional formats
                #maybe the image is not showing the correct extension,
                #but the header is OK,
                succeed_parsing = False
                #try all the parsing functions
                parsing_functions = [self._parsejpg,self._parsepng,self._parsegif]
                for pf in parsing_functions:
                    try:
                        info = pf(name)
                        succeed_parsing = True
                        break;
                    except:
                        pass
                #last resource
                if not succeed_parsing:
                    mtd='_parse'+type
                    if not hasattr(self,mtd):
                        self.error('Unsupported image type: '+type)
                    info=getattr(self, mtd)(name)
                mtd='_parse'+type
                if not hasattr(self,mtd):
                    self.error('Unsupported image type: '+type)
                info=getattr(self, mtd)(name)
            info['i']=len(self.images)+1
            self.images[name]=info
        else:
            info=self.images[name]
        #Automatic width and height calculation if needed
        if(w==0 and h==0):
            #Put image at 72 dpi
            w=info['w']/self.k
            h=info['h']/self.k
        elif(w==0):
            w=h*info['w']/info['h']
        elif(h==0):
            h=w*info['h']/info['w']
        # Flowing mode
        if y is None:
            if (self.y + h > self.page_break_trigger and not self.in_footer and self.accept_page_break()):
                #Automatic page break
                x = self.x
                self.add_page(self.cur_orientation)
                self.x = x
            y = self.y
            self.y += h
        if x is None:
            x = self.x
        self._out(sprintf('q %.2f 0 0 %.2f %.2f %.2f cm /I%d Do Q',w*self.k,h*self.k,x*self.k,(self.h-(y+h))*self.k,info['i']))
        if(link):
            self.link(x,y,w,h,link)

    def ln(self, h=''):
        "Line Feed; default value is last cell height"
        self.x=self.l_margin
        if(isinstance(h, basestring)):
            self.y+=self.lasth
        else:
            self.y+=h

    def get_x(self):
        "Get x position"
        return self.x

    def set_x(self, x):
        "Set x position"
        if(x>=0):
            self.x=x
        else:
            self.x=self.w+x

    def get_y(self):
        "Get y position"
        return self.y

    def set_y(self, y):
        "Set y position and reset x"
        self.x=self.l_margin
        if(y>=0):
            self.y=y
        else:
            self.y=self.h+y

    def set_xy(self, x,y):
        "Set x and y positions"
        self.set_y(y)
        self.set_x(x)

    def output(self, name='',dest=''):
        "Output PDF to some destination"
        #Finish document if necessary
        if(self.state<3):
            self.close()
        dest=dest.upper()
        if(dest==''):
            if(name==''):
                name='doc.pdf'
                dest='I'
            else:
                dest='F'
        if dest=='I':
            print self.buffer
        elif dest=='D':
            print self.buffer
        elif dest=='F':
            #Save to local file
            f=open(name,'wb')
            if(not f):
                self.error('Unable to create output file: '+name)
            if PY3K:
                # TODO: proper unicode support
                f.write(self.buffer.encode("latin1"))
            else:
                f.write(self.buffer)
            f.close()
        elif dest=='S':
            #Return as a string
            return self.buffer
        else:
            self.error('Incorrect output destination: '+dest)
        return ''

    def normalize_text(self, txt):
        "Check that text input is in the correct format/encoding"
        # - for TTF unicode fonts: unicode object (utf8 encoding)
        # - for built-in fonts: string instances (latin 1 encoding)
        if self.unifontsubset and isinstance(txt, str):
            txt = txt.decode('utf8')
        elif not self.unifontsubset and isinstance(txt, unicode) and not PY3K:
            txt = txt.encode('latin1')
        return txt


    def _dochecks(self):
        #Check for locale-related bug
#        if(1.1==1):
#            self.error("Don\'t alter the locale before including class file");
        #Check for decimal separator
        if(sprintf('%.1f',1.0)!='1.0'):
            import locale
            locale.setlocale(locale.LC_NUMERIC,'C')

    def _getfontpath(self):
        return FPDF_FONT_DIR+'/'

    def _putpages(self):
        nb=self.page
        if hasattr(self,'str_alias_nb_pages'):
            # Replace number of pages in fonts using subsets (unicode)
            alias = UTF8ToUTF16BE(self.str_alias_nb_pages, False);
            r = UTF8ToUTF16BE(str(nb), False)
            for n in xrange(1, nb+1):
                self.pages[n] = self.pages[n].replace(alias, r)
            # Now repeat for no pages in non-subset fonts
            for n in xrange(1,nb+1):
                self.pages[n]=self.pages[n].replace(self.str_alias_nb_pages,str(nb))
        if(self.def_orientation=='P'):
            w_pt=self.fw_pt
            h_pt=self.fh_pt
        else:
            w_pt=self.fh_pt
            h_pt=self.fw_pt
        if self.compress:
            filter='/Filter /FlateDecode '
        else:
            filter=''
        for n in xrange(1,nb+1):
            #Page
            self._newobj()
            self._out('<</Type /Page')
            self._out('/Parent 1 0 R')
            if n in self.orientation_changes:
                self._out(sprintf('/MediaBox [0 0 %.2f %.2f]',h_pt,w_pt))
            self._out('/Resources 2 0 R')
            if self.page_links and n in self.page_links:
                #Links
                annots='/Annots ['
                for pl in self.page_links[n]:
                    rect=sprintf('%.2f %.2f %.2f %.2f',pl[0],pl[1],pl[0]+pl[2],pl[1]-pl[3])
                    annots+='<</Type /Annot /Subtype /Link /Rect ['+rect+'] /Border [0 0 0] '
                    if(isinstance(pl[4],basestring)):
                        annots+='/A <</S /URI /URI '+self._textstring(pl[4])+'>>>>'
                    else:
                        l=self.links[pl[4]]
                        if l[0] in self.orientation_changes:
                            h=w_pt
                        else:
                            h=h_pt
                        annots+=sprintf('/Dest [%d 0 R /XYZ 0 %.2f null]>>',1+2*l[0],h-l[1]*self.k)
                self._out(annots+']')
            if(self.pdf_version>'1.3'):
                self._out('/Group <</Type /Group /S /Transparency /CS /DeviceRGB>>')
            self._out('/Contents '+str(self.n+1)+' 0 R>>')
            self._out('endobj')
            #Page content
            if self.compress:
                p = zlib.compress(self.pages[n])
            else:
                p = self.pages[n]
            self._newobj()
            self._out('<<'+filter+'/Length '+str(len(p))+'>>')
            self._putstream(p)
            self._out('endobj')
        #Pages root
        self.offsets[1]=len(self.buffer)
        self._out('1 0 obj')
        self._out('<</Type /Pages')
        kids='/Kids ['
        for i in xrange(0,nb):
            kids+=str(3+2*i)+' 0 R '
        self._out(kids+']')
        self._out('/Count '+str(nb))
        self._out(sprintf('/MediaBox [0 0 %.2f %.2f]',w_pt,h_pt))
        self._out('>>')
        self._out('endobj')

    def _putfonts(self):
        nf=self.n
        for diff in self.diffs:
            #Encodings
            self._newobj()
            self._out('<</Type /Encoding /BaseEncoding /WinAnsiEncoding /Differences ['+self.diffs[diff]+']>>')
            self._out('endobj')
        for name,info in self.font_files.iteritems():
            if 'type' in info and info['type'] != 'TTF':
                #Font file embedding
                self._newobj()
                self.font_files[name]['n']=self.n
                font=''
                f=open(self._getfontpath()+name,'rb',1)
                if(not f):
                    self.error('Font file not found')
                font=f.read()
                f.close()
                compressed=(substr(name,-2)=='.z')
                if(not compressed and 'length2' in info):
                    header=(ord(font[0])==128)
                    if(header):
                        #Strip first binary header
                        font=substr(font,6)
                    if(header and ord(font[info['length1']])==128):
                        #Strip second binary header
                        font=substr(font,0,info['length1'])+substr(font,info['length1']+6)
                self._out('<</Length '+str(len(font)))
                if(compressed):
                    self._out('/Filter /FlateDecode')
                self._out('/Length1 '+str(info['length1']))
                if('length2' in info):
                    self._out('/Length2 '+str(info['length2'])+' /Length3 0')
                self._out('>>')
                self._putstream(font)
                self._out('endobj')
        for k,font in self.fonts.iteritems():
            #Font objects
            self.fonts[k]['n']=self.n+1
            type=font['type']
            name=font['name']
            if(type=='core'):
                #Standard font
                self._newobj()
                self._out('<</Type /Font')
                self._out('/BaseFont /'+name)
                self._out('/Subtype /Type1')
                if(name!='Symbol' and name!='ZapfDingbats'):
                    self._out('/Encoding /WinAnsiEncoding')
                self._out('>>')
                self._out('endobj')
            elif(type=='Type1' or type=='TrueType'):
                #Additional Type1 or TrueType font
                self._newobj()
                self._out('<</Type /Font')
                self._out('/BaseFont /'+name)
                self._out('/Subtype /'+type)
                self._out('/FirstChar 32 /LastChar 255')
                self._out('/Widths '+str(self.n+1)+' 0 R')
                self._out('/FontDescriptor '+str(self.n+2)+' 0 R')
                if(font['enc']):
                    if('diff' in font):
                        self._out('/Encoding '+str(nf+font['diff'])+' 0 R')
                    else:
                        self._out('/Encoding /WinAnsiEncoding')
                self._out('>>')
                self._out('endobj')
                #Widths
                self._newobj()
                cw=font['cw']
                s='['
                for i in xrange(32,256):
                    # Get doesn't rise exception; returns 0 instead of None if not set
                    s+=str(cw.get(chr(i)) or 0)+' '
                self._out(s+']')
                self._out('endobj')
                #Descriptor
                self._newobj()
                s='<</Type /FontDescriptor /FontName /'+name
                for k in ('Ascent', 'Descent', 'CapHeight', 'Falgs', 'FontBBox', 'ItalicAngle', 'StemV', 'MissingWidth'):
                    s += ' /%s %s' % (k, font['desc'][k])
                filename=font['file']
                if(filename):
                    s+=' /FontFile'
                    if type!='Type1':
                        s+='2'
                    s+=' '+str(self.font_files[filename]['n'])+' 0 R'
                self._out(s+'>>')
                self._out('endobj')
            elif (type == 'TTF'):
                self.fonts[k]['n'] = self.n + 1
                ttf = TTFontFile()
                fontname = 'MPDFAA' + '+' + font['name']
                subset = font['subset']
                del subset[0]
                ttfontstream = ttf.makeSubset(font['ttffile'], subset)
                ttfontsize = len(ttfontstream)
                fontstream = zlib.compress(ttfontstream)
                codeToGlyph = ttf.codeToGlyph
                ##del codeToGlyph[0]
                # Type0 Font
                # A composite font - a font composed of other fonts, organized hierarchically
                self._newobj()
                self._out('<</Type /Font');
                self._out('/Subtype /Type0');
                self._out('/BaseFont /' + fontname + '');
                self._out('/Encoding /Identity-H');
                self._out('/DescendantFonts [' + str(self.n + 1) + ' 0 R]')
                self._out('/ToUnicode ' + str(self.n + 2) + ' 0 R')
                self._out('>>')
                self._out('endobj')

                # CIDFontType2
                # A CIDFont whose glyph descriptions are based on TrueType font technology
                self._newobj()
                self._out('<</Type /Font')
                self._out('/Subtype /CIDFontType2')
                self._out('/BaseFont /' + fontname + '')
                self._out('/CIDSystemInfo ' + str(self.n + 2) + ' 0 R')
                self._out('/FontDescriptor ' + str(self.n + 3) + ' 0 R')
                if (font['desc'].get('MissingWidth')):
                    self._out('/DW %d' % font['desc']['MissingWidth'])
                self._putTTfontwidths(font, ttf.maxUni)
                self._out('/CIDToGIDMap ' + str(self.n + 4) + ' 0 R')
                self._out('>>')
                self._out('endobj')

                # ToUnicode
                self._newobj()
                toUni = "/CIDInit /ProcSet findresource begin\n" \
                        "12 dict begin\n" \
                        "begincmap\n" \
                        "/CIDSystemInfo\n" \
                        "<</Registry (Adobe)\n" \
                        "/Ordering (UCS)\n" \
                        "/Supplement 0\n" \
                        ">> def\n" \
                        "/CMapName /Adobe-Identity-UCS def\n" \
                        "/CMapType 2 def\n" \
                        "1 begincodespacerange\n" \
                        "<0000> <FFFF>\n" \
                        "endcodespacerange\n" \
                        "1 beginbfrange\n" \
                        "<0000> <FFFF> <0000>\n" \
                        "endbfrange\n" \
                        "endcmap\n" \
                        "CMapName currentdict /CMap defineresource pop\n" \
                        "end\n" \
                        "end"
                self._out('<</Length ' + str(len(toUni)) + '>>')
                self._putstream(toUni)
                self._out('endobj')

                # CIDSystemInfo dictionary
                self._newobj()
                self._out('<</Registry (Adobe)')
                self._out('/Ordering (UCS)')
                self._out('/Supplement 0')
                self._out('>>')
                self._out('endobj')

                # Font descriptor
                self._newobj()
                self._out('<</Type /FontDescriptor')
                self._out('/FontName /' + fontname)
                for kd in ('Ascent', 'Descent', 'CapHeight', 'Flags', 'FontBBox', 'ItalicAngle', 'StemV', 'MissingWidth'):
                    v = font['desc'][kd]
                    if (kd == 'Flags'):
                        v = v | 4;
                        v = v & ~32; # SYMBOLIC font flag
                    self._out(' /%s %s' % (kd, v))
                self._out('/FontFile2 ' + str(self.n + 2) + ' 0 R')
                self._out('>>')
                self._out('endobj')

                # Embed CIDToGIDMap
                # A specification of the mapping from CIDs to glyph indices
                cidtogidmap = '';
                cidtogidmap = ["\x00"] * 256*256*2
                for cc, glyph in codeToGlyph.items():
                    cidtogidmap[cc*2] = chr(glyph >> 8)
                    cidtogidmap[cc*2 + 1] = chr(glyph & 0xFF)
                cidtogidmap = zlib.compress(''.join(cidtogidmap));
                self._newobj()
                self._out('<</Length ' + str(len(cidtogidmap)) + '')
                self._out('/Filter /FlateDecode')
                self._out('>>')
                self._putstream(cidtogidmap)
                self._out('endobj')

                #Font file
                self._newobj()
                self._out('<</Length ' + str(len(fontstream)))
                self._out('/Filter /FlateDecode')
                self._out('/Length1 ' + str(ttfontsize))
                self._out('>>')
                self._putstream(fontstream)
                self._out('endobj')
                del ttf
            else:
                #Allow for additional types
                mtd='_put'+type.lower()
                if(not method_exists(self,mtd)):
                    self.error('Unsupported font type: '+type)
                self.mtd(font)

    def _putTTfontwidths(self, font, maxUni):
        cw127fname = os.path.splitext(font['unifilename'])[0] + '.cw127.pkl'
        if (os.path.exists(cw127fname)):
            fh = open(cw127fname);
            try:
                font_dict = pickle.load(fh)
            finally:
                fh.close()
            rangeid = font_dict['rangeid']
            range_ = font_dict['range']
            prevcid = font_dict['prevcid']
            prevwidth = font_dict['prevwidth']
            interval = font_dict['interval']
            range_interval = font_dict['range_interval']
            startcid = 128
        else:
            rangeid = 0
            range_ = {}
            range_interval = {}
            prevcid = -2
            prevwidth = -1
            interval = False
            startcid = 1
        cwlen = maxUni + 1

        # for each character
        for cid in range(startcid, cwlen):
            if (cid==128 and not os.path.exists(cw127fname)):
                try:
                    fh = open(cw127fname, "wb")
                    font_dict = {}
                    font_dict['rangeid'] = rangeid
                    font_dict['prevcid'] = prevcid
                    font_dict['prevwidth'] = prevwidth
                    font_dict['interval'] = interval
                    font_dict['range_interval'] = range_interval
                    font_dict['range'] = range_
                    pickle.dump(font_dict, fh)
                    fh.close()
                except IOError, e:
                    if not e.errno == errno.EACCES:
                        raise  # Not a permission error.
            if (font['cw'][cid] == 0):
                continue
            width = font['cw'][cid]
            if (width == 65535): width = 0
            if (cid > 255 and (cid not in font['subset']) or not cid): #
                continue
            if ('dw' not in font or (font['dw'] and width != font['dw'])):
                if (cid == (prevcid + 1)):
                    if (width == prevwidth):
                        if (width == range_[rangeid][0]):
                            range_.setdefault(rangeid, []).append(width)
                        else:
                            range_[rangeid].pop()
                            # new range
                            rangeid = prevcid
                            range_[rangeid] = [prevwidth, width]
                        interval = True
                        range_interval[rangeid] = True
                    else:
                        if (interval):
                            # new range
                            rangeid = cid
                            range_[rangeid] = [width]
                        else:
                            range_[rangeid].append(width)
                        interval = False
                else:
                    rangeid = cid
                    range_[rangeid] = [width]
                    interval = False
                prevcid = cid
                prevwidth = width
        prevk = -1
        nextk = -1
        prevint = False
        for k, ws in sorted(range_.items()):
            cws = len(ws)
            if (k == nextk and not prevint and (not k in range_interval or cws < 3)):
                if (k in range_interval):
                    del range_interval[k]
                range_[prevk] = range_[prevk] + range_[k]
                del range_[k]
            else:
                prevk = k
            nextk = k + cws
            if (k in range_interval):
                prevint = (cws > 3)
                del range_interval[k]
                nextk -= 1
            else:
                prevint = False
        w = []
        for k, ws in sorted(range_.items()):
            if (len(set(ws)) == 1):
                w.append(' %s %s %s' % (k, k + len(ws) - 1, ws[0]))
            else:
                w.append(' %s [ %s ]\n' % (k, ' '.join([str(int(h)) for h in ws]))) ##
        self._out('/W [%s]' % ''.join(w))

    def _putimages(self):
        filter=''
        if self.compress:
            filter='/Filter /FlateDecode '
        for filename,info in self.images.iteritems():
            self._putimage(info)
            del info['data']
            if 'smask' in info:
                del info['smask']

    def _putimage(self, info):
        if 'data' in info:
            self._newobj()
            info['n']=self.n
            self._out('<</Type /XObject')
            self._out('/Subtype /Image')
            self._out('/Width '+str(info['w']))
            self._out('/Height '+str(info['h']))
            if(info['cs']=='Indexed'):
                self._out('/ColorSpace [/Indexed /DeviceRGB '+str(len(info['pal'])/3-1)+' '+str(self.n+1)+' 0 R]')
            else:
                self._out('/ColorSpace /'+info['cs'])
                if(info['cs']=='DeviceCMYK'):
                    self._out('/Decode [1 0 1 0 1 0 1 0]')
            self._out('/BitsPerComponent '+str(info['bpc']))
            if 'f' in info:
                self._out('/Filter /'+info['f'])
            if 'dp' in info:
                self._out('/DecodeParms <<' + info['dp'] + '>>')
            if('trns' in info and isinstance(info['trns'], list)):
                trns=''
                for i in xrange(0,len(info['trns'])):
                    trns+=str(info['trns'][i])+' '+str(info['trns'][i])+' '
                self._out('/Mask ['+trns+']')
            if('smask' in info):
                self._out('/SMask ' + str(self.n+1) + ' 0 R');
            self._out('/Length '+str(len(info['data']))+'>>')
            self._putstream(info['data'])
            self._out('endobj')
            # Soft mask
            if('smask' in info):
                dp = '/Predictor 15 /Colors 1 /BitsPerComponent 8 /Columns ' + str(info['w'])
                smask = {'w': info['w'], 'h': info['h'], 'cs': 'DeviceGray', 'bpc': 8, 'f': info['f'], 'dp': dp, 'data': info['smask']}
                self._putimage(smask)
            #Palette
            if(info['cs']=='Indexed'):
                self._newobj()
                filter = self.compress and '/Filter /FlateDecode ' or ''
                if self.compress:
                    pal=zlib.compress(info['pal'])
                else:
                    pal=info['pal']
                self._out('<<'+filter+'/Length '+str(len(pal))+'>>')
                self._putstream(pal)
                self._out('endobj')

    def _putxobjectdict(self):
        for image in self.images.values():
            self._out('/I'+str(image['i'])+' '+str(image['n'])+' 0 R')

    def _putresourcedict(self):
        self._out('/ProcSet [/PDF /Text /ImageB /ImageC /ImageI]')
        self._out('/Font <<')
        for font in self.fonts.values():
            self._out('/F'+str(font['i'])+' '+str(font['n'])+' 0 R')
        self._out('>>')
        self._out('/XObject <<')
        self._putxobjectdict()
        self._out('>>')

    def _putresources(self):
        self._putfonts()
        self._putimages()
        #Resource dictionary
        self.offsets[2]=len(self.buffer)
        self._out('2 0 obj')
        self._out('<<')
        self._putresourcedict()
        self._out('>>')
        self._out('endobj')

    def _putinfo(self):
        self._out('/Producer '+self._textstring('PyFPDF '+FPDF_VERSION+' http://pyfpdf.googlecode.com/'))
        if hasattr(self,'title'):
            self._out('/Title '+self._textstring(self.title))
        if hasattr(self,'subject'):
            self._out('/Subject '+self._textstring(self.subject))
        if hasattr(self,'author'):
            self._out('/Author '+self._textstring(self.author))
        if hasattr (self,'keywords'):
            self._out('/Keywords '+self._textstring(self.keywords))
        if hasattr(self,'creator'):
            self._out('/Creator '+self._textstring(self.creator))
        self._out('/CreationDate '+self._textstring('D:'+datetime.now().strftime('%Y%m%d%H%M%S')))

    def _putcatalog(self):
        self._out('/Type /Catalog')
        self._out('/Pages 1 0 R')
        if(self.zoom_mode=='fullpage'):
            self._out('/OpenAction [3 0 R /Fit]')
        elif(self.zoom_mode=='fullwidth'):
            self._out('/OpenAction [3 0 R /FitH null]')
        elif(self.zoom_mode=='real'):
            self._out('/OpenAction [3 0 R /XYZ null null 1]')
        elif(not isinstance(self.zoom_mode,basestring)):
            self._out('/OpenAction [3 0 R /XYZ null null '+(self.zoom_mode/100)+']')
        if(self.layout_mode=='single'):
            self._out('/PageLayout /SinglePage')
        elif(self.layout_mode=='continuous'):
            self._out('/PageLayout /OneColumn')
        elif(self.layout_mode=='two'):
            self._out('/PageLayout /TwoColumnLeft')

    def _putheader(self):
        self._out('%PDF-'+self.pdf_version)

    def _puttrailer(self):
        self._out('/Size '+str(self.n+1))
        self._out('/Root '+str(self.n)+' 0 R')
        self._out('/Info '+str(self.n-1)+' 0 R')

    def _enddoc(self):
        self._putheader()
        self._putpages()
        self._putresources()
        #Info
        self._newobj()
        self._out('<<')
        self._putinfo()
        self._out('>>')
        self._out('endobj')
        #Catalog
        self._newobj()
        self._out('<<')
        self._putcatalog()
        self._out('>>')
        self._out('endobj')
        #Cross-ref
        o=len(self.buffer)
        self._out('xref')
        self._out('0 '+(str(self.n+1)))
        self._out('0000000000 65535 f ')
        for i in xrange(1,self.n+1):
            self._out(sprintf('%010d 00000 n ',self.offsets[i]))
        #Trailer
        self._out('trailer')
        self._out('<<')
        self._puttrailer()
        self._out('>>')
        self._out('startxref')
        self._out(o)
        self._out('%%EOF')
        self.state=3

    def _beginpage(self, orientation):
        self.page+=1
        self.pages[self.page]=''
        self.state=2
        self.x=self.l_margin
        self.y=self.t_margin
        self.font_family=''
        #Page orientation
        if(not orientation):
            orientation=self.def_orientation
        else:
            orientation=orientation[0].upper()
            if(orientation!=self.def_orientation):
                self.orientation_changes[self.page]=1
        if(orientation!=self.cur_orientation):
            #Change orientation
            if(orientation=='P'):
                self.w_pt=self.fw_pt
                self.h_pt=self.fh_pt
                self.w=self.fw
                self.h=self.fh
            else:
                self.w_pt=self.fh_pt
                self.h_pt=self.fw_pt
                self.w=self.fh
                self.h=self.fw
            self.page_break_trigger=self.h-self.b_margin
            self.cur_orientation=orientation

    def _endpage(self):
        #End of page contents
        self.state=1

    def _newobj(self):
        #Begin a new object
        self.n+=1
        self.offsets[self.n]=len(self.buffer)
        self._out(str(self.n)+' 0 obj')

    def _dounderline(self, x,y,txt):
        #Underline text
        up=self.current_font['up']
        ut=self.current_font['ut']
        w=self.get_string_width(txt)+self.ws*txt.count(' ')
        return sprintf('%.2f %.2f %.2f %.2f re f',x*self.k,(self.h-(y-up/1000.0*self.font_size))*self.k,w*self.k,-ut/1000.0*self.font_size_pt)

    def _parsejpg(self, filename):
        # Extract info from a JPEG file
        if Image is None:
            self.error('PIL not installed')
        try:
            f = open(filename, 'rb')
            im = Image.open(f)
        except Exception, e:
            self.error('Missing or incorrect image file: %s. error: %s' % (filename, str(e)))
        else:
            a = im.size
        # We shouldn't get into here, as Jpeg is RGB=8bpp right(?), but, just in case...
        bpc=8
        if im.mode == 'RGB':
            colspace='DeviceRGB'
        elif im.mode == 'CMYK':
            colspace='DeviceCMYK'
        else:
            colspace='DeviceGray'

        # Read whole file from the start
        f.seek(0)
        data = f.read()
        f.close()
        return {'w':a[0],'h':a[1],'cs':colspace,'bpc':bpc,'f':'DCTDecode','data':data}

    def _parsegif(self, filename):
        # Extract info from a GIF file (via PNG conversion)
        if Image is None:
            self.error('PIL is required for GIF support')
        try:
            im = Image.open(filename)
        except Exception, e:
            self.error('Missing or incorrect image file: %s. error: %s' % (filename, str(e)))
        else:
            # Use temporary file
            f = tempfile.NamedTemporaryFile(delete=False, suffix=".png")
            tmp = f.name
            f.close()
            if "transparency" in im.info:
                im.save(tmp, transparency = im.info['transparency'])
            else:
                im.save(tmp)
            info = self._parsepng(tmp)
            os.unlink(tmp)
        return info

    def _parsepng(self, name):
        #Extract info from a PNG file
        if name.startswith("http://") or name.startswith("https://"):
            import urllib
            f = urllib.urlopen(name)
        else:
            f=open(name,'rb')
        if(not f):
            self.error("Can't open image file: "+name)
        #Check signature
        if(f.read(8)!='\x89'+'PNG'+'\r'+'\n'+'\x1a'+'\n'):
            self.error('Not a PNG file: '+name)
        #Read header chunk
        f.read(4)
        if(f.read(4)!='IHDR'):
            self.error('Incorrect PNG file: '+name)
        w=self._freadint(f)
        h=self._freadint(f)
        bpc=ord(f.read(1))
        if(bpc>8):
            self.error('16-bit depth not supported: '+name)
        ct=ord(f.read(1))
        if(ct==0 or ct==4):
            colspace='DeviceGray'
        elif(ct==2 or ct==6):
            colspace='DeviceRGB'
        elif(ct==3):
            colspace='Indexed'
        else:
            self.error('Unknown color type: '+name)
        if(ord(f.read(1))!=0):
            self.error('Unknown compression method: '+name)
        if(ord(f.read(1))!=0):
            self.error('Unknown filter method: '+name)
        if(ord(f.read(1))!=0):
            self.error('Interlacing not supported: '+name)
        f.read(4)
        dp='/Predictor 15 /Colors '
        if colspace == 'DeviceRGB':
            dp+='3'
        else:
            dp+='1'
        dp+=' /BitsPerComponent '+str(bpc)+' /Columns '+str(w)+''
        #Scan chunks looking for palette, transparency and image data
        pal=''
        trns=''
        data=''
        n=1
        while n != None:
            n=self._freadint(f)
            type=f.read(4)
            if(type=='PLTE'):
                #Read palette
                pal=f.read(n)
                f.read(4)
            elif(type=='tRNS'):
                #Read transparency info
                t=f.read(n)
                if(ct==0):
                    trns=[ord(substr(t,1,1)),]
                elif(ct==2):
                    trns=[ord(substr(t,1,1)),ord(substr(t,3,1)),ord(substr(t,5,1))]
                else:
                    pos=t.find('\x00')
                    if(pos!=-1):
                        trns=[pos,]
                f.read(4)
            elif(type=='IDAT'):
                #Read image data block
                data+=f.read(n)
                f.read(4)
            elif(type=='IEND'):
                break
            else:
                f.read(n+4)
        if(colspace=='Indexed' and not pal):
            self.error('Missing palette in '+name)
        f.close()
        info = {'w':w,'h':h,'cs':colspace,'bpc':bpc,'f':'FlateDecode','dp':dp,'pal':pal,'trns':trns,}
        if(ct>=4):
            # Extract alpha channel
            data = zlib.decompress(data)
            color = '';
            alpha = '';
            if(ct==4):
                # Gray image
                length = 2*w
                for i in range(h):
                    pos = (1+length)*i
                    color += data[pos]
                    alpha += data[pos]
                    line = substr(data, pos+1, length)
                    color += re.sub('(.).',lambda m: m.group(1),line, flags=re.DOTALL)
                    alpha += re.sub('.(.)',lambda m: m.group(1),line, flags=re.DOTALL)
            else:
                # RGB image
                length = 4*w
                for i in range(h):
                    pos = (1+length)*i
                    color += data[pos]
                    alpha += data[pos]
                    line = substr(data, pos+1, length)
                    color += re.sub('(.{3}).',lambda m: m.group(1),line, flags=re.DOTALL)
                    alpha += re.sub('.{3}(.)',lambda m: m.group(1),line, flags=re.DOTALL)
            del data
            data = zlib.compress(color)
            info['smask'] = zlib.compress(alpha)
            if (self.pdf_version < '1.4'):
                self.pdf_version = '1.4'
        info['data'] = data
        return info

    def _freadint(self, f):
        #Read a 4-byte integer from file
        try:
            return struct.unpack('>I', f.read(4))[0]
        except:
            return None

    def _textstring(self, s):
        #Format a text string
        return '('+self._escape(s)+')'

    def _escape(self, s):
        #Add \ before \, ( and )
        return s.replace('\\','\\\\').replace(')','\\)').replace('(','\\(').replace('\r','\\r')

    def _putstream(self, s):
        self._out('stream')
        self._out(s)
        self._out('endstream')

    def _out(self, s):
        #Add a line to the document
        if(self.state==2):
            self.pages[self.page]+=s+"\n"
        else:
            self.buffer+=str(s)+"\n"

    def interleaved2of5(self, txt, x, y, w=1.0, h=10.0):
        "Barcode I2of5 (numeric), adds a 0 if odd lenght"
        narrow = w / 3.0
        wide = w

        # wide/narrow codes for the digits
        bar_char={'0': 'nnwwn', '1': 'wnnnw', '2': 'nwnnw', '3': 'wwnnn',
                  '4': 'nnwnw', '5': 'wnwnn', '6': 'nwwnn', '7': 'nnnww',
                  '8': 'wnnwn', '9': 'nwnwn', 'A': 'nn', 'Z': 'wn'}

        self.set_fill_color(0)
        code = txt
        # add leading zero if code-length is odd
        if len(code) % 2 != 0:
            code = '0' + code

        # add start and stop codes
        code = 'AA' + code.lower() + 'ZA'

        for i in xrange(0, len(code), 2):
            # choose next pair of digits
            char_bar = code[i]
            char_space = code[i+1]
            # check whether it is a valid digit
            if not char_bar in bar_char.keys():
                raise RuntimeError ('Char "%s" invalid for I25: ' % char_bar)
            if not char_space in bar_char.keys():
                raise RuntimeError ('Char "%s" invalid for I25: ' % char_space)

            # create a wide/narrow-seq (first digit=bars, second digit=spaces)
            seq = ''
            for s in xrange(0, len(bar_char[char_bar])):
                seq += bar_char[char_bar][s] + bar_char[char_space][s]

            for bar in xrange(0, len(seq)):
                # set line_width depending on value
                if seq[bar] == 'n':
                    line_width = narrow
                else:
                    line_width = wide

                # draw every second value, the other is represented by space
                if bar % 2 == 0:
                    self.rect(x, y, line_width, h, 'F')

                x += line_width


    def code39(self, txt, x, y, w=1.5, h=5.0):
        "Barcode 3of9"
        wide = w
        narrow = w / 3.0
        gap = narrow

        bar_char={'0': 'nnnwwnwnn', '1': 'wnnwnnnnw', '2': 'nnwwnnnnw',
                  '3': 'wnwwnnnnn', '4': 'nnnwwnnnw', '5': 'wnnwwnnnn',
                  '6': 'nnwwwnnnn', '7': 'nnnwnnwnw', '8': 'wnnwnnwnn',
                  '9': 'nnwwnnwnn', 'A': 'wnnnnwnnw', 'B': 'nnwnnwnnw',
                  'C': 'wnwnnwnnn', 'D': 'nnnnwwnnw', 'E': 'wnnnwwnnn',
                  'F': 'nnwnwwnnn', 'G': 'nnnnnwwnw', 'H': 'wnnnnwwnn',
                  'I': 'nnwnnwwnn', 'J': 'nnnnwwwnn', 'K': 'wnnnnnnww',
                  'L': 'nnwnnnnww', 'M': 'wnwnnnnwn', 'N': 'nnnnwnnww',
                  'O': 'wnnnwnnwn', 'P': 'nnwnwnnwn', 'Q': 'nnnnnnwww',
                  'R': 'wnnnnnwwn', 'S': 'nnwnnnwwn', 'T': 'nnnnwnwwn',
                  'U': 'wwnnnnnnw', 'V': 'nwwnnnnnw', 'W': 'wwwnnnnnn',
                  'X': 'nwnnwnnnw', 'Y': 'wwnnwnnnn', 'Z': 'nwwnwnnnn',
                  '-': 'nwnnnnwnw', '.': 'wwnnnnwnn', ' ': 'nwwnnnwnn',
                  '*': 'nwnnwnwnn', '$': 'nwnwnwnnn', '/': 'nwnwnnnwn',
                  '+': 'nwnnnwnwn', '%': 'nnnwnwnwn'}

        self.set_fill_color(0)
        code = txt

        code = code.upper()
        for i in xrange (0, len(code), 2):
            char_bar = code[i]

            if not char_bar in bar_char.keys():
                raise RuntimeError ('Char "%s" invalid for Code39' % char_bar)

            seq= ''
            for s in xrange(0, len(bar_char[char_bar])):
                seq += bar_char[char_bar][s]

            for bar in xrange(0, len(seq)):
                if seq[bar] == 'n':
                    line_width = narrow
                else:
                    line_width = wide

                if bar % 2 == 0:
                    self.rect(x, y, line_width, h, 'F')
                x += line_width
        x += gap


