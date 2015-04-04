#******************************************************************************
# TTFontFile class                                                             
#                                                                              
# This class is based on The ReportLab Open Source PDF library                 
# written in Python - http://www.reportlab.com/software/opensource/            
# together with ideas from the OpenOffice source code and others.              
#                                                                              
# Version:  1.04                                                               
# Date:     2011-09-18                                                         
# Author:   Ian Back <ianb@bpm1.com>                                           
# License:  LGPL                                                               
# Copyright (c) Ian Back, 2010                                                 
# Ported to Python 2.7 by Mariano Reingart (reingart@gmail.com) on 2012        
# This header must be retained in any redistribution or                        
# modification of the file.                                                    
#                                                                              
#******************************************************************************

from struct import pack, unpack, unpack_from
import re
import warnings
from php import die, substr, str_repeat, str_pad, strlen, count


# Define the value used in the "head" table of a created TTF file
# 0x74727565 "true" for Mac
# 0x00010000 for Windows
# Either seems to work for a font embedded in a PDF file
# when read by Adobe Reader on a Windows PC(!)
_TTF_MAC_HEADER = False


# TrueType Font Glyph operators
GF_WORDS = (1 << 0)
GF_SCALE = (1 << 3)
GF_MORE  = (1 << 5)
GF_XYSCALE  = (1 << 6)
GF_TWOBYTWO = (1 << 7)


def sub32(x, y):
    xlo = x[1]
    xhi = x[0]
    ylo = y[1]
    yhi = y[0]
    if (ylo > xlo):  
        xlo += 1 << 16 
        yhi += 1 
    reslo = xlo-ylo
    if (yhi > xhi):  
        xhi += 1 << 16  
    reshi = xhi-yhi
    reshi = reshi & 0xFFFF
    return (reshi, reslo)

def calcChecksum(data): 
    if (strlen(data) % 4):
        data += str_repeat("\0", (4-(len(data) % 4)))
    hi=0x0000
    lo=0x0000
    for i in range(0, len(data), 4): 
        hi += (ord(data[i])<<8) + ord(data[i+1])
        lo += (ord(data[i+2])<<8) + ord(data[i+3])
        hi += lo >> 16
        lo = lo & 0xFFFF
        hi = hi & 0xFFFF
    return (hi, lo)


class TTFontFile:

    def __init__(self):
        self.maxStrLenRead = 200000    # Maximum size of glyf table to read in as string (otherwise reads each glyph from file)

    def getMetrics(self, file):
        self.filename = file
        self.fh = open(file,'rb')
        self._pos = 0
        self.charWidths = []
        self.glyphPos = {}
        self.charToGlyph = {}
        self.tables = {}
        self.otables = {}
        self.ascent = 0
        self.descent = 0
        self.TTCFonts = {}
        self.version = version = self.read_ulong()
        if (version==0x4F54544F):
            die("Postscript outlines are not supported")
        if (version==0x74746366):
            die("ERROR - TrueType Fonts Collections not supported")
        if (version not in (0x00010000,0x74727565)):
            die("Not a TrueType font: version=" + version)
        self.readTableDirectory()
        self.extractInfo()
        self.fh.close()
    
    def readTableDirectory(self, ):
        self.numTables = self.read_ushort()
        self.searchRange = self.read_ushort()
        self.entrySelector = self.read_ushort()
        self.rangeShift = self.read_ushort()
        self.tables = {}    
        for i in range(self.numTables):
            record = {}
            record['tag'] = self.read_tag()
            record['checksum'] = (self.read_ushort(),self.read_ushort())
            record['offset'] = self.read_ulong()
            record['length'] = self.read_ulong()
            self.tables[record['tag']] = record    

    def get_table_pos(self, tag):
        offset = self.tables[tag]['offset']
        length = self.tables[tag]['length']
        return (offset, length)
    
    def seek(self, pos): 
        self._pos = pos
        self.fh.seek(self._pos)
    
    def skip(self, delta): 
        self._pos = self._pos + delta
        self.fh.seek(self._pos)
    
    def seek_table(self, tag, offset_in_table = 0):
        tpos = self.get_table_pos(tag)
        self._pos = tpos[0] + offset_in_table
        self.fh.seek(self._pos)
        return self._pos

    def read_tag(self):
        self._pos += 4
        return self.fh.read(4)

    def read_short(self): 
        self._pos += 2
        s = self.fh.read(2)
        a = (ord(s[0])<<8) + ord(s[1])
        if (a & (1 << 15) ):
            a = (a - (1 << 16)) 
        return a
    
    def unpack_short(self, s):
        a = (ord(s[0])<<8) + ord(s[1])
        if (a & (1 << 15) ):
            a = (a - (1 << 16))     
        return a
    
    def read_ushort(self):
        self._pos += 2
        s = self.fh.read(2)
        return (ord(s[0])<<8) + ord(s[1])

    def read_ulong(self): 
        self._pos += 4
        s = self.fh.read(4)
        # if large uInt32 as an integer, PHP converts it to -ve
        return (ord(s[0])*16777216) + (ord(s[1])<<16) + (ord(s[2])<<8) + ord(s[3]) #     16777216  = 1<<24

    def get_ushort(self, pos): 
        self.fh.seek(pos)
        s = self.fh.read(2)
        return (ord(s[0])<<8) + ord(s[1])

    def get_ulong(self, pos):
        self.fh.seek(pos)
        s = self.fh.read(4)
        # iF large uInt32 as an integer, PHP converts it to -ve
        return (ord(s[0])*16777216) + (ord(s[1])<<16) + (ord(s[2])<<8) + ord(s[3]) #     16777216  = 1<<24    

    def pack_short(self, val):
        if (val<0):
            val = abs(val)
            val = ~val
            val += 1
        return pack(">H",val) 
    
    def splice(self, stream, offset, value):
        return substr(stream,0,offset) + value + substr(stream,offset+strlen(value))
    
    def _set_ushort(self, stream, offset, value):
        up = pack(">H", value)
        return self.splice(stream, offset, up)    

    def _set_short(self, stream, offset, val):
        if (val<0):
            val = abs(val)
            val = ~val
            val += 1
        up = pack(">H",val) 
        return self.splice(stream, offset, up)

    def get_chunk(self, pos, length): 
        self.fh.seek(pos)
        if (length <1):  return '' 
        return (self.fh.read(length))

    def get_table(self, tag):
        (pos, length) = self.get_table_pos(tag)
        if (length == 0):
            die('Truetype font (' + self.filename + '): error reading table: ' + tag) 
        self.fh.seek(pos)
        return (self.fh.read(length))

    def add(self, tag, data):
        if (tag == 'head') :
            data = self.splice(data, 8, "\0\0\0\0")        
        self.otables[tag] = data

############################################/
############################################/

############################################/

    def extractInfo(self): 
        #################/
        # name - Naming table
        #################/
        self.sFamilyClass = 0
        self.sFamilySubClass = 0

        name_offset = self.seek_table("name")
        format = self.read_ushort()
        if (format != 0):
            die("Unknown name table format " + format)
        numRecords = self.read_ushort()
        string_data_offset = name_offset + self.read_ushort()
        names = {1:'',2:'',3:'',4:'',6:''}
        K = names.keys()
        nameCount = len(names)
        for i in range(numRecords): 
            platformId = self.read_ushort()
            encodingId = self.read_ushort()
            languageId = self.read_ushort()
            nameId = self.read_ushort()
            length = self.read_ushort()
            offset = self.read_ushort()
            if (nameId not in K): continue
            N = ''
            if (platformId == 3 and encodingId == 1 and languageId == 0x409):  # Microsoft, Unicode, US English, PS Name
                opos = self._pos
                self.seek(string_data_offset + offset)
                if (length % 2 != 0):
                    die("PostScript name is UTF-16BE string of odd length")
                length /= 2
                N = ''
                while (length > 0):
                    char = self.read_ushort()
                    N += (chr(char))
                    length -= 1
                self._pos = opos
                self.seek(opos)
            
            elif (platformId == 1 and encodingId == 0 and languageId == 0):  # Macintosh, Roman, English, PS Name
                opos = self._pos
                N = self.get_chunk(string_data_offset + offset, length)
                self._pos = opos
                self.seek(opos)
            
            if (N and names[nameId]==''):
                names[nameId] = N
                nameCount -= 1
                if (nameCount==0): break
            
        
        if (names[6]):
            psName = names[6]
        elif (names[4]):
            psName = re.sub(' ','-',names[4])
        elif (names[1]):
            psName = re.sub(' ','-',names[1])
        else:
            psName = ''
        if (not psName):
            die("Could not find PostScript font name")
        self.name = psName
        if (names[1]):
            self.familyName = names[1]  
        else:  
            self.familyName = psName 
        if (names[2]):
            self.styleName = names[2]
        else:
            self.styleName = 'Regular' 
        if (names[4]):
            self.fullName = names[4]
        else:
            self.fullName = psName 
        if (names[3]):
            self.uniqueFontID = names[3]
        else:
            self.uniqueFontID = psName 
        if (names[6]):
            self.fullName = names[6] 

        #################/
        # head - Font header table
        #################/
        self.seek_table("head")
        self.skip(18) 
        self.unitsPerEm = unitsPerEm = self.read_ushort()
        scale = 1000 / float(unitsPerEm)
        self.skip(16)
        xMin = self.read_short()
        yMin = self.read_short()
        xMax = self.read_short()
        yMax = self.read_short()
        self.bbox = [(xMin*scale), (yMin*scale), (xMax*scale), (yMax*scale)]
        self.skip(3*2)
        indexToLocFormat = self.read_ushort()
        glyphDataFormat = self.read_ushort()
        if (glyphDataFormat != 0):
            die('Unknown glyph data format ' + glyphDataFormat)

        #################/
        # hhea metrics table
        #################/
        # ttf2t1 seems to use this value rather than the one in OS/2 - so put in for compatibility
        if ("hhea" in self.tables):
            self.seek_table("hhea")
            self.skip(4)
            hheaAscender = self.read_short()
            hheaDescender = self.read_short()
            self.ascent = (hheaAscender *scale)
            self.descent = (hheaDescender *scale)
        

        #################/
        # OS/2 - OS/2 and Windows metrics table
        #################/
        if ("OS/2" in self.tables): 
            self.seek_table("OS/2")
            version = self.read_ushort()
            self.skip(2)
            usWeightClass = self.read_ushort()
            self.skip(2)
            fsType = self.read_ushort()
            if (fsType == 0x0002 or (fsType & 0x0300) != 0): 
                die('ERROR - Font file ' + self.filename + ' cannot be embedded due to copyright restrictions.')
                self.restrictedUse = True
            
            self.skip(20)
            sF = self.read_short()
            self.sFamilyClass = (sF >> 8)
            self.sFamilySubClass = (sF & 0xFF)
            self._pos += 10  #PANOSE = 10 byte length
            panose = self.fh.read(10)
            self.skip(26)
            sTypoAscender = self.read_short()
            sTypoDescender = self.read_short()
            if (not self.ascent): 
                self.ascent = (sTypoAscender*scale)
            if (not self.descent): 
                self.descent = (sTypoDescender*scale)
            if (version > 1):
                self.skip(16)
                sCapHeight = self.read_short()
                self.capHeight = (sCapHeight*scale)
            else:
                self.capHeight = self.ascent            
        
        else:
            usWeightClass = 500
            if (not self.ascent): self.ascent = (yMax*scale)
            if (not self.descent): self.descent = (yMin*scale)
            self.capHeight = self.ascent
        
        self.stemV = 50 + int(pow((usWeightClass / 65.0),2))

        #################/
        # post - PostScript table
        #################/
        self.seek_table("post")
        self.skip(4) 
        self.italicAngle = self.read_short() + self.read_ushort() / 65536.0
        self.underlinePosition = self.read_short() * scale
        self.underlineThickness = self.read_short() * scale
        isFixedPitch = self.read_ulong()

        self.flags = 4

        if (self.italicAngle!= 0):
            self.flags = self.flags | 64
        if (usWeightClass >= 600):
            self.flags = self.flags | 262144
        if (isFixedPitch):
            self.flags = self.flags | 1

        #################/
        # hhea - Horizontal header table
        #################/
        self.seek_table("hhea")
        self.skip(32) 
        metricDataFormat = self.read_ushort()
        if (metricDataFormat != 0):
            die('Unknown horizontal metric data format '.metricDataFormat)
        numberOfHMetrics = self.read_ushort()
        if (numberOfHMetrics == 0):
            die('Number of horizontal metrics is 0')

        #################/
        # maxp - Maximum profile table
        #################/
        self.seek_table("maxp")
        self.skip(4)
        numGlyphs = self.read_ushort()

        #################/
        # cmap - Character to glyph index mapping table
        #################/
        cmap_offset = self.seek_table("cmap")
        self.skip(2)
        cmapTableCount = self.read_ushort()
        unicode_cmap_offset = 0
        unicode_cmap_offset12 = 0
        
        for i in range(cmapTableCount):
            platformID = self.read_ushort()
            encodingID = self.read_ushort()
            offset = self.read_ulong()
            save_pos = self._pos
            if platformID == 3 and encodingID == 10:  # Microsoft, UCS-4
                format = self.get_ushort(cmap_offset + offset)
                if (format == 12):
                    if not unicode_cmap_offset12:
                        unicode_cmap_offset12 = cmap_offset + offset
                    break
            if ((platformID == 3 and encodingID == 1) or platformID == 0):  # Microsoft, Unicode
                format = self.get_ushort(cmap_offset + offset)
                if (format == 4):
                    if (not unicode_cmap_offset):
                        unicode_cmap_offset = cmap_offset + offset
                    break
                    
            self.seek(save_pos)
        
        if not unicode_cmap_offset and not unicode_cmap_offset12:
            die('Font (' + self.filename + ') does not have cmap for Unicode (platform 3, encoding 1, format 4, or platform 3, encoding 10, format 12, or platform 0, any encoding, format 4)')

        glyphToChar = {}
        charToGlyph = {}
        if unicode_cmap_offset12:
            self.getCMAP12(unicode_cmap_offset12, glyphToChar, charToGlyph)
        else:    
            self.getCMAP4(unicode_cmap_offset, glyphToChar, charToGlyph)

        #################/
        # hmtx - Horizontal metrics table
        #################/
        self.getHMTX(numberOfHMetrics, numGlyphs, glyphToChar, scale)


############################################/
############################################/

    def makeSubset(self, file, subset):
        self.filename = file
        self.fh = open(file ,'rb')
        self._pos = 0
        self.charWidths = []
        self.glyphPos = {}
        self.charToGlyph = {}
        self.tables = {}
        self.otables = {}
        self.ascent = 0
        self.descent = 0
        self.skip(4)
        self.maxUni = 0
        self.readTableDirectory()

        #################/
        # head - Font header table
        #################/
        self.seek_table("head")
        self.skip(50) 
        indexToLocFormat = self.read_ushort()
        glyphDataFormat = self.read_ushort()

        #################/
        # hhea - Horizontal header table
        #################/
        self.seek_table("hhea")
        self.skip(32) 
        metricDataFormat = self.read_ushort()
        orignHmetrics = numberOfHMetrics = self.read_ushort()

        #################/
        # maxp - Maximum profile table
        #################/
        self.seek_table("maxp")
        self.skip(4)
        numGlyphs = self.read_ushort()

        #################/
        # cmap - Character to glyph index mapping table
        #################/
        cmap_offset = self.seek_table("cmap")
        self.skip(2)
        cmapTableCount = self.read_ushort()
        unicode_cmap_offset = 0
        unicode_cmap_offset12 = 0
        for i in range(cmapTableCount):
            platformID = self.read_ushort()
            encodingID = self.read_ushort()
            offset = self.read_ulong()
            save_pos = self._pos
            if platformID == 3 and encodingID == 10:  # Microsoft, UCS-4
                format = self.get_ushort(cmap_offset + offset)
                if (format == 12):
                    if not unicode_cmap_offset12:
                        unicode_cmap_offset12 = cmap_offset + offset
                    break
            if ((platformID == 3 and encodingID == 1) or platformID == 0):  # Microsoft, Unicode
                format = self.get_ushort(cmap_offset + offset)
                if (format == 4):
                    unicode_cmap_offset = cmap_offset + offset
                    break
                
            self.seek(save_pos )
        
        if not unicode_cmap_offset and not unicode_cmap_offset12:
            die('Font (' + self.filename + ') does not have cmap for Unicode (platform 3, encoding 1, format 4, or platform 3, encoding 10, format 12, or platform 0, any encoding, format 4)')

        glyphToChar = {}
        charToGlyph = {}
        if unicode_cmap_offset12:
            self.getCMAP12(unicode_cmap_offset12, glyphToChar, charToGlyph)
        else:    
            self.getCMAP4(unicode_cmap_offset, glyphToChar, charToGlyph)

        self.charToGlyph = charToGlyph

        #################/
        # hmtx - Horizontal metrics table
        #################/
        scale = 1    # not used
        self.getHMTX(numberOfHMetrics, numGlyphs, glyphToChar, scale)

        #################/
        # loca - Index to location
        #################/
        self.getLOCA(indexToLocFormat, numGlyphs)

        subsetglyphs = [(0, 0)]     # special "sorted dict"!
        subsetCharToGlyph = {}
        for code in subset: 
            if (code in self.charToGlyph):
                if (self.charToGlyph[code], code) not in subsetglyphs:
                    subsetglyphs.append((self.charToGlyph[code], code))   # Old Glyph ID => Unicode
                subsetCharToGlyph[code] = self.charToGlyph[code]    # Unicode to old GlyphID
            self.maxUni = max(self.maxUni, code)
        (start,dummy) = self.get_table_pos('glyf')

        subsetglyphs.sort()
        glyphSet = {}
        n = 0
        fsLastCharIndex = 0    # maximum Unicode index (character code) in this font, according to the cmap subtable for platform ID 3 and platform- specific encoding ID 0 or 1.
        for originalGlyphIdx, uni in subsetglyphs:
            fsLastCharIndex = max(fsLastCharIndex , uni)
            glyphSet[originalGlyphIdx] = n    # old glyphID to new glyphID
            n += 1

        codeToGlyph = {}
        for uni, originalGlyphIdx in sorted(subsetCharToGlyph.items()):
            codeToGlyph[uni] = glyphSet[originalGlyphIdx] 
        
        self.codeToGlyph = codeToGlyph
        
        for originalGlyphIdx, uni in subsetglyphs: 
            nonlocals = {'start': start, 'glyphSet': glyphSet, 
                         'subsetglyphs': subsetglyphs}
            self.getGlyphs(originalGlyphIdx, nonlocals)

        numGlyphs = numberOfHMetrics = len(subsetglyphs)

        #tables copied from the original
        tags = ['name']
        for tag in tags:  
            self.add(tag, self.get_table(tag)) 
        tags = ['cvt ', 'fpgm', 'prep', 'gasp']
        for tag in tags:
            if (tag in self.tables):  
                self.add(tag, self.get_table(tag))        

        # post - PostScript
        opost = self.get_table('post')
        post = "\x00\x03\x00\x00" + substr(opost,4,12) + "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        self.add('post', post)

        # Sort CID2GID map into segments of contiguous codes
        if 0 in codeToGlyph:
            del codeToGlyph[0]
        #unset(codeToGlyph[65535])
        rangeid = 0
        range_ = {}
        prevcid = -2
        prevglidx = -1
        # for each character
        for cid, glidx in sorted(codeToGlyph.items()):
            if (cid == (prevcid + 1) and glidx == (prevglidx + 1)):
                range_[rangeid].append(glidx)
            else:
                # new range
                rangeid = cid
                range_[rangeid] = []
                range_[rangeid].append(glidx)
            prevcid = cid
            prevglidx = glidx

        # cmap - Character to glyph mapping - Format 4 (MS / )
        segCount = len(range_) + 1    # + 1 Last segment has missing character 0xFFFF
        searchRange = 1
        entrySelector = 0
        while (searchRange * 2 <= segCount ):
            searchRange = searchRange * 2
            entrySelector = entrySelector + 1
        
        searchRange = searchRange * 2
        rangeShift = segCount * 2 - searchRange
        length = 16 + (8*segCount ) + (numGlyphs+1)
        cmap = [0, 1,        # Index : version, number of encoding subtables
            3, 1,                # Encoding Subtable : platform (MS=3), encoding (Unicode)
            0, 12,            # Encoding Subtable : offset (hi,lo)
            4, length, 0,         # Format 4 Mapping subtable: format, length, language
            segCount*2,
            searchRange,
            entrySelector,
            rangeShift]

        range_ = sorted(range_.items())
        
        # endCode(s)
        for start, subrange in range_:
            endCode = start + (len(subrange)-1)
            cmap.append(endCode)    # endCode(s)
        
        cmap.append(0xFFFF)    # endCode of last Segment
        cmap.append(0)    # reservedPad

        # startCode(s)
        for start, subrange in range_: 
            cmap.append(start)    # startCode(s)
        
        cmap.append(0xFFFF)    # startCode of last Segment
        # idDelta(s) 
        for start, subrange in range_: 
            idDelta = -(start-subrange[0])
            n += count(subrange)
            cmap.append(idDelta)    # idDelta(s)
        
        cmap.append(1)    # idDelta of last Segment
        # idRangeOffset(s) 
        for subrange in range_: 
            cmap.append(0)    # idRangeOffset[segCount]      Offset in bytes to glyph indexArray, or 0
        
        cmap.append(0)    # idRangeOffset of last Segment
        for subrange, glidx in range_: 
            cmap.extend(glidx)
        
        cmap.append(0)    # Mapping for last character
        cmapstr = ''
        for cm in cmap:
            if cm >= 0:
                cmapstr += pack(">H", cm) 
            else:
                try:
                    cmapstr += pack(">h", cm) 
                except:
                    warnings.warn("cmap value too big/small: %s" % cm)
                    cmapstr += pack(">H", -cm) 
        self.add('cmap', cmapstr)

        # glyf - Glyph data
        (glyfOffset,glyfLength) = self.get_table_pos('glyf')
        if (glyfLength < self.maxStrLenRead):
            glyphData = self.get_table('glyf')

        offsets = []
        glyf = ''
        pos = 0

        hmtxstr = ''
        xMinT = 0
        yMinT = 0
        xMaxT = 0
        yMaxT = 0
        advanceWidthMax = 0
        minLeftSideBearing = 0
        minRightSideBearing = 0
        xMaxExtent = 0
        maxPoints = 0            # points in non-compound glyph
        maxContours = 0            # contours in non-compound glyph
        maxComponentPoints = 0    # points in compound glyph
        maxComponentContours = 0    # contours in compound glyph
        maxComponentElements = 0    # number of glyphs referenced at top level
        maxComponentDepth = 0        # levels of recursion, set to 0 if font has only simple glyphs
        self.glyphdata = {}

        for originalGlyphIdx, uni in subsetglyphs: 
            # hmtx - Horizontal Metrics
            hm = self.getHMetric(orignHmetrics, originalGlyphIdx)    
            hmtxstr += hm

            offsets.append(pos)
            try:
                glyphPos = self.glyphPos[originalGlyphIdx]
                glyphLen = self.glyphPos[originalGlyphIdx + 1] - glyphPos
            except IndexError:
                warnings.warn("missing glyph %s" % (originalGlyphIdx))
                glyphLen = 0

            if (glyfLength < self.maxStrLenRead):
                data = substr(glyphData,glyphPos,glyphLen)
            else:
                if (glyphLen > 0):
                    data = self.get_chunk(glyfOffset+glyphPos,glyphLen)
                else:
                    data = ''
            
            if (glyphLen > 0):
                up = unpack(">H", substr(data,0,2))[0]
            if (glyphLen > 2 and (up & (1 << 15)) ):     # If number of contours <= -1 i.e. composiste glyph
                pos_in_glyph = 10
                flags = GF_MORE
                nComponentElements = 0
                while (flags & GF_MORE):
                    nComponentElements += 1    # number of glyphs referenced at top level
                    up = unpack(">H", substr(data,pos_in_glyph,2))
                    flags = up[0]
                    up = unpack(">H", substr(data,pos_in_glyph+2,2))
                    glyphIdx = up[0]
                    self.glyphdata.setdefault(originalGlyphIdx, {}).setdefault('compGlyphs', []).append(glyphIdx)
                    try:
                        data = self._set_ushort(data, pos_in_glyph + 2, glyphSet[glyphIdx])
                    except KeyError:
                        data = 0
                        warnings.warn("missing glyph data %s" % glyphIdx)
                    pos_in_glyph += 4
                    if (flags & GF_WORDS): 
                        pos_in_glyph += 4 
                    else: 
                        pos_in_glyph += 2 
                    if (flags & GF_SCALE):
                        pos_in_glyph += 2 
                    elif (flags & GF_XYSCALE):
                        pos_in_glyph += 4 
                    elif (flags & GF_TWOBYTWO):
                        pos_in_glyph += 8 
                
                maxComponentElements = max(maxComponentElements, nComponentElements)
            
            glyf += data
            pos += glyphLen
            if (pos % 4 != 0): 
                padding = 4 - (pos % 4)
                glyf += str_repeat("\0",padding)
                pos += padding

        offsets.append(pos)
        self.add('glyf', glyf)

        # hmtx - Horizontal Metrics
        self.add('hmtx', hmtxstr)

        # loca - Index to location
        locastr = ''
        if (((pos + 1) >> 1) > 0xFFFF): 
            indexToLocFormat = 1        # long format
            for offset in offsets:
                locastr += pack(">L",offset) 
        else:
            indexToLocFormat = 0        # short format
            for offset in offsets:  
                locastr += pack(">H",(offset/2)) 
        
        self.add('loca', locastr)

        # head - Font header
        head = self.get_table('head')
        head = self._set_ushort(head, 50, indexToLocFormat)
        self.add('head', head)

        # hhea - Horizontal Header
        hhea = self.get_table('hhea')
        hhea = self._set_ushort(hhea, 34, numberOfHMetrics)
        self.add('hhea', hhea)

        # maxp - Maximum Profile
        maxp = self.get_table('maxp')
        maxp = self._set_ushort(maxp, 4, numGlyphs)
        self.add('maxp', maxp)

        # OS/2 - OS/2
        os2 = self.get_table('OS/2')
        self.add('OS/2', os2 )

        self.fh.close()

        # Put the TTF file together
        stm = self.endTTFile('')
        return stm 
    

    #########################################
    # Recursively get composite glyph data
    def getGlyphData(self, originalGlyphIdx, nonlocals):
        # &maxdepth, &depth, &points, &contours
        nonlocals['depth'] += 1
        nonlocals['maxdepth'] = max(nonlocals['maxdepth'], nonlocals['depth'])
        if (len(self.glyphdata[originalGlyphIdx]['compGlyphs'])):
            for glyphIdx in self.glyphdata[originalGlyphIdx]['compGlyphs']: 
                self.getGlyphData(glyphIdx, nonlocals)            
        
        elif ((self.glyphdata[originalGlyphIdx]['nContours'] > 0) and nonlocals['depth'] > 0):     # simple
            contours += self.glyphdata[originalGlyphIdx]['nContours']
            points += self.glyphdata[originalGlyphIdx]['nPoints']
        
        nonlocals['depth'] -= 1


    #########################################
    # Recursively get composite glyphs
    def getGlyphs(self, originalGlyphIdx, nonlocals):
        # &start, &glyphSet, &subsetglyphs) 
        
        try:
            glyphPos = self.glyphPos[originalGlyphIdx]
            glyphLen = self.glyphPos[originalGlyphIdx + 1] - glyphPos
        except IndexError:
            warnings.warn("missing glyph %s" % (originalGlyphIdx))
            return

        if (not glyphLen):  
            return
        
        self.seek(nonlocals['start'] + glyphPos)
        numberOfContours = self.read_short()
        if (numberOfContours < 0):
            self.skip(8)
            flags = GF_MORE
            while (flags & GF_MORE): 
                flags = self.read_ushort()
                glyphIdx = self.read_ushort()
                if (glyphIdx not in nonlocals['glyphSet']):
                    nonlocals['glyphSet'][glyphIdx] = len(nonlocals['subsetglyphs'])    # old glyphID to new glyphID
                    nonlocals['subsetglyphs'].append((glyphIdx, 1))
                
                savepos = self.fh.tell()
                self.getGlyphs(glyphIdx, nonlocals)
                self.seek(savepos)
                if (flags & GF_WORDS):
                    self.skip(4)
                else:
                    self.skip(2)
                if (flags & GF_SCALE):
                    self.skip(2)
                elif (flags & GF_XYSCALE):
                    self.skip(4)
                elif (flags & GF_TWOBYTWO):
                    self.skip(8)

    #########################################

    def getHMTX(self, numberOfHMetrics, numGlyphs, glyphToChar, scale):
        start = self.seek_table("hmtx")
        aw = 0
        self.charWidths = [0] * 256*256*2
        nCharWidths = 0
        if ((numberOfHMetrics*4) < self.maxStrLenRead): 
            data = self.get_chunk(start,(numberOfHMetrics*4))
            arr = unpack(">" + "H" * (len(data)/2), data)
        else:
            self.seek(start) 
        for glyph in range(numberOfHMetrics): 
            if ((numberOfHMetrics*4) < self.maxStrLenRead):
                aw = arr[(glyph*2)] # PHP starts arrays from index 0!? +1
            else:
                aw = self.read_ushort()
                lsb = self.read_ushort()
            
            if (glyph in glyphToChar or glyph == 0):
                if (aw >= (1 << 15) ):
                    aw = 0     # 1.03 Some (arabic) fonts have -ve values for width
                    # although should be unsigned value - comes out as e.g. 65108 (intended -50)
                if (glyph == 0): 
                    self.defaultWidth = scale*aw
                    continue
                
                for char in glyphToChar[glyph]: 
                    if (char != 0 and char != 65535): 
                        w = int(round(scale*aw))
                        if (w == 0):  w = 65535 
                        if (char < 196608): 
                            self.charWidths[char] = w 
                            nCharWidths += 1
            
        
        data = self.get_chunk((start+numberOfHMetrics*4),(numGlyphs*2))
        arr = unpack(">" + "H" * (len(data)/2), data)
        diff = numGlyphs-numberOfHMetrics
        for pos in range(diff): 
            glyph = pos + numberOfHMetrics
            if (glyph in glyphToChar): 
                for char in glyphToChar[glyph]: 
                    if (char != 0 and char != 65535): 
                        w = int(round(scale*aw))
                        if (w == 0):  w = 65535 
                        if (char < 196608):
                            self.charWidths[char] = w
                            nCharWidths += 1 
                        
        
        # NB 65535 is a set width of 0
        # First bytes define number of chars in font
        self.charWidths[0] = nCharWidths 
    

    def getHMetric(self, numberOfHMetrics, gid): 
        start = self.seek_table("hmtx")
        if (gid < numberOfHMetrics):
            self.seek(start+(gid*4))
            hm = self.fh.read(4)
        else:
            self.seek(start+((numberOfHMetrics-1)*4))
            hm = self.fh.read(2)
            self.seek(start+(numberOfHMetrics*2)+(gid*2))
            hm += self.fh.read(2)
        return hm
    

    def getLOCA(self, indexToLocFormat, numGlyphs): 
        start = self.seek_table('loca')
        self.glyphPos = []
        if (indexToLocFormat == 0):
            data = self.get_chunk(start,(numGlyphs*2)+2)
            arr = unpack(">" + "H" * (len(data)/2), data)
            for n in range(numGlyphs): 
                self.glyphPos.append((arr[n] * 2))  # n+1 !?
        elif (indexToLocFormat == 1):
            data = self.get_chunk(start,(numGlyphs*4)+4)
            arr = unpack(">" + "L" * (len(data)/4), data)
            for n in range(numGlyphs):
                self.glyphPos.append((arr[n]))  # n+1 !?
        else:
            die('Unknown location table format ' + indexToLocFormat)

    # CMAP Format 4
    def getCMAP4(self, unicode_cmap_offset, glyphToChar, charToGlyph):
        self.maxUniChar = 0
        self.seek(unicode_cmap_offset + 2)
        length = self.read_ushort()
        limit = unicode_cmap_offset + length
        self.skip(2)

        segCount = self.read_ushort() / 2
        self.skip(6)
        endCount = []
        for i in range(segCount):
            endCount.append(self.read_ushort())
        self.skip(2)
        startCount = []
        for i in range(segCount):
            startCount.append(self.read_ushort()) 
        idDelta = []
        for i in range(segCount):
            idDelta.append(self.read_short())         # ???? was unsigned short
        idRangeOffset_start = self._pos
        idRangeOffset = []
        for i in range(segCount):
            idRangeOffset.append(self.read_ushort()) 

        for n in range(segCount): 
            endpoint = (endCount[n] + 1)
            for unichar in range(startCount[n], endpoint, 1): 
                if (idRangeOffset[n] == 0):
                    glyph = (unichar + idDelta[n]) & 0xFFFF
                else:
                    offset = (unichar - startCount[n]) * 2 + idRangeOffset[n]
                    offset = idRangeOffset_start + 2 * n + offset
                    if (offset >= limit):
                        glyph = 0
                    else:
                        glyph = self.get_ushort(offset)
                        if (glyph != 0):
                           glyph = (glyph + idDelta[n]) & 0xFFFF
                    
                charToGlyph[unichar] = glyph
                if (unichar < 196608):
                    self.maxUniChar = max(unichar,self.maxUniChar) 
                glyphToChar.setdefault(glyph, []).append(unichar)

    # CMAP Format 12
    def getCMAP12(self, unicode_cmap_offset, glyphToChar, charToGlyph):
        self.maxUniChar = 0
        # table (skip format version, should be 12)
        self.seek(unicode_cmap_offset + 2)
        # reserved
        self.skip(2)
        # table length
        length = self.read_ulong()
        # language (should be 0)
        self.skip(4)
        # groups count
        grpCount = self.read_ulong()

        if 2 + 2 + 4 + 4 + 4 + grpCount * 3 * 4 > length:
            die("TTF format 12 cmap table too small")  
        for n in range(grpCount):
            startCharCode = self.read_ulong()
            endCharCode = self.read_ulong()
            glyph = self.read_ulong()
            for unichar in range(startCharCode, endCharCode + 1):
                charToGlyph[unichar] = glyph
                if (unichar < 196608):
                    self.maxUniChar = max(unichar, self.maxUniChar) 
                glyphToChar.setdefault(glyph, []).append(unichar)
                glyph += 1
            
            

    # Put the TTF file together
    def endTTFile(self, stm): 
        stm = ''
        numTables = count(self.otables)
        searchRange = 1
        entrySelector = 0
        while (searchRange * 2 <= numTables): 
            searchRange = searchRange * 2
            entrySelector = entrySelector + 1
        
        searchRange = searchRange * 16
        rangeShift = numTables * 16 - searchRange

        # Header
        if (_TTF_MAC_HEADER): 
            stm += (pack(">LHHHH", 0x74727565, numTables, searchRange, entrySelector, rangeShift))    # Mac
        else:
            stm += (pack(">LHHHH", 0x00010000 , numTables, searchRange, entrySelector, rangeShift))    # Windows

        
        # Table directory
        tables = self.otables

        offset = 12 + numTables * 16
        sorted_tables = sorted(tables.items())
        for tag, data in sorted_tables:
            if (tag == 'head'):
                head_start = offset 
            stm += tag
            checksum = calcChecksum(data)
            stm += pack(">HH", checksum[0],checksum[1])
            stm += pack(">LL", offset, strlen(data))
            paddedLength = (strlen(data)+3)&~3
            offset = offset + paddedLength

        # Table data
        for tag, data in sorted_tables: 
            data += "\0\0\0"
            stm += substr(data,0,(strlen(data)&~3))

        checksum = calcChecksum(stm)
        checksum = sub32((0xB1B0,0xAFBA), checksum)
        chk = pack(">HH", checksum[0],checksum[1])
        stm = self.splice(stm,(head_start + 8),chk)
        return stm 
    
if __name__ == '__main__':
    ttf = TTFontFile()
    ttffile = 'DejaVuSansCondensed.ttf';
    ttf.getMetrics(ttffile)
    # test basic metrics:
    assert round(ttf.descent, 0) == -236
    assert round(ttf.capHeight, 0) == 928
    assert ttf.flags == 4
    assert [round(i, 0) for i in ttf.bbox] == [-918, -415, 1513, 1167]
    assert ttf.italicAngle == 0
    assert ttf.stemV == 87
    assert round(ttf.defaultWidth, 0) == 540
    assert round(ttf.underlinePosition, 0) == -63
    assert round(ttf.underlineThickness, 0) == 44
    # test char widths 8(against binary file generated by tfpdf.php):
    assert ''.join(ttf.charWidths) == open("dejavusanscondensed.cw.dat").read()
    
