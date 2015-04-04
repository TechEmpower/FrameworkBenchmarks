from    types       import  IntType, FloatType, LongType, StringTypes
from    copy        import  deepcopy
from    binascii    import  hexlify

from    Constants   import  *
from    Styles      import  *

class UnhandledParamError( Exception ) :
    def __init__( self, param ) :
        Exception.__init__( self, "Don't know what to do with param %s" % param )

#                                               red green blue
StandardColours = Colours()
StandardColours.append( Colour( 'Black',         0,    0,   0 ) )
StandardColours.append( Colour( 'Blue',          0,    0, 255 ) )
StandardColours.append( Colour( 'Turquoise',     0,  255, 255 ) )
StandardColours.append( Colour( 'Green',         0,  255,   0 ) )
StandardColours.append( Colour( 'Pink',        255,    0, 255 ) )
StandardColours.append( Colour( 'Red',         255,    0,   0 ) )
StandardColours.append( Colour( 'Yellow',      255,  255,   0 ) )
StandardColours.append( Colour( 'White',       255,  255, 255 ) )
StandardColours.append( Colour( 'Blue Dark',     0,    0, 128 ) )
StandardColours.append( Colour( 'Teal',          0,  128, 128 ) )
StandardColours.append( Colour( 'Green Dark',    0,  128,   0 ) )
StandardColours.append( Colour( 'Violet',      128,    0, 128 ) )
StandardColours.append( Colour( 'Red Dark',    128,    0,   0 ) )
StandardColours.append( Colour( 'Yellow Dark', 128,  128,   0 ) )
StandardColours.append( Colour( 'Grey Dark',   128,  128, 128 ) )
StandardColours.append( Colour( 'Grey',        192,  192, 192 ) )

StandardFonts = Fonts()
StandardFonts.append( Font( 'Arial'                   , 'swiss' , 0, 2, '020b0604020202020204' ) )
StandardFonts.append( Font( 'Arial Black'             , 'swiss' , 0, 2, '020b0a04020102020204' ) )
StandardFonts.append( Font( 'Arial Narrow'            , 'swiss' , 0, 2, '020b0506020202030204' ) )
StandardFonts.append( Font( 'Bitstream Vera Sans Mono', 'modern', 0, 1, '020b0609030804020204' ) )
StandardFonts.append( Font( 'Bitstream Vera Sans'     , 'swiss' , 0, 2, '020b0603030804020204' ) )
StandardFonts.append( Font( 'Bitstream Vera Serif'    , 'roman' , 0, 2, '02060603050605020204' ) )
StandardFonts.append( Font( 'Book Antiqua'            , 'roman' , 0, 2, '02040602050305030304' ) )
StandardFonts.append( Font( 'Bookman Old Style'       , 'roman' , 0, 2, '02050604050505020204' ) )
StandardFonts.append( Font( 'Castellar'               , 'roman' , 0, 2, '020a0402060406010301' ) )
StandardFonts.append( Font( 'Century Gothic'          , 'swiss' , 0, 2, '020b0502020202020204' ) )
StandardFonts.append( Font( 'Comic Sans MS'           , 'script', 0, 2, '030f0702030302020204' ) )
StandardFonts.append( Font( 'Courier New'             , 'modern', 0, 1, '02070309020205020404' ) )
StandardFonts.append( Font( 'Franklin Gothic Medium'  , 'swiss' , 0, 2, '020b0603020102020204' ) )
StandardFonts.append( Font( 'Garamond'                , 'roman' , 0, 2, '02020404030301010803' ) )
StandardFonts.append( Font( 'Georgia'                 , 'roman' , 0, 2, '02040502050405020303' ) )
StandardFonts.append( Font( 'Haettenschweiler'        , 'swiss' , 0, 2, '020b0706040902060204' ) )
StandardFonts.append( Font( 'Impact'                  , 'swiss' , 0, 2, '020b0806030902050204' ) )
StandardFonts.append( Font( 'Lucida Console'          , 'modern', 0, 1, '020b0609040504020204' ) )
StandardFonts.append( Font( 'Lucida Sans Unicode'     , 'swiss' , 0, 2, '020b0602030504020204' ) )
StandardFonts.append( Font( 'Microsoft Sans Serif'    , 'swiss' , 0, 2, '020b0604020202020204' ) )
StandardFonts.append( Font( 'Monotype Corsiva'        , 'script', 0, 2, '03010101010201010101' ) )
StandardFonts.append( Font( 'Palatino Linotype'       , 'roman' , 0, 2, '02040502050505030304' ) )
StandardFonts.append( Font( 'Papyrus'                 , 'script', 0, 2, '03070502060502030205' ) )
StandardFonts.append( Font( 'Sylfaen'                 , 'roman' , 0, 2, '010a0502050306030303' ) )
StandardFonts.append( Font( 'Symbol'                  , 'roman' , 2, 2, '05050102010706020507' ) )
StandardFonts.append( Font( 'Tahoma'                  , 'swiss' , 0, 2, '020b0604030504040204' ) )
StandardFonts.append( Font( 'Times New Roman'         , 'roman' , 0, 2, '02020603050405020304' ) )
StandardFonts.append( Font( 'Trebuchet MS'            , 'swiss' , 0, 2, '020b0603020202020204' ) )
StandardFonts.append( Font( 'Verdana'                 , 'swiss' , 0, 2, '020b0604030504040204' ) )

StandardFonts.Castellar.SetAlternate( StandardFonts.Georgia )

"""
Found the following definition at http://www.pbdr.com/vbtips/gen/convtwip.htm

Twips are screen-independent units used to ensure that the placement and
proportion of screen elements in your screen application are the same on all
display systems. A twip is a unit of screen measurement equal to 1/20 of a
printer's point. The conversion between twips and
inches/centimeters/millimeters is as follows:

There are approximately 1440 twips to a inch (the length of a screen item
measuring one inch when printed).

As there are 2.54 centimeters to 1 inch, then there are approximately 567
twips to a centimeter (the length of a screen item measuring one centimeter
when printed).

Or in millimeters, as there are 25.4 millimeters to 1 inch, therefore there
are approximately 56.7 twips to a millimeter (the length of a screen item
measuring one millimeter when printed)."""

# Width default is 12240, Height default is 15840
StandardPaper = Papers()
StandardPaper.append( Paper( 'LETTER'             ,  1,  'Letter 8 1/2 x 11 in'               ,   12240,  15840 ) )
StandardPaper.append( Paper( 'LETTERSMALL'        ,  2,  'Letter Small 8 1/2 x 11 in'         ,   12240,  15840 ) )
StandardPaper.append( Paper( 'TABLOID'            ,  3,  'Tabloid 11 x 17 in'                 ,   15840,  24480 ) )
StandardPaper.append( Paper( 'LEDGER'             ,  4,  'Ledger 17 x 11 in'                  ,   24480,  15840 ) )
StandardPaper.append( Paper( 'LEGAL'              ,  5,  'Legal 8 1/2 x 14 in'                ,   12240,  20160 ) )
StandardPaper.append( Paper( 'STATEMENT'          ,  6,  'Statement 5 1/2 x 8 1/2 in'         ,    7920,  12240 ) )
StandardPaper.append( Paper( 'EXECUTIVE'          ,  7,  'Executive 7 1/4 x 10 1/2 in'        ,   10440,  15120 ) )
StandardPaper.append( Paper( 'A3'                 ,  8,  'A3 297 x 420 mm'                    ,   16838,  23811 ) )
StandardPaper.append( Paper( 'A4'                 ,  9,  'A4 210 x 297 mm'                    ,   11907,  16838 ) )
StandardPaper.append( Paper( 'A4SMALL'            , 10,  'A4 Small 210 x 297 mm'              ,   11907,  16838 ) )
StandardPaper.append( Paper( 'A5'                 , 11,  'A5 148 x 210 mm'                    ,    8391,  11907 ) )
StandardPaper.append( Paper( 'B4'                 , 12,  'B4 (JIS) 250 x 354'                 ,   14175,  20072 ) )
StandardPaper.append( Paper( 'B5'                 , 13,  'B5 (JIS) 182 x 257 mm'              ,   10319,  14572 ) )
StandardPaper.append( Paper( 'FOLIO'              , 14,  'Folio 8 1/2 x 13 in'                ,   12240,  18720 ) )
StandardPaper.append( Paper( 'QUARTO'             , 15,  'Quarto 215 x 275 mm'                ,   12191,  15593 ) )
StandardPaper.append( Paper( '10X14'              , 16,  '10x14 in'                           ,   14400,  20160 ) )
StandardPaper.append( Paper( '11X17'              , 17,  '11x17 in'                           ,   15840,  24480 ) )
StandardPaper.append( Paper( 'NOTE'               , 18,  'Note 8 1/2 x 11 in'                 ,   12240,  15840 ) )
StandardPaper.append( Paper( 'ENV_9'              , 19,  'Envelope #9 3 7/8 x 8 7/8'          ,    5580,  12780 ) )
StandardPaper.append( Paper( 'ENV_10'             , 20,  'Envelope #10 4 1/8 x 9 1/2'         ,    5940,  13680 ) )
StandardPaper.append( Paper( 'ENV_11'             , 21,  'Envelope #11 4 1/2 x 10 3/8'        ,    6480,  14940 ) )
StandardPaper.append( Paper( 'ENV_12'             , 22,  'Envelope #12 4 3/4 x 11'            ,    6840,  15840 ) )
StandardPaper.append( Paper( 'ENV_14'             , 23,  'Envelope #14 5 x 11 1/2'            ,    7200,  16560 ) )
StandardPaper.append( Paper( 'CSHEET'             , 24,  'C size sheet 18 x 24 in'            ,   29520,  34560 ) )
StandardPaper.append( Paper( 'DSHEET'             , 25,  'D size sheet 22 x 34 in'            ,   31680,  48960 ) )
StandardPaper.append( Paper( 'ESHEET'             , 26,  'E size sheet 34 x 44 in'            ,   48960,  63360 ) )
StandardPaper.append( Paper( 'ENV_DL'             , 27,  'Envelope DL 110 x 220mm'            ,    6237,  12474 ) )
StandardPaper.append( Paper( 'ENV_C5'             , 28,  'Envelope C5 162 x 229 mm'           ,    9185,  12984 ) )
StandardPaper.append( Paper( 'ENV_C3'             , 29,  'Envelope C3  324 x 458 mm'          ,   18371,  25969 ) )
StandardPaper.append( Paper( 'ENV_C4'             , 30,  'Envelope C4  229 x 324 mm'          ,   12984,  18371 ) )
StandardPaper.append( Paper( 'ENV_C6'             , 31,  'Envelope C6  114 x 162 mm'          ,    6464,   9185 ) )
StandardPaper.append( Paper( 'ENV_C65'            , 32,  'Envelope C65 114 x 229 mm'          ,    6464,  12984 ) )
StandardPaper.append( Paper( 'ENV_B4'             , 33,  'Envelope B4  250 x 353 mm'          ,   14175,  20015 ) )
StandardPaper.append( Paper( 'ENV_B5'             , 34,  'Envelope B5  176 x 250 mm'          ,    9979,  14175 ) )
StandardPaper.append( Paper( 'ENV_B6'             , 35,  'Envelope B6  176 x 125 mm'          ,    9979,   7088 ) )
StandardPaper.append( Paper( 'ENV_ITALY'          , 36,  'Envelope 110 x 230 mm'              ,    6237,  13041 ) )
StandardPaper.append( Paper( 'ENV_MONARCH'        , 37,  'Envelope Monarch 3.875 x 7.5 in'    ,    5580,  10800 ) )
StandardPaper.append( Paper( 'ENV_PERSONAL'       , 38,  '6 3/4 Envelope 3 5/8 x 6 1/2 in'    ,    5220,   9360 ) )
StandardPaper.append( Paper( 'FANFOLD_US'         , 39,  'US Std Fanfold 14 7/8 x 11 in'      ,   21420,  15840 ) )
StandardPaper.append( Paper( 'FANFOLD_STD_GERMAN' , 40,  'German Std Fanfold 8 1/2 x 12 in'   ,   12240,  17280 ) )
StandardPaper.append( Paper( 'FANFOLD_LGL_GERMAN' , 41,  'German Legal Fanfold 8 1/2 x 13 in' ,   12240,  18720 ) )

#
#   Finally a StyleSheet in which all of this stuff is put together
#
class StyleSheet :
    def __init__( self, colours=None, fonts=None ) :

        self.Colours = colours or deepcopy( StandardColours )
        self.Fonts   = fonts   or deepcopy( StandardFonts   )

        self.TextStyles      = AttributedList()
        self.ParagraphStyles = AttributedList()

class Section( list ) :
    NONE   = 1
    COLUMN = 2
    PAGE   = 3
    EVEN   = 4
    ODD    = 5
    BREAK_TYPES = [ NONE, COLUMN, PAGE, EVEN, ODD ]

    def __init__( self, paper=None, margins=None, break_type=None, headery=None, footery=None, landscape=None, first_page_number=None ) :
        super( Section, self ).__init__()

        self.Paper   = paper   or StandardPaper.A4
        self.SetMargins( margins )

        self.Header = []
        self.Footer = []
        self.FirstHeader = []
        self.FirstFooter = []

        self.SetBreakType( break_type or self.NONE )
        self.SetHeaderY( headery )
        self.SetFooterY( footery )
        self.SetLandscape( landscape )
        self.SetFirstPageNumber( first_page_number )

    def TwipsToRightMargin( self ) :
        return self.Paper.Width - ( self.Margins.Left + self.Margins.Right )

    def SetMargins( self, value ) :
        self.Margins = value or MarginsPropertySet( top=1000, left=1200, bottom=1000, right=1200 )
        self.Width   = self.Paper.Width - ( self.Margins.Left + self.Margins.Right )

    def SetBreakType( self, value ) :
        assert value in self.BREAK_TYPES
        self.BreakType = value
        return self

    def SetHeaderY( self, value ) :
        self.HeaderY = value
        return self

    def SetFooterY( self, value ) :
        self.FooterY = value
        return self

    def SetLandscape( self, value ) :
        self.Landscape = False
        if value : self.Landscape = True
        return self

    def SetFirstPageNumber( self, value ) :
        self.FirstPageNumber = value
        return self

def MakeDefaultStyleSheet( ) :
    result = StyleSheet()

    NormalText = TextStyle( TextPropertySet( result.Fonts.Arial, 22 ) )

    ps = ParagraphStyle( 'Normal',
                         NormalText.Copy(),
                         ParagraphPropertySet( space_before = 60,
                                               space_after  = 60 ) )
    result.ParagraphStyles.append( ps )

    ps = ParagraphStyle( 'Normal Short',
                         NormalText.Copy() )
    result.ParagraphStyles.append( ps )

    NormalText.TextPropertySet.SetSize( 32 )
    ps = ParagraphStyle( 'Heading 1',
                         NormalText.Copy(),
                         ParagraphPropertySet( space_before = 240,
                                               space_after  = 60 ) )
    result.ParagraphStyles.append( ps )

    NormalText.TextPropertySet.SetSize( 24 ).SetBold( True )
    ps = ParagraphStyle( 'Heading 2',
                         NormalText.Copy(),
                         ParagraphPropertySet( space_before = 240,
                                               space_after  = 60 ) )
    result.ParagraphStyles.append( ps )

    #   Add some more in that are based on the normal template but that
    #   have some indenting set that makes them suitable for doing numbered
    normal_numbered = result.ParagraphStyles.Normal.Copy()
    normal_numbered.SetName( 'Normal Numbered' )
    normal_numbered.ParagraphPropertySet.SetFirstLineIndent( TabPropertySet.DEFAULT_WIDTH * -1 )
    normal_numbered.ParagraphPropertySet.SetLeftIndent     ( TabPropertySet.DEFAULT_WIDTH )

    result.ParagraphStyles.append( normal_numbered )

    normal_numbered2 = result.ParagraphStyles.Normal.Copy()
    normal_numbered2.SetName( 'Normal Numbered 2' )
    normal_numbered2.ParagraphPropertySet.SetFirstLineIndent( TabPropertySet.DEFAULT_WIDTH * -1 )
    normal_numbered2.ParagraphPropertySet.SetLeftIndent     ( TabPropertySet.DEFAULT_WIDTH *  2 )

    result.ParagraphStyles.append( normal_numbered2 )

    ## LIST STYLES
    for idx, indent in [ (1, TabPS.DEFAULT_WIDTH    ),
                         (2, TabPS.DEFAULT_WIDTH * 2),
                         (3, TabPS.DEFAULT_WIDTH * 3) ] :
        indent = TabPropertySet.DEFAULT_WIDTH
        ps = ParagraphStyle( 'List %s' % idx,
                             TextStyle( TextPropertySet( result.Fonts.Arial, 22 ) ),
                             ParagraphPropertySet( space_before = 60,
                                                   space_after  = 60,
                                                   first_line_indent = -indent,
                                                   left_indent       = indent) )
        result.ParagraphStyles.append( ps )

    return result

class TAB  : pass
class LINE : pass

class RawCode :
    def __init__( self, data ) :
        self.Data = data

PAGE_NUMBER   = RawCode( r'{\field{\fldinst page}}'   )
TOTAL_PAGES   = RawCode( r'{\field{\fldinst numpages}}' )
SECTION_PAGES = RawCode( r'{\field{\fldinst sectionpages}}' )
ARIAL_BULLET  = RawCode( r'{\f2\'95}' )

def _get_jpg_dimensions( fin ):
    """
    converted from: http://dev.w3.org/cvsweb/Amaya/libjpeg/rdjpgcom.c?rev=1.2
    """

    M_SOF0   = chr( 0xC0 )  #   /* Start Of Frame N */
    M_SOF1   = chr( 0xC1 )  #   /* N indicates which compression process */
    M_SOF2   = chr( 0xC2 )  #   /* Only SOF0-SOF2 are now in common use */
    M_SOF3   = chr( 0xC3 )  #
    M_SOF5   = chr( 0xC5 )  #   /* NB: codes C4 and CC are NOT SOF markers */
    M_SOF6   = chr( 0xC6 )  #
    M_SOF7   = chr( 0xC7 )  #
    M_SOF9   = chr( 0xC9 )  #
    M_SOF10  = chr( 0xCA )  #
    M_SOF11  = chr( 0xCB )  #
    M_SOF13  = chr( 0xCD )  #
    M_SOF14  = chr( 0xCE )  #
    M_SOF15  = chr( 0xCF )  #
    M_SOI    = chr( 0xD8 )  #   /* Start Of Image (beginning of datastream) */
    M_EOI    = chr( 0xD9 )  #   /* End Of Image (end of datastream) */

    M_FF = chr( 0xFF )

    MARKERS = [ M_SOF0, M_SOF1,  M_SOF2,  M_SOF3,
                M_SOF5, M_SOF6,  M_SOF7,  M_SOF9,
                M_SOF10,M_SOF11, M_SOF13, M_SOF14,
                M_SOF15 ]

    def get_length() :
        b1 = fin.read( 1 )
        b2 = fin.read( 1 )
        return (ord(b1) << 8) + ord(b2)

    def next_marker() :
        #  markers come straight after an 0xFF so skip everything
        #  up to the first 0xFF that we find
        while fin.read(1) != M_FF :
            pass

        #  there can be more than one 0xFF as they can be used
        #  for padding so we are now looking for the first byte
        #  that isn't an 0xFF, this will be the marker
        while True :
            result = fin.read(1)
            if result != M_FF :
                return result

        raise Exception( 'Invalid JPEG' )

    #  BODY OF THE FUNCTION
    if not ((fin.read(1) == M_FF) and (fin.read(1) == M_SOI)) :
        raise Exception( 'Invalid Jpeg' )

    while True :
        marker = next_marker()

        #  the marker is always followed by two bytes representing the length of the data field
        length = get_length ()
        if length < 2 : raise Exception( "Erroneous JPEG marker length" )

        #  if it is a compression process marker then it will contain the dimension of the image
        if marker in MARKERS :
            #  the next byte is the data precision, just skip it
            fin.read(1)

            #  bingo
            image_height = get_length()
            image_width  = get_length()
            return image_width, image_height

        #  just skip whatever data it contains
        fin.read( length - 2 )

    raise Exception( 'Invalid JPEG, end of stream reached' )


_PNG_HEADER = '\x89\x50\x4e'
def _get_png_dimensions( data ) :
    if data[0:3] != _PNG_HEADER :
        raise Exception( 'Invalid PNG image' )

    width  = (ord(data[18]) * 256) + (ord(data[19]))
    height = (ord(data[22]) * 256) + (ord(data[23]))
    return width, height

def _get_emf_dimensions( fin ):
    import struct
    def get_DWORD():
        return struct.unpack("<L",fin.read(4))[0]
    def get_LONG():
        return struct.unpack("<l",fin.read(4))[0]
    def get_WORD():
        return struct.unpack("<H",fin.read(2))[0]
    class Empty:
        pass
    header = Empty()
    header.RecordType = get_DWORD()      # Record type
    header.RecordSize = get_DWORD()      # Size of the record in bytes
    header.BoundsLeft = get_LONG()       # Left inclusive bounds
    header.BoundsTop = get_LONG()        # Top inclusive bounds
    header.BoundsRight = get_LONG()      # Right inclusive bounds
    header.BoundsBottom = get_LONG()     # Bottom inclusive bounds
    header.FrameLeft = get_LONG()        # Left side of inclusive picture frame
    header.FrameTop = get_LONG()         # Top side of inclusive picture frame
    header.FrameRight = get_LONG()       # Right side of inclusive picture frame
    header.FrameBottom = get_LONG()      # Bottom side of inclusive picture frame
    header.Signature = get_DWORD()       # Signature ID (always 0x464D4520)
    header.Version = get_DWORD()         # Version of the metafile
    header.Size = get_DWORD()            # Size of the metafile in bytes
    header.NumOfRecords = get_DWORD()    # Number of records in the metafile
    header.NumOfHandles = get_WORD()     # Number of handles in the handle table
    header.Reserved = get_WORD()         # Not used (always 0)
    header.SizeOfDescrip = get_DWORD()   # Size of description string in WORDs
    header.OffsOfDescrip = get_DWORD()   # Offset of description string in metafile
    header.NumPalEntries = get_DWORD()   # Number of color palette entries
    header.WidthDevPixels = get_LONG()   # Width of reference device in pixels
    header.HeightDevPixels = get_LONG()  # Height of reference device in pixels
    header.WidthDevMM = get_LONG()       # Width of reference device in millimeters
    header.HeightDevMM = get_LONG()      # Height of reference device in millimeters

    if 0:
        klist = header.__dict__.keys()
        klist.sort()
        for k in klist:
            print "%20s:%s" % (k,header.__dict__[k])

    dw = header.FrameRight-header.FrameLeft
    dh = header.FrameBottom-header.FrameTop

    # convert from 0.01mm units to 1/72in units
    return int(dw * 72.0/2540.0), int(dh * 72.0/2540.0)

class Image( RawCode ) :

    #  Need to add in the width and height in twips as it crashes
    #  word xp with these values.  Still working out the most
    #  efficient way of getting these values.
    # \picscalex100\picscaley100\piccropl0\piccropr0\piccropt0\piccropb0
    # picwgoal900\pichgoal281

    PNG_LIB = 'pngblip'
    JPG_LIB = 'jpegblip'
    EMF_LIB = 'emfblip'
    PICT_TYPES = { 'png' : PNG_LIB,
                   'jpg' : JPG_LIB,
                   'emf' : EMF_LIB}

    def __init__( self, infile, **kwargs ) :

        if hasattr( infile, 'read' ):
            fin = infile
            if 'datatype' not in kwargs.keys():
                msg = "If passing in a file object, you must also specify type='xxx' where xxx is one of %s" % self.PICT_TYPES.keys()
                raise ValueError,msg
            file_name = kwargs.pop('datatype')
        else:
            fin = file( infile, 'rb' )
            file_name = infile

        pict_type = self.PICT_TYPES[ file_name[ -3 : ].lower() ]
        if pict_type == self.PNG_LIB :
            width, height = _get_png_dimensions( fin.read( 100 ) )
        elif pict_type == self.JPG_LIB :
            width, height = _get_jpg_dimensions( fin )
        elif pict_type == self.EMF_LIB :
            width, height = _get_emf_dimensions( fin )


        # if user specified height or width but not both, then
        # scale unspecified dimension to maintain aspect ratio

        if ('width' in kwargs) and ('height' not in kwargs):
            height = int(height * float(kwargs['width'])/width)
        elif ('height' in kwargs) and ('width' not in kwargs):
            width = int(width * float(kwargs['height'])/height)

        width  = kwargs.pop('width',width)
        height = kwargs.pop('height', height)

        codes = [ pict_type,
                  'picwgoal%s' % (width  * 20),
                  'pichgoal%s' % (height * 20) ]
        # let user specify global scaling
        scale = kwargs.pop('scale',100)

        for kwarg, code, default in [ ( 'scale_x',     'scalex', scale ),
                                      ( 'scale_y',     'scaley', scale ),
                                      ( 'crop_left',   'cropl',    '0' ),
                                      ( 'crop_right',  'cropr',    '0' ),
                                      ( 'crop_top',    'cropt',    '0' ),
                                      ( 'crop_bottom', 'cropb',    '0' ) ] :
            codes.append( 'pic%s%s' % ( code, kwargs.pop( kwarg, default ) ) )


        #  reset back to the start of the file to get all of it and now
        #  turn it into hex.
        fin.seek( 0, 0 )
        image = hexlify( fin.read() )
        fin.close()
        data = []
        for i in range( 0, len( image ), 128 ) :
            data.append( image[ i : i + 128 ] )

        data = r'{\pict{\%s}%s}' % ( '\\'.join( codes ), '\n'.join( data ) )
        RawCode.__init__( self, data )

    def ToRawCode( self, var_name ) :
        return '%s = RawCode( """%s""" )' % ( var_name, self.Data )

class Text :
    def __init__( self, *params ) :
        self.Data       = None
        self.Style      = None
        self.Properties = None
        self.Shading    = None

        for param in params :
            if   isinstance( param, TextStyle  ) : self.Style      = param
            elif isinstance( param, TextPS     ) : self.Properties = param
            elif isinstance( param, ShadingPS  ) : self.Shading    = param
            else :
                #   otherwise let the rendering custom handler sort it out itself
                self.Data = param

    def SetData( self, value ) :
        self.Data = value

class Inline( list ) :
    def __init__( self, *params ) :
        super( Inline, self ).__init__()

        self.Style      = None
        self.Properties = None
        self.Shading    = None

        self._append = super( Inline, self ).append

        for param in params :
            if   isinstance( param, TextStyle  ) : self.Style      = param
            elif isinstance( param, TextPS     ) : self.Properties = param
            elif isinstance( param, ShadingPS  ) : self.Shading    = param
            else :
                #   otherwise we add to it to our list of elements and let
                #   the rendering custom handler sort it out itself.
                self.append( param )

    def append( self, *params ) :
        #   filter out any that are explicitly None
        [ self._append( param ) for param in params if param is not None ]

class Paragraph( list ) :
    def __init__( self, *params ) :
        super( Paragraph, self ).__init__()

        self.Style      = None
        self.Properties = None
        self.Frame      = None
        self.Shading    = None

        self._append = super( Paragraph, self ).append

        for param in params :
            if   isinstance( param, ParagraphStyle ) : self.Style      = param
            elif isinstance( param, ParagraphPS    ) : self.Properties = param
            elif isinstance( param, FramePS        ) : self.Frame      = param
            elif isinstance( param, ShadingPS      ) : self.Shading    = param
            else :
                #   otherwise we add to it to our list of elements and let
                #   the rendering custom handler sort it out itself.
                self.append( param )

    def append( self, *params ) :
        #   filter out any that are explicitly None
        [ self._append( param ) for param in params if param is not None ]

    def insert( self, index, value ) :
        if value is not None :
            super( Paragraph, self ).insert( index, value )

class Table :
    LEFT    = 1
    RIGHT   = 2
    CENTER  = 3
    ALIGNMENT = [ LEFT, RIGHT, CENTER ]

    NO_WRAPPING = 1
    WRAP_AROUND = 2
    WRAPPING = [ NO_WRAPPING, WRAP_AROUND ]

    #   trrh height of row, 0 means automatically adjust, use negative for an absolute
    #   trgaph is half of the space between a table cell in width, reduce this one
    #   to get a really tiny column

    def __init__( self, *column_widths, **kwargs ) :

        self.Rows = []

        self.SetAlignment      ( kwargs.pop( 'alignment',         self.LEFT ) )
        self.SetLeftOffset     ( kwargs.pop( 'left_offset',       None      ) )
        self.SetGapBetweenCells( kwargs.pop( 'gap_between_cells', None      ) )
        self.SetColumnWidths   ( *column_widths         )

        assert not kwargs, 'invalid keyword args %s' % kwargs

    def SetAlignment( self, value ) :
        assert value is None or value in self.ALIGNMENT
        self.Alignment = value or self.LEFT
        return self

    def SetLeftOffset( self, value ) :
        self.LeftOffset = value
        return self

    def SetGapBetweenCells( self, value ) :
        self.GapBetweenCells = value
        return self

    def SetColumnWidths( self, *column_widths ) :
        self.ColumnWidths = column_widths
        self.ColumnCount  = len( column_widths )
        return self

    def AddRow( self, *cells ) :
        height = None
        if isinstance( cells[ 0 ], (IntType, FloatType, LongType) ):
            height = int( cells[ 0 ] )
            cells  = cells[ 1 : ]

        #  make sure all of the spans add up to the number of columns
        #  otherwise the table will get corrupted
        if self.ColumnCount != sum( [ cell.Span for cell in cells ] ) :
            raise Exception( 'ColumnCount != the total of this row\'s cell.Spans.' )

        self.Rows.append( ( height, cells ) )

    append = AddRow

class Cell( list ) :

    """
    \clvertalt  Text is top-aligned in cell (the default).
    \clvertalc  Text is centered vertically in cell.
    \clvertalb  Text is bottom-aligned in cell.
    \cltxlrtb   Vertical text aligned left (direction bottom up).
    \cltxtbrl   Vertical text aligned right (direction top down).
    """

    ALIGN_TOP    = 1
    ALIGN_CENTER = 2
    ALIGN_BOTTOM = 3

    FLOW_LR_TB          = 1
    FLOW_RL_TB          = 2
    FLOW_LR_BT          = 3
    FLOW_VERTICAL_LR_TB = 4
    FLOW_VERTICAL_TB_RL = 5

    def __init__( self, *params, **kwargs ) :
        super( Cell, self ).__init__()

        self.SetFrame  ( None )
        self.SetMargins( None )

        self.SetAlignment( kwargs.get( 'alignment', self.ALIGN_TOP  ) )
        self.SetFlow     ( kwargs.get( 'flow'     , self.FLOW_LR_TB ) )
        self.SetSpan     ( kwargs.get( 'span',      1               ) )

        self.SetStartVerticalMerge( kwargs.get( 'start_vertical_merge', False ) )
        self.SetVerticalMerge     ( kwargs.get( 'vertical_merge',       False ) )

        self._append = super( Cell, self ).append

        for param in params :
            if   isinstance( param, StringType ) : self.append    ( param )
            elif isinstance( param, Paragraph  ) : self.append    ( param )
            elif isinstance( param, FramePS    ) : self.SetFrame  ( param )
            elif isinstance( param, MarginsPS  ) : self.SetMargins( param )

    def SetFrame( self, value ) :
        self.Frame = value
        return self

    def SetMargins( self, value ) :
        self.Margins = value
        return self

    def SetAlignment( self, value ) :
        assert value in [ self.ALIGN_TOP, self.ALIGN_CENTER, self.ALIGN_BOTTOM ] #, self.ALIGN_TEXT_TOP_DOWN, self.ALIGN_TEXT_BOTTOM_UP ]
        self.Alignment = value

    def SetFlow( self, value ) :
        assert value in [ self.FLOW_LR_TB, self.FLOW_RL_TB, self.FLOW_LR_BT, self.FLOW_VERTICAL_LR_TB, self.FLOW_VERTICAL_TB_RL ]
        self.Flow = value

    def SetSpan( self, value ) :
        #  must be a positive integer
        self.Span = int( max( value, 1 ) )
        return self

    def SetStartVerticalMerge( self, value ) :
        self.StartVerticalMerge = False
        if value :
            self.StartVerticalMerge = True
        return self

    def SetVerticalMerge( self, value ) :
        self.VerticalMerge  = False
        if value :
            self.VerticalMerge = True
        return self

    def append( self, *params ) :
        [ self._append( param ) for param in params ]

class Document :
    def __init__( self, style_sheet=None, default_language=None, view_kind=None, view_zoom_kind=None, view_scale=None ) :
        self.StyleSheet = style_sheet or MakeDefaultStyleSheet()
        self.Sections = AttributedList( Section )

        self.SetTitle( None )

        self.DefaultLanguage = default_language or Languages.DEFAULT
        self.ViewKind        = view_kind        or ViewKind.DEFAULT
        self.ViewZoomKind    = view_zoom_kind
        self.ViewScale       = view_scale

    def NewSection( self, *params, **kwargs ) :
        result = Section( *params, **kwargs )
        self.Sections.append( result )
        return result

    def SetTitle( self, value ) :
        self.Title = value
        return self

    def Copy( self ) :
        result = Document( style_sheet      = self.StyleSheet.Copy(),
                           default_language = self.DefaultLanguage,
                           view_kind        = self.ViewKind,
                           view_zoom_kind   = self.ViewZoomKind,
                           view_scale       = self.ViewScale )
        result.SetTitle( self.Title )
        result.Sections = self.Sections.Copy()

        return result

def TEXT( *params, **kwargs ) :
    text_props = TextPropertySet()
    text_props.SetFont     ( kwargs.get( 'font',      None  ) )
    text_props.SetSize     ( kwargs.get( 'size',      None  ) )
    text_props.SetBold     ( kwargs.get( 'bold',      False ) )
    text_props.SetItalic   ( kwargs.get( 'italic',    False ) )
    text_props.SetUnderline( kwargs.get( 'underline', False ) )
    text_props.SetColour   ( kwargs.get( 'colour',    None  ) )

    if len( params ) == 1 :
        return Text( params[ 0 ], text_props )

    result = Inline( text_props )
    apply( result.append, params )
    return result

def B( *params ) :
    text_props = TextPropertySet( bold=True )

    if len( params ) == 1 :
        return Text( params[ 0 ], text_props )

    result = Inline( text_props )
    apply( result.append, params )
    return result

def I( *params ) :
    text_props = TextPropertySet( italic=True )

    if len( params ) == 1 :
        return Text( params[ 0 ], text_props )

    result = Inline( text_props )
    apply( result.append, params )
    return result

def U( *params ) :
    text_props = TextPropertySet( underline=True )

    if len( params ) == 1 :
        return Text( params[ 0 ], text_props )

    result = Inline( text_props )
    apply( result.append, params )
    return result

