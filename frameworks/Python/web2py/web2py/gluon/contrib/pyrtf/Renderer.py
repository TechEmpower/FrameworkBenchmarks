from types import StringType, ListType, TupleType
from copy import deepcopy
from Elements import *

DEFAULT_TAB_WIDTH = 720

ParagraphAlignmentMap = { ParagraphPropertySet.LEFT       : 'ql',
                          ParagraphPropertySet.RIGHT      : 'qr',
                          ParagraphPropertySet.CENTER     : 'qc',
                          ParagraphPropertySet.JUSTIFY    : 'qj',
                          ParagraphPropertySet.DISTRIBUTE : 'qd' }

TabAlignmentMap = { TabPropertySet.LEFT    : '',
                    TabPropertySet.RIGHT   : 'tqr',
                    TabPropertySet.CENTER  : 'tqc',
                    TabPropertySet.DECIMAL : 'tqdec' }

TableAlignmentMap = { Table.LEFT   : 'trql',
                      Table.RIGHT  : 'trqr',
                      Table.CENTER : 'trqc' }

CellAlignmentMap = { Cell.ALIGN_TOP            : '', # clvertalt
                     Cell.ALIGN_CENTER         : 'clvertalc',
                     Cell.ALIGN_BOTTOM         : 'clvertalb' }

CellFlowMap = { Cell.FLOW_LR_TB          : '',           # cltxlrtb, Text in a cell flows from left to right and top to bottom (default)
                Cell.FLOW_RL_TB          : 'cltxtbrl',   # Text in a cell flows right to left and top to bottom
                Cell.FLOW_LR_BT          : 'cltxbtlr',   # Text in a cell flows left to right and bottom to top
                Cell.FLOW_VERTICAL_LR_TB : 'cltxlrtbv',  # Text in a cell flows left to right and top to bottom, vertical
                Cell.FLOW_VERTICAL_TB_RL : 'cltxtbrlv' } # Text in a cell flows top to bottom and right to left, vertical

ShadingPatternMap = { ShadingPropertySet.HORIZONTAL             : 'bghoriz',
                      ShadingPropertySet.VERTICAL               : 'bgvert',
                      ShadingPropertySet.FORWARD_DIAGONAL       : 'bgfdiag',
                      ShadingPropertySet.BACKWARD_DIAGONAL      : 'bgbdiag',
                      ShadingPropertySet.VERTICAL_CROSS         : 'bgcross',
                      ShadingPropertySet.DIAGONAL_CROSS         : 'bgdcross',
                      ShadingPropertySet.DARK_HORIZONTAL        : 'bgdkhoriz',
                      ShadingPropertySet.DARK_VERTICAL          : 'bgdkvert',
                      ShadingPropertySet.DARK_FORWARD_DIAGONAL  : 'bgdkfdiag',
                      ShadingPropertySet.DARK_BACKWARD_DIAGONAL : 'bgdkbdiag',
                      ShadingPropertySet.DARK_VERTICAL_CROSS    : 'bgdkcross',
                      ShadingPropertySet.DARK_DIAGONAL_CROSS    : 'bgdkdcross' }

TabLeaderMap = { TabPropertySet.DOTS        : 'tldot',
                 TabPropertySet.HYPHENS     : 'tlhyph',
                 TabPropertySet.UNDERLINE   : 'tlul',
                 TabPropertySet.THICK_LINE  : 'tlth',
                 TabPropertySet.EQUAL_SIGN  : 'tleq' }

BorderStyleMap = { BorderPropertySet.SINGLE   : 'brdrs',
                   BorderPropertySet.DOUBLE   : 'brdrth',
                   BorderPropertySet.SHADOWED : 'brdrsh',
                   BorderPropertySet.DOUBLED  : 'brdrdb',
                   BorderPropertySet.DOTTED   : 'brdrdot',
                   BorderPropertySet.DASHED   : 'brdrdash',
                   BorderPropertySet.HAIRLINE : 'brdrhair' }

SectionBreakTypeMap = { Section.NONE   : 'sbknone',
                        Section.COLUMN : 'sbkcol',
                        Section.PAGE   : 'sbkpage',
                        Section.EVEN   : 'sbkeven',
                        Section.ODD    : 'sbkodd' }

class Settings( list ) :
    def __init__( self ) :
        super( Settings, self ).__init__()
        self._append = super( Settings, self ).append

    def append( self, value, mask=None, fallback=None ) :
        if (value is not 0) and value in [ False, None, '' ] :
            if fallback : self._append( self, fallback )

        else :
            if mask :
                if value is True :
                    value = mask
                else :
                    value = mask % value
            self._append( value )

    def Join( self ) :
        if self : return r'\%s' % '\\'.join( self )
        return ''

    def __repr__( self ) :
        return self.Join()

class Renderer :
    def __init__( self, write_custom_element_callback=None ) :
        self.character_style_map = {}
        self.paragraph_style_map = {}
        self.WriteCustomElement  = write_custom_element_callback

    #
    #   All of the Rend* Functions populate a Settings object with values
    #
    def _RendPageProperties( self, section, settings, in_section ) :
        #  this one is different from the others as it takes the settings from a
        if in_section :
            #paper_size_code   = 'psz%s'
            paper_width_code  = 'pgwsxn%s'
            paper_height_code = 'pghsxn%s'
            landscape         = 'lndscpsxn'
            margin_suffix     = 'sxn'

        else :
            #paper_size_code   = 'psz%s'
            paper_width_code  = 'paperw%s'
            paper_height_code = 'paperh%s'
            landscape         = 'landscape'
            margin_suffix     = ''

        #settings.append( section.Paper.Code,   paper_size_code  )
        settings.append( section.Paper.Width,  paper_width_code  )
        settings.append( section.Paper.Height, paper_height_code )

        if section.Landscape :
            settings.append( landscape )

        if section.FirstPageNumber :
            settings.append( section.FirstPageNumber, 'pgnstarts%s' )
            settings.append( 'pgnrestart' )

        self._RendMarginsPropertySet( section.Margins, settings, margin_suffix )

    def _RendShadingPropertySet( self, shading_props, settings, prefix='' ) :
        if not shading_props : return

        settings.append( shading_props.Shading, prefix + 'shading%s' )
        settings.append( ShadingPatternMap.get( shading_props.Pattern, False ) )

        settings.append( self._colour_map.get( shading_props.Foreground, False ), prefix + 'cfpat%s' )
        settings.append( self._colour_map.get( shading_props.Background, False ), prefix + 'cbpat%s' )

    def _RendBorderPropertySet( self, edge_props, settings ) :
        settings.append( BorderStyleMap[ edge_props.Style ] )
        settings.append( edge_props.Width                                , 'brdrw%s'  )
        settings.append( self._colour_map.get( edge_props.Colour, False ), 'brdrcf%s' )
        settings.append( edge_props.Spacing or False                     , 'brsp%s'   )

    def _RendFramePropertySet( self, frame_props, settings, tag_prefix='' ) :
        if not frame_props : return

        if frame_props.Top :
            settings.append( tag_prefix + 'brdrt' )
            self._RendBorderPropertySet( frame_props.Top, settings )

        if frame_props.Left :
            settings.append( tag_prefix + 'brdrl' )
            self._RendBorderPropertySet( frame_props.Left, settings )

        if frame_props.Bottom :
            settings.append( tag_prefix + 'brdrb' )
            self._RendBorderPropertySet( frame_props.Bottom, settings )

        if frame_props.Right :
            settings.append( tag_prefix + 'brdrr' )
            self._RendBorderPropertySet( frame_props.Right, settings )

    def _RendMarginsPropertySet( self, margin_props, settings, suffix='' ) :
        if not margin_props : return

        settings.append( margin_props.Top,    'margt' + suffix + '%s' )
        settings.append( margin_props.Left,   'margl' + suffix + '%s' )
        settings.append( margin_props.Bottom, 'margb' + suffix + '%s' )
        settings.append( margin_props.Right,  'margr' + suffix + '%s' )

    def _RendParagraphPropertySet( self, paragraph_props, settings ) :
        if not paragraph_props : return
        settings.append( ParagraphAlignmentMap[ paragraph_props.Alignment ] )

        settings.append( paragraph_props.SpaceBefore, 'sb%s' )
        settings.append( paragraph_props.SpaceAfter,  'sa%s' )

        #   then we have to find out all of the tabs
        width = 0
        for tab in paragraph_props.Tabs :
            settings.append( TabAlignmentMap[ tab.Alignment ]   )
            settings.append( TabLeaderMap.get( tab.Leader, '' ) )

            width += tab.Width or DEFAULT_TAB_WIDTH
            settings.append( 'tx%s' % width             )

        settings.append( paragraph_props.PageBreakBefore, 'pagebb' )

        settings.append( paragraph_props.FirstLineIndent, 'fi%s'   )
        settings.append( paragraph_props.LeftIndent,      'li%s'   )
        settings.append( paragraph_props.RightIndent,     'ri%s'   )

        if paragraph_props.SpaceBetweenLines :
            if paragraph_props.SpaceBetweenLines < 0 :
                settings.append( paragraph_props.SpaceBetweenLines, r'sl%s\slmult0' )
            else :
                settings.append( paragraph_props.SpaceBetweenLines, r'sl%s\slmult1' )

    def _RendTextPropertySet( self, text_props, settings ) :
        if not text_props : return

        if text_props.Expansion :
            settings.append( text_props.Expansion, 'expndtw%s' )

        settings.append( text_props.Bold,            'b'    )
        settings.append( text_props.Italic,          'i'    )
        settings.append( text_props.Underline,       'ul'   )
        settings.append( text_props.DottedUnderline, 'uld'  )
        settings.append( text_props.DoubleUnderline, 'uldb' )
        settings.append( text_props.WordUnderline,   'ulw'  )

        settings.append( self._font_map.get( text_props.Font, False ), 'f%s' )
        settings.append( text_props.Size, 'fs%s' )
        settings.append( self._colour_map.get( text_props.Colour, False ), 'cf%s' )

        if text_props.Frame :
            frame = text_props.Frame
            settings.append( 'chbrdr' )
            settings.append( BorderStyleMap[ frame.Style ] )
            settings.append( frame.Width                                , 'brdrw%s' )
            settings.append( self._colour_map.get( frame.Colour, False ), 'brdrcf%s' )

    #
    #   All of the Write* functions will write to the internal file object
    #
    #   the _ ones probably don't need to be used by anybody outside
    #   but the other ones like WriteTextElement could be used in the Custom
    #   callback.
    def Write( self, document, fout ) :
        #  write all of the standard stuff based upon the first document
        self._doc  = document
        self._fout = fout
        self._WriteDocument  ()
        self._WriteColours   ()
        self._WriteFonts     ()
        self._WriteStyleSheet()

        settings = Settings()
        self._RendPageProperties( self._doc.Sections[ 0 ], settings, in_section=False )
        self._write( repr( settings ) )

        #  handle the simplest case first, we don't need to do anymore mucking around
        #  with section headers, etc we can just rip the document out
        if len( document.Sections ) == 1 :
            self._WriteSection( document.Sections[ 0 ],
                                is_first   = True,
                                add_header = False )

        else :
            for section_idx, section in enumerate( document.Sections ) :
                is_first       = section_idx == 0
                add_header     = True
                self._WriteSection( section, is_first, add_header )

        self._write( '}' )

        del self._fout, self._doc, self._CurrentStyle

    def _write( self, data, *params ) :
        #----------------------------------
        # begin modification
        # by Herbert Weinhandl
        # to convert accented characters
        # to their rtf-compatible form
        #for c in range( 128, 256 ) :
        #   data = data.replace( chr(c), "\'%x" % c)
        # end modification
        #
        #  This isn't the right place for this as it is going to do
        #  this loop for all sorts of writes, including settings, control codes, etc.
        #
        #  I will create a def _WriteText (or something) method that is used when the
        #  actual string that is to be viewed in the document is written, this can then
        #  do the final accented character check.
        #
        #  I left it here so that I remember to do the right thing when I have time
        #----------------------------------

        if params : data = data % params
        self._fout.write( data )

    def _WriteDocument( self ) :
        settings = Settings()

        assert Languages.IsValid   ( self._doc.DefaultLanguage )
        assert ViewKind.IsValid    ( self._doc.ViewKind        )
        assert ViewZoomKind.IsValid( self._doc.ViewZoomKind    )
        assert ViewScale.IsValid   ( self._doc.ViewScale       )

        settings.append( self._doc.DefaultLanguage, 'deflang%s'   )
        settings.append( self._doc.ViewKind       , 'viewkind%s'  )
        settings.append( self._doc.ViewZoomKind   , 'viewzk%s'    )
        settings.append( self._doc.ViewScale      , 'viewscale%s' )

        self._write( "{\\rtf1\\ansi\\ansicpg1252\\deff0%s\n" % settings )

    def _WriteColours( self ) :
        self._write( r"{\colortbl ;" )

        self._colour_map = {}
        offset = 0
        for colour in self._doc.StyleSheet.Colours :
            self._write( r'\red%s\green%s\blue%s;', colour.Red, colour.Green, colour.Blue )
            self._colour_map[ colour ] = offset + 1
            offset += 1
        self._write( "}\n" )

    def _WriteFonts( self ) :
        self._write( r'{\fonttbl' )

        self._font_map = {}
        offset = 0
        for font in self._doc.StyleSheet.Fonts :
            pitch     = ''
            panose    = ''
            alternate = ''
            if font.Pitch     : pitch     = r'\fprq%s'    % font.Pitch
            if font.Panose    : panose    = r'{\*\panose %s}' % font.Panose
            if font.Alternate : alternate = r'{\*\falt %s}'   % font.Alternate.Name

            self._write( r'{\f%s\f%s%s\fcharset%s%s %s%s;}',
                         offset,
                         font.Family,
                         pitch,
                         font.CharacterSet,
                         panose,
                         font.Name,
                         alternate )

            self._font_map[ font ] = offset
            offset += 1

        self._write( "}\n" )

    def _WriteStyleSheet( self ) :
        self._write( r"{\stylesheet" )

        #   TO DO: character styles, does anybody actually use them?

        offset_map = {}
        for idx, style in enumerate( self._doc.StyleSheet.ParagraphStyles ) :
            offset_map[ style ] = idx

        #   paragraph styles
        self.paragraph_style_map = {}
        for idx, style in enumerate( self._doc.StyleSheet.ParagraphStyles ) :

            if idx == 0 :
                default = style
            else :
                self._write( '\n' )

            settings = Settings()

            #   paragraph properties
            self._RendParagraphPropertySet( style.ParagraphPropertySet, settings )
            self._RendFramePropertySet    ( style.FramePropertySet,     settings )
            self._RendShadingPropertySet  ( style.ShadingPropertySet,   settings )

            #   text properties
            self._RendTextPropertySet   ( style.TextStyle.TextPropertySet,     settings )
            self._RendShadingPropertySet( style.TextStyle.ShadingPropertySet,  settings )

            #   have to take
            based_on = '\\sbasedon%s' % offset_map.get( style.BasedOn, 0 )
            next     = '\\snext%s'    % offset_map.get( style.Next,    0 )

            inln = '\\s%s%s' % ( idx, settings )
            self._write( "{%s%s%s %s;}", inln, based_on, next, style.Name )

            self.paragraph_style_map[ style ] = inln

        #   if now style is specified for the first paragraph to be written, this one
        #   will be used
        self._CurrentStyle = self.paragraph_style_map[ default ]

        self._write( "}\n" )

    def _WriteSection( self, section, is_first, add_header ) :

        def WriteHF( hf, rtfword ) :
            #if not hf : return

            #  if we don't have anything in the header/footer then include
            #  a blank paragraph, this stops it from picking up the header/footer
            #  from the previous section
            # if not hf :   hf = [ Paragraph( '' ) ]
            if not hf : hf = []

            self._write( '{\\%s' % rtfword )
            self._WriteElements( hf )
            self._write( '}\n' )

        settings = Settings()

        if not is_first :
            #  we need to finish off the preceding section
            #  and reset all of our defaults back to standard
            settings.append( 'sect'  )

        #  reset to our defaults
        settings.append( 'sectd' )

        if add_header :
            settings.append( SectionBreakTypeMap[ section.BreakType ] )
            self._RendPageProperties( section, settings, in_section=True )

        settings.append( section.HeaderY, 'headery%s' )
        settings.append( section.FooterY, 'footery%s' )

        #  write all of these out now as we need to do a write elements in the
        #  next section
        self._write( repr( settings ) )

        #   finally after all that has settled down we can do the
        #   headers and footers
        if section.FirstHeader or section.FirstFooter :
            #  include the titlepg flag if the first page has a special format
            self._write( r'\titlepg' )
            WriteHF( section.FirstHeader, 'headerf' )
            WriteHF( section.FirstFooter, 'footerf' )

        WriteHF( section.Header, 'header' )
        WriteHF( section.Footer, 'footer' )

        #   and at last the contents of the section that actually appear on the page
        self._WriteElements( section )

    def _WriteElements( self, elements ) :
        new_line = ''
        for element in elements :
            self._write( new_line )
            new_line = '\n'

            clss = element.__class__

            if clss == Paragraph :
                self.WriteParagraphElement( element )

            elif clss == Table :
                self.WriteTableElement( element )

            elif clss == StringType :
                self.WriteParagraphElement( Paragraph( element ) )

            elif clss in [ RawCode, Image ] :
                self.WriteRawCode( element )

            #elif clss == List  :
            #   self._HandleListElement( element )

            elif self.WriteCustomElement :
                self.WriteCustomElement( self, element )

            else :
                raise Exception( "Don't know how to handle elements of type %s" % clss )

    def WriteParagraphElement( self, paragraph_elem, tag_prefix='', tag_suffix=r'\par', opening='{', closing='}' ) :

        #   the tag_prefix and the tag_suffix take care of paragraphs in tables.  A
        #   paragraph in a table requires and extra tag at the front (intbl) and we
        #   don't want the ending tag everytime.  We want it for all paragraphs but
        #   the last.

        overrides = Settings()
        self._RendParagraphPropertySet( paragraph_elem.Properties, overrides )
        self._RendFramePropertySet    ( paragraph_elem.Frame,      overrides )
        self._RendShadingPropertySet  ( paragraph_elem.Shading,    overrides )

        #   when writing the RTF the style is carried from the previous paragraph to the next,
        #   so if the currently written paragraph has a style then make it the current one,
        #   otherwise leave it as it was
        self._CurrentStyle = self.paragraph_style_map.get( paragraph_elem.Style, self._CurrentStyle )

        self._write( r'%s\pard\plain%s %s%s ' % ( opening, tag_prefix, self._CurrentStyle, overrides ) )

        for element in paragraph_elem :

            if isinstance( element, StringType ) :
                self._write( element )

            elif isinstance( element, RawCode ) :
                self._write( element.Data )

            elif isinstance( element, Text ) :
                self.WriteTextElement( element )

            elif isinstance( element, Inline ) :
                self.WriteInlineElement( element )

            elif element == TAB :
                self._write( r'\tab ' )

            elif element == LINE :
                self._write( r'\line ' )

            elif self.WriteCustomElement :
                self.WriteCustomElement( self, element )

            else :
                raise Exception( 'Don\'t know how to handle %s' % element )

        self._write( tag_suffix + closing )

    def WriteRawCode( self, raw_elem ) :
        self._write( raw_elem.Data )

    def WriteTextElement( self, text_elem ) :
        overrides = Settings()

        self._RendTextPropertySet   ( text_elem.Properties, overrides )
        self._RendShadingPropertySet( text_elem.Shading,    overrides, 'ch' )

        #   write the wrapper and then let the custom handler have a go
        if overrides : self._write( '{%s ' % repr( overrides ) )

        #   if the data is just a string then we can now write it
        if isinstance( text_elem.Data, StringType ) :
            self._write( text_elem.Data or '' )

        elif text_elem.Data == TAB :
            self._write( r'\tab ' )

        else :
            self.WriteCustomElement( self, text_elem.Data )

        if overrides : self._write( '}' )

    def WriteInlineElement( self, inline_elem ) :
        overrides = Settings()

        self._RendTextPropertySet   ( inline_elem.Properties, overrides )
        self._RendShadingPropertySet( inline_elem.Shading,    overrides, 'ch' )

        #   write the wrapper and then let the custom handler have a go
        if overrides : self._write( '{%s ' % repr( overrides ) )

        for element in inline_elem :
            #   if the data is just a string then we can now write it
            if isinstance( element, StringType ) :
                self._write( element )

            elif isinstance( element, RawCode ) :
                self._write( element.Data )

            elif element == TAB :
                self._write( r'\tab ' )

            elif element == LINE :
                self._write( r'\line ' )

            else :
                self.WriteCustomElement( self, element )

        if overrides : self._write( '}' )

    def WriteText( self, text ) :
        self._write( text or '' )

    def WriteTableElement( self, table_elem ) :

        vmerge = [ False ] * table_elem.ColumnCount
        for height, cells in table_elem.Rows :

            #   calculate the right hand edge of the cells taking into account the spans
            offset   = table_elem.LeftOffset or 0
            cellx    = []
            cell_idx = 0
            for cell in cells :
                cellx.append( offset + sum( table_elem.ColumnWidths[ : cell_idx + cell.Span ] ) )
                cell_idx += cell.Span

            self._write( r'{\trowd' )

            settings = Settings()

            #   the spec says that this value is mandatory and I think that 108 is the default value
            #   so I'll take care of it here
            settings.append( table_elem.GapBetweenCells or 108, 'trgaph%s' )
            settings.append( TableAlignmentMap[ table_elem.Alignment ] )
            settings.append( height, 'trrh%s' )
            settings.append( table_elem.LeftOffset, 'trleft%s' )

            width = table_elem.LeftOffset or 0
            for idx, cell in enumerate( cells ) :
                self._RendFramePropertySet  ( cell.Frame,   settings, 'cl' )

                #  cells don't have margins so I don't know why I was doing this
                #  I think it might have an affect in some versions of some WPs.
                #self._RendMarginsPropertySet( cell.Margins, settings, 'cl' )

                #  if we are starting to merge or if this one is the first in what is
                #  probably a series of merges then start the vertical merging
                if cell.StartVerticalMerge or (cell.VerticalMerge and not vmerge[ idx ]) :
                    settings.append( 'clvmgf' )
                    vmerge[ idx ] = True

                elif cell.VerticalMerge :
                    #..continuing a merge
                    settings.append( 'clvmrg' )

                else :
                    #..no merging going on so make sure that it is off
                    vmerge[ idx ] = False

                #  for any cell in the next row that is covered by this span we
                #  need to run off the vertical merging as we don't want them
                #  merging up into this spanned cell
                for vmerge_idx in range( idx + 1, idx + cell.Span - 1 ) :
                    vmerge[ vmerge_idx ] = False

                settings.append( CellAlignmentMap[ cell.Alignment ] )
                settings.append( CellFlowMap[ cell.Flow ] )

                #  this terminates the definition of a cell and represents the right most edge of the cell from the left margin
                settings.append( cellx[ idx ], 'cellx%s' )

            self._write( repr( settings ) )

            for cell in cells :
                if len( cell ) :
                    last_idx = len( cell ) - 1
                    for element_idx, element in enumerate( cell ) :
                        #   wrap plain strings in paragraph tags
                        if isinstance( element, StringType ) :
                            element = Paragraph( element )

                        #   don't forget the prefix or else word crashes and does all sorts of strange things
                        if element_idx == last_idx :
                            self.WriteParagraphElement( element, tag_prefix=r'\intbl', tag_suffix='', opening='', closing='' )

                        else :
                            self.WriteParagraphElement( element, tag_prefix=r'\intbl', opening='', closing='' )

                    self._write( r'\cell' )

                else :
                    self._write( r'\pard\intbl\cell' )

            self._write( '\\row}\n' )

