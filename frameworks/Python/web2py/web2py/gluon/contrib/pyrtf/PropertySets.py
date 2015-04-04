"""
PropertySets group common attributes together, each property set is used to control a specific part of the rendering.

PropertySets can be used in different elements of the document.

For example the FramePropertySet is used in paragraphs, tables, cells, etc.

The TextPropertySet can be used for text or in a Paragraph Style.

"""

from    types       import  StringType
from    copy        import  deepcopy


#
#   We need some basic Type like fonts, colours and paper definitions
#
def MakeAttributeName( value ) :
    assert value and type( value ) is StringType
    value = value.replace( ' ', '' )
    return value

class AttributedList( list ) :
    def __init__( self, accepted_type=None ) :
        super( AttributedList, self ).__init__()
        self.AcceptedType = accepted_type
        self._append = super( AttributedList, self ).append

    def append( self, *values ) :
        for value in values :
            if self.AcceptedType : assert isinstance( value, self.AcceptedType )

            self._append( value )

            name = getattr( value, 'Name', None )
            if name :
                name = MakeAttributeName( value.Name )
                setattr( self, name, value )

    def __deepcopy__( self, memo ) :
        result = self.__class__()
        result.append( *self[:] )
        return result

class Colour :
    def __init__( self, name, red, green, blue ) :
        self.SetName ( name  )
        self.SetRed  ( red   )
        self.SetGreen( green )
        self.SetBlue ( blue  )

    def SetName( self, value ) :
        self.Name = value
        return self

    def SetRed( self, value ) :
        self.Red = value
        return self

    def SetGreen( self, value ) :
        self.Green = value
        return self

    def SetBlue( self, value ) :
        self.Blue = value
        return self

class Colours( AttributedList ) :
    def __init__( self ) :
        super( Colours, self ).__init__( Colour )

class Font :
    def __init__( self, name, family, character_set = 0, pitch = None, panose = None, alternate = None ) :
        self.SetName        ( name          )
        self.SetFamily      ( family        )
        self.SetCharacterSet( character_set )
        self.SetPitch       ( pitch         )
        self.SetPanose      ( panose        )
        self.SetAlternate   ( alternate     )

    def SetName( self, value ) :
        self.Name = value
        return self

    def SetFamily( self, value ) :
        self.Family = value
        return self

    def SetCharacterSet( self, value ) :
        self.CharacterSet = value
        return self

    def SetPitch( self, value ) :
        self.Pitch = value
        return self

    def SetPanose( self, value ) :
        self.Panose = value
        return self

    def SetAlternate( self, value ) :
        self.Alternate = value
        return self

class Fonts( AttributedList ) :
    def __init__( self ) :
        super( Fonts, self ).__init__( Font )

class Paper :
    def __init__( self, name, code, description, width, height ) :
        self.SetName       ( name        )
        self.SetCode       ( code        )
        self.SetDescription( description )
        self.SetWidth      ( width       )
        self.SetHeight     ( height      )

    def SetName( self, value ) :
        self.Name = value
        return self

    def SetCode( self, value ) :
        self.Code = value
        return self

    def SetDescription( self, value ) :
        self.Description = value
        return self

    def SetWidth( self, value ) :
        self.Width = value
        return self

    def SetHeight( self, value ) :
        self.Height = value
        return self

class Papers( AttributedList ) :
    def __init__( self ) :
        super( Papers, self ).__init__( Paper )

#
#   Then we have property sets which represent different aspects of Styles
#
class MarginsPropertySet :
    def __init__( self, top=None, left=None, bottom=None, right=None ) :
        self.SetTop   ( top    )
        self.SetLeft  ( left   )
        self.SetBottom( bottom )
        self.SetRight ( right  )

    def SetTop( self, value ) :
        self.Top = value
        return self

    def SetLeft( self, value ) :
        self.Left = value
        return self

    def SetBottom( self, value ) :
        self.Bottom = value
        return self

    def SetRight( self, value ) :
        self.Right = value
        return self

class ShadingPropertySet :
    HORIZONTAL             =  1
    VERTICAL               =  2
    FORWARD_DIAGONAL       =  3
    BACKWARD_DIAGONAL      =  4
    VERTICAL_CROSS         =  5
    DIAGONAL_CROSS         =  6
    DARK_HORIZONTAL        =  7
    DARK_VERTICAL          =  8
    DARK_FORWARD_DIAGONAL  =  9
    DARK_BACKWARD_DIAGONAL = 10
    DARK_VERTICAL_CROSS    = 11
    DARK_DIAGONAL_CROSS    = 12
    PATTERNS = [ HORIZONTAL,
                 VERTICAL,
                 FORWARD_DIAGONAL,
                 BACKWARD_DIAGONAL,
                 VERTICAL_CROSS,
                 DIAGONAL_CROSS,
                 DARK_HORIZONTAL,
                 DARK_VERTICAL,
                 DARK_FORWARD_DIAGONAL,
                 DARK_BACKWARD_DIAGONAL,
                 DARK_VERTICAL_CROSS,
                 DARK_DIAGONAL_CROSS ]

    def __init__( self, shading=None, pattern=None, foreground=None, background=None ) :
        self.SetShading   ( shading    )
        self.SetForeground( foreground )
        self.SetBackground( background )
        self.SetPattern   ( pattern    )

    def __deepcopy__( self, memo ) :
        return ShadingPropertySet( self.Shading,
                                   self.Foreground,
                                   self.Background,
                                   self.Pattern )

    def SetShading( self, value ) :
        self.Shading = value
        return self

    def SetPattern( self, value ) :
        assert value is None or value in self.PATTERNS
        self.Pattern = value
        return self

    def SetForeground( self, value ) :
        assert not value or isinstance( value, Colour )
        self.Foreground = value
        return self

    def SetBackground( self, value ) :
        assert not value or isinstance( value, Colour )
        self.Background = value
        return self


class BorderPropertySet :
    SINGLE    = 1
    DOUBLE    = 2
    SHADOWED  = 3
    DOUBLED   = 4
    DOTTED    = 5
    DASHED    = 6
    HAIRLINE  = 7
    STYLES    = [ SINGLE, DOUBLE, SHADOWED, DOUBLED, DOTTED, DASHED, HAIRLINE ]

    def __init__( self, width=None, style=None, colour=None, spacing=None ) :
        self.SetWidth  ( width   )
        self.SetStyle  ( style or self.SINGLE )
        self.SetColour ( colour  )
        self.SetSpacing( spacing )

    def SetWidth( self, value ) :
        self.Width = value
        return self

    def SetStyle( self, value ) :
        assert value is None or value in self.STYLES
        self.Style = value
        return self

    def SetColour( self, value ) :
        assert value is None or isinstance( value, Colour )
        self.Colour = value
        return self

    def SetSpacing( self, value ) :
        self.Spacing = value
        return self

class FramePropertySet :
    def __init__( self, top=None, left=None, bottom=None, right=None ) :
        self.SetTop   ( top    )
        self.SetLeft  ( left   )
        self.SetBottom( bottom )
        self.SetRight ( right  )

    def SetTop( self, value ) :
        assert value is None or isinstance( value, BorderPropertySet )
        self.Top = value
        return self

    def SetLeft( self, value ) :
        assert value is None or isinstance( value, BorderPropertySet )
        self.Left = value
        return self

    def SetBottom( self, value ) :
        assert value is None or isinstance( value, BorderPropertySet )
        self.Bottom = value
        return self

    def SetRight( self, value ) :
        assert value is None or isinstance( value, BorderPropertySet )
        self.Right = value
        return self

class TabPropertySet :
    DEFAULT_WIDTH = 720

    LEFT      = 1
    RIGHT     = 2
    CENTER    = 3
    DECIMAL   = 4
    ALIGNMENT = [ LEFT, RIGHT, CENTER, DECIMAL ]

    DOTS        = 1
    HYPHENS     = 2
    UNDERLINE   = 3
    THICK_LINE  = 4
    EQUAL_SIGN  = 5
    LEADERS = [ DOTS, HYPHENS, UNDERLINE, THICK_LINE, EQUAL_SIGN ]

    def __init__( self, width=None, alignment=None, leader=None ) :
        self.SetWidth    ( width     )
        self.SetAlignment( alignment or self.LEFT )
        self.SetLeader   ( leader    )

    def SetWidth( self, value ) :
        self.Width = value
        return self

    def SetAlignment( self, value ) :
        assert value in self.ALIGNMENT
        self.Alignment = value
        return self

    def SetLeader( self, value ) :
        assert not value or value in self.LEADERS
        self.Leader = value
        return self

class TextPropertySet :

    def __init__( self, font=None, size=None, bold=None, italic=None, underline=None, colour=None, frame=None, expansion=None ) :
        self.SetFont         ( font )
        self.SetSize         ( size )

        self.SetBold         ( bold      or False )
        self.SetItalic       ( italic    or False )
        self.SetUnderline    ( underline or False )

        self.SetColour( colour )
        self.SetFrame ( frame  )

        self.SetStrikeThrough  ( False )
        self.SetDottedUnderline( False )
        self.SetDoubleUnderline( False )
        self.SetWordUnderline  ( False )
        self.SetExpansion      ( expansion )

    def Copy( self ) :
        return deepcopy( self )

    def __deepcopy__( self, memo ) :
        #   the font must remain a reference to the same font that we are looking at
        #   so we want to stop the recursiveness at this point and return an object
        #   with the right references.
        result = TextPropertySet( self.Font,
                                  self.Size,
                                  self.Bold,
                                  self.Italic,
                                  self.Underline,
                                  self.Colour,
                                  deepcopy( self.Frame, memo ) )
        result.SetStrikeThrough( self.StrikeThrough )
        return result

    def SetFont( self, value ) :
        assert not value or isinstance( value, Font )
        self.Font = value
        return self

    def SetSize( self, value ) :
        self.Size = value
        return self

    def SetBold( self, value ) :
        self.Bold = False
        if value : self.Bold = True
        return self

    def SetItalic( self, value ) :
        self.Italic = False
        if value : self.Italic = True
        return self

    def SetUnderline( self, value ) :
        self.Underline = False
        if value : self.Underline = True
        return self

    def SetColour( self, value ) :
        assert value is None or isinstance( value, Colour )
        self.Colour = value
        return self

    def SetFrame( self, value ) :
        assert value is None or isinstance( value, BorderPropertySet )
        self.Frame = value
        return self

    def SetStrikeThrough( self, value ) :
        self.StrikeThrough = False
        if value : self.StrikeThrough = True
        return self

    def SetDottedUnderline( self, value ) :
        self.DottedUnderline = False
        if value : self.DottedUnderline = True
        return self

    def SetDoubleUnderline( self, value ) :
        self.DoubleUnderline = False
        if value : self.DoubleUnderline = True
        return self

    def SetWordUnderline( self, value ) :
        self.WordUnderline = False
        if value : self.WordUnderline = True
        return self

    def SetExpansion( self, value ) :
        self.Expansion = value
        return self

class ParagraphPropertySet :
    LEFT       = 1
    RIGHT      = 2
    CENTER     = 3
    JUSTIFY    = 4
    DISTRIBUTE = 5
    ALIGNMENT  = [ LEFT, RIGHT, CENTER, JUSTIFY, DISTRIBUTE ]

    def __init__( self, alignment=None, space_before=None, space_after=None, tabs=None, first_line_indent=None, left_indent=None, right_indent=None, page_break_before=None ) :
        self.SetAlignment  ( alignment or self.LEFT )
        self.SetSpaceBefore( space_before )
        self.SetSpaceAfter ( space_after  )

        self.Tabs = []
        if tabs : apply( self.SetTabs, tabs )

        self.SetFirstLineIndent( first_line_indent or None )
        self.SetLeftIndent     ( left_indent or None )
        self.SetRightIndent    ( right_indent or None )

        self.SetPageBreakBefore( page_break_before )

        self.SetSpaceBetweenLines( None )

    def Copy( self ) :
        return deepcopy( self )

    def SetAlignment( self, value ) :
        assert not value or value in self.ALIGNMENT
        self.Alignment = value or self.LEFT
        return self

    def SetSpaceBefore( self, value ) :
        self.SpaceBefore = value
        return self

    def SetSpaceAfter( self, value ) :
        self.SpaceAfter = value
        return self

    def SetTabs( self, *params ) :
        self.Tabs = params
        return self

    def SetFirstLineIndent( self, value ) :
        self.FirstLineIndent = value
        return self

    def SetLeftIndent( self, value ) :
        self.LeftIndent = value
        return self

    def SetRightIndent( self, value ) :
        self.RightIndent = value
        return self

    def SetSpaceBetweenLines( self, value ) :
        self.SpaceBetweenLines = value
        return self

    def SetPageBreakBefore( self, value ) :
        self.PageBreakBefore = False
        if value : self.PageBreakBefore = True
        return self

#   Some short cuts to make the code a bit easier to read
MarginsPS   = MarginsPropertySet
ShadingPS   = ShadingPropertySet
BorderPS    = BorderPropertySet
FramePS     = FramePropertySet
TabPS       = TabPropertySet
TextPS      = TextPropertySet
ParagraphPS = ParagraphPropertySet

