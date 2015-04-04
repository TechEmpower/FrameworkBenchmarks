"""
A Styles is a collection of PropertySets that can be applied to a particular RTF element.

At present there are only two, Text and Paragraph but ListStyles will be added soon too.


"""

from PropertySets import *

class TextStyle :
    def __init__( self, text_props, name=None, shading_props=None ) :
        self.SetTextPropertySet   ( text_props    )
        self.SetName              ( name          )
        self.SetShadingPropertySet( shading_props )

    def Copy( self ) :
        return deepcopy( self )

    def SetName( self, value ) :
        self.Name = value
        return self

    def SetTextPropertySet( self, value ) :
        assert isinstance( value, TextPropertySet )
        self.TextPropertySet = value
        return self

    def SetShadingPropertySet( self, value ) :
        assert value is None or isinstance( value, ShadingPropertySet )
        self.ShadingPropertySet = value or ShadingPropertySet()
        return self

class ParagraphStyle :
    def __init__( self, name, text_style, paragraph_props=None, frame_props=None, shading_props=None ) :

        #   A style must have Font and a Font Size but the Text property set doesn't
        #   make these mandatory so that they can be used for overrides so at this point
        #   we need to make sure that that we have these values set
        if not text_style.TextPropertySet.Font : raise Exception( 'Paragraph Styles must have a Font specified.'      )
        if not text_style.TextPropertySet.Size : raise Exception( 'Paragraph Styles must have a Font Size specified.' )

        self.SetName                ( name            )
        self.SetTextStyle           ( text_style      )
        self.SetParagraphPropertySet( paragraph_props )
        self.SetFramePropertySet    ( frame_props     )
        self.SetShadingPropertySet  ( shading_props   )

        self.SetBasedOn( None )
        self.SetNext   ( None )

    def Copy( self ) :
        return deepcopy( self )

    def SetName( self, value ) :
        self.Name = value
        return self

    def SetTextStyle( self, value ) :
        assert isinstance( value, TextStyle )
        self.TextStyle = value
        return self

    def SetParagraphPropertySet( self, value ) :
        assert value is None or isinstance( value, ParagraphPropertySet )
        self.ParagraphPropertySet = value or ParagraphPropertySet()
        return self

    def SetFramePropertySet( self, value ) :
        assert value is None or isinstance( value, FramePropertySet )
        self.FramePropertySet = value or FramePropertySet()
        return self

    def SetShadingPropertySet( self, value ) :
        """Set the background shading for the paragraph."""

        assert value is None or isinstance( value, ShadingPropertySet )
        self.ShadingPropertySet = value or ShadingPropertySet()
        return self

    def SetBasedOn( self, value ) :
        """Set the Paragraph Style that this one is based on."""

        assert not value or isinstance( value, ParagraphStyle )
        self.BasedOn = value
        return self

    def SetNext( self, value ) :
        """Set the Paragraph Style that should follow this one."""

        assert not value or isinstance( value, ParagraphStyle )
        self.Next    = value
        return self

