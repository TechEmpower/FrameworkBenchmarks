class ViewKind :
    """An integer (0-5) that represents the view mode of the document."""

    NONE           = 0
    PageLayout     = 1
    Outline        = 2
    MasterDocument = 3
    Normal         = 4
    OnlineLayout   = 5

    DEFAULT = PageLayout

    def _IsValid( cls, value ) :
        return value in [ 0, 1, 2, 3, 4, 5 ]
    IsValid = classmethod( _IsValid )

class ViewScale :
    """Zoom level of the document; the N argument is a value representing a percentage (the default is 100)."""

    def _IsValid( cls, value ) :
        return value is None or (0 < value < 101)
    IsValid = classmethod( _IsValid )

class ViewZoomKind :
    """An integer (0 to 2) that represents the zoom kind of the document."""

    NONE     = 0
    FullPage = 1
    BestFit  = 2

    def _IsValid( cls, value ) :
        return value in [ None, 0, 1, 2 ]
    IsValid = classmethod( _IsValid )


class Languages :
    NoLanguage            = 1024
    Albanian              = 1052
    Arabic                = 1025
    Bahasa                = 1057
    BelgianDutch          = 2067
    BelgianFrench         = 2060
    BrazilianPortuguese   = 1046
    Bulgarian             = 1026
    Catalan               = 1027
    CroatoSerbianLatin    = 1050
    Czech                 = 1029
    Danish                = 1030
    Dutch                 = 1043
    EnglishAustralian     = 3081
    EnglishUK             = 2057
    EnglishUS             = 1033
    Finnish               = 1035
    French                = 1036
    FrenchCanadian        = 3084
    German                = 1031
    Greek                 = 1032
    Hebrew                = 1037
    Hungarian             = 1038
    Icelandic             = 1039
    Italian               = 1040
    Japanese              = 1041
    Korean                = 1042
    NorwegianBokmal       = 1044
    NorwegianNynorsk      = 2068
    Polish                = 1045
    Portuguese            = 2070
    RhaetoRomanic         = 1047
    Romanian              = 1048
    Russian               = 1049
    SerboCroatianCyrillic = 2074
    SimplifiedChinese     = 2052
    Slovak                = 1051
    SpanishCastilian      = 1034
    SpanishMexican        = 2058
    Swedish               = 1053
    SwissFrench           = 4108
    SwissGerman           = 2055
    SwissItalian          = 2064
    Thai                  = 1054
    TraditionalChinese    = 1028
    Turkish               = 1055
    Urdu                  = 1056
    SesothoSotho          = 1072
    Afrikaans             = 1078
    Zulu                  = 1077
    Xhosa                 = 1076
    Venda                 = 1075
    Tswana                = 1074
    Tsonga                = 1073
    FarsiPersian          = 1065

    Codes = [ 1024,
              1052,
              1025,
              1057,
              2067,
              2060,
              1046,
              1026,
              1027,
              1050,
              1029,
              1030,
              1043,
              3081,
              2057,
              1033,
              1035,
              1036,
              3084,
              1031,
              1032,
              1037,
              1038,
              1039,
              1040,
              1041,
              1042,
              1044,
              2068,
              1045,
              2070,
              1047,
              1048,
              1049,
              2074,
              2052,
              1051,
              1034,
              2058,
              1053,
              4108,
              2055,
              2064,
              1054,
              1028,
              1055,
              1056,
              1072,
              1078,
              1077,
              1076,
              1075,
              1074,
              1073,
              1065 ]

    #  make it Australian as that is what I use most of the time
    DEFAULT = EnglishAustralian

    def _IsValid( cls, value ) :
        return value in cls.Codes
    IsValid = classmethod( _IsValid )

if __name__ == '__main__' :
    PrintHexTable()

