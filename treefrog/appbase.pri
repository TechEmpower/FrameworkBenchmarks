win32 {
  INCLUDEPATH += $$quote($$(TFDIR)\\include)
  LIBS += -L$$quote($$(TFDIR)\\bin)
  CONFIG(debug, debug|release) {
    LIBS += -ltreefrogd1
  } else {
    LIBS += -ltreefrog1
  }
} else {
  macx {
    LIBS += -framework treefrog
  } else {
    LIBS += -ltreefrog
  }
  unix:INCLUDEPATH += /usr/include/treefrog
}
