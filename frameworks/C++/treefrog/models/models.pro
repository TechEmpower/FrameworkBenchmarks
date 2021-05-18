TARGET = model
TEMPLATE = lib
CONFIG += shared x86_64 c++14
QT += sql qml
QT -= gui
DEFINES += TF_DLL
DESTDIR = ../lib
INCLUDEPATH += ../helpers sqlobjects mongoobjects
DEPENDPATH  += ../helpers sqlobjects mongoobjects
LIBS += -L../lib -lhelper

include(../appbase.pri)

HEADERS += sqlobjects/fortuneobject.h
HEADERS += fortune.h
SOURCES += fortune.cpp
HEADERS += sqlobjects/worldobject.h
HEADERS += world.h
SOURCES += world.cpp
HEADERS += sqlobjects/pworldobject.h
HEADERS += pworld.h
SOURCES += pworld.cpp
HEADERS += mongoobjects/mngworldobject.h
HEADERS += mngworld.h
SOURCES += mngworld.cpp
HEADERS += mongoobjects/mngfortuneobject.h
HEADERS += mngfortune.h
SOURCES += mngfortune.cpp
