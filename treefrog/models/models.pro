TARGET = model
TEMPLATE = lib
CONFIG += shared x86_64
QT += sql
QT -= gui
DEFINES += TF_DLL
DESTDIR = ../lib
INCLUDEPATH += ../helpers sqlobjects
DEPENDPATH  += ../helpers sqlobjects
LIBS += -L../lib -lhelper

include(../appbase.pri)

HEADERS += sqlobjects/fortuneobject.h
HEADERS += fortune.h
SOURCES += fortune.cpp
HEADERS += sqlobjects/worldobject.h
HEADERS += world.h
SOURCES += world.cpp
