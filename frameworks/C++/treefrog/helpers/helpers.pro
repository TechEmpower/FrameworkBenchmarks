TARGET = helper
TEMPLATE = lib
CONFIG += shared x86_64
QT  -= gui
QT  += 
DEFINES += TF_DLL
DESTDIR = ../lib
DEPENDPATH +=

include(../appbase.pri)

HEADERS += applicationhelper.h
SOURCES += applicationhelper.cpp
