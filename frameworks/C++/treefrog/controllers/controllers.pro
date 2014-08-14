TARGET = controller
TEMPLATE = lib
CONFIG += shared x86_64
QT += network sql xml
QT -= gui
DEFINES += TF_DLL
DESTDIR = ../lib
INCLUDEPATH += ../helpers ../models
DEPENDPATH  += ../helpers ../models
LIBS += -L../lib -lhelper -lmodel

include(../appbase.pri)

HEADERS += applicationcontroller.h
SOURCES += applicationcontroller.cpp
HEADERS += fortunecontroller.h
SOURCES += fortunecontroller.cpp
HEADERS += worldcontroller.h
SOURCES += worldcontroller.cpp
HEADERS += jsoncontroller.h
SOURCES += jsoncontroller.cpp
