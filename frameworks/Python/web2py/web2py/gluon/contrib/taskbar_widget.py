#!/usr/bin/env python
# -*- coding: utf-8 -*-
# # Creates a taskbar icon for web2py
# # Author: Mark Larsen, mostly stolen from Mark Hammond's
# #   C:\Python25\Lib\site-packages\win32\Demos\win32gui_taskbar.py
# # 11/7/08
# dual licensed under the web2py license (LGPL) and the Python license.

import os
import sys
import base64
import win32con
import win32api
import win32gui


class TaskBarIcon:

    def __init__(self, iconPath=None):

        self.iconPath = iconPath
        self.status = []

        msg_TaskbarRestart = \
            win32api.RegisterWindowMessage('TaskbarCreated')
        message_map = {
            msg_TaskbarRestart: self.OnRestart,
            win32con.WM_DESTROY: self.OnDestroy,
            win32con.WM_COMMAND: self.OnCommand,
            win32con.WM_USER + 20: self.OnTaskbarNotify,
        }

        # Register the Window class.

        wc = win32gui.WNDCLASS()
        hinst = wc.hInstance = win32api.GetModuleHandle(None)
        wc.lpszClassName = 'web2pyTaskbar'
        wc.style = win32con.CS_VREDRAW | win32con.CS_HREDRAW
        wc.hCursor = win32gui.LoadCursor(0, win32con.IDC_ARROW)
        wc.hbrBackground = win32con.COLOR_WINDOW
        wc.lpfnWndProc = message_map  # could also specify a wndproc.
        classAtom = win32gui.RegisterClass(wc)

        # Create the Window.

        style = win32con.WS_OVERLAPPED | win32con.WS_SYSMENU
        self.hwnd = win32gui.CreateWindow(
            classAtom,
            'web2pyTaskbar',
            style,
            0,
            0,
            win32con.CW_USEDEFAULT,
            win32con.CW_USEDEFAULT,
            0,
            0,
            hinst,
            None,
        )
        win32gui.UpdateWindow(self.hwnd)
        self.SetServerStopped()

    def __createIcon(self):

        # try and use custom icon

        if self.iconPath and os.path.isfile(self.iconPath):
            hicon = self.__loadFromFile(self.iconPath)
        else:
            try:
                fp = 'tmp.ico'
                icFH = file(fp, 'wb')
                if self.serverState == self.EnumServerState.STOPPED:
                    icFH.write(base64.b64decode(self.__getIconStopped()))
                elif self.serverState == self.EnumServerState.RUNNING:
                    icFH.write(base64.b64decode(self.__getIconRunning()))
                icFH.close()
                hicon = self.__loadFromFile(fp)
                os.unlink(fp)
            except:
                print "Can't load web2py icons - using default"
                hicon = win32gui.LoadIcon(0, win32con.IDI_APPLICATION)

        flags = win32gui.NIF_ICON | win32gui.NIF_MESSAGE\
            | win32gui.NIF_TIP
        nid = (
            self.hwnd,
            0,
            flags,
            win32con.WM_USER + 20,
            hicon,
            'web2py Framework',
        )
        try:
            win32gui.Shell_NotifyIcon(win32gui.NIM_MODIFY, nid)
        except:
            try:
                win32gui.Shell_NotifyIcon(win32gui.NIM_ADD, nid)
            except win32api.error:

                # This is common when windows is starting, and this code is hit
                # before the taskbar has been created.

                print 'Failed to add the taskbar icon - is explorer running?'

                # but keep running anyway - when explorer starts, we get the

    def OnRestart(
        self,
        hwnd,
        msg,
        wparam,
        lparam,
    ):
        self._DoCreateIcons()

    def OnDestroy(
        self,
        hwnd,
        msg,
        wparam,
        lparam,
    ):
        nid = (self.hwnd, 0)
        win32gui.Shell_NotifyIcon(win32gui.NIM_DELETE, nid)

    def OnTaskbarNotify(
        self,
        hwnd,
        msg,
        wparam,
        lparam,
    ):
        if lparam == win32con.WM_LBUTTONUP:
            pass
        elif lparam == win32con.WM_LBUTTONDBLCLK:
            pass
        elif lparam == win32con.WM_RBUTTONUP:
            menu = win32gui.CreatePopupMenu()
            win32gui.AppendMenu(menu, win32con.MF_STRING, 1023,
                                'Toggle Display')
            win32gui.AppendMenu(menu, win32con.MF_SEPARATOR, 0, '')
            if self.serverState == self.EnumServerState.STOPPED:
                win32gui.AppendMenu(menu, win32con.MF_STRING, 1024,
                                    'Start Server')
                win32gui.AppendMenu(menu, win32con.MF_STRING
                                    | win32con.MF_GRAYED, 1025,
                                    'Restart Server')
                win32gui.AppendMenu(menu, win32con.MF_STRING
                                    | win32con.MF_GRAYED, 1026,
                                    'Stop Server')
            else:
                win32gui.AppendMenu(menu, win32con.MF_STRING
                                    | win32con.MF_GRAYED, 1024,
                                    'Start Server')
                win32gui.AppendMenu(menu, win32con.MF_STRING, 1025,
                                    'Restart Server')
                win32gui.AppendMenu(menu, win32con.MF_STRING, 1026,
                                    'Stop Server')
            win32gui.AppendMenu(menu, win32con.MF_SEPARATOR, 0, '')
            win32gui.AppendMenu(menu, win32con.MF_STRING, 1027,
                                'Quit (pid:%i)' % os.getpid())
            pos = win32gui.GetCursorPos()

            # See http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winui/menus_0hdi.asp

            win32gui.SetForegroundWindow(self.hwnd)
            win32gui.TrackPopupMenu(
                menu,
                win32con.TPM_LEFTALIGN,
                pos[0],
                pos[1],
                0,
                self.hwnd,
                None,
            )
            win32api.PostMessage(self.hwnd, win32con.WM_NULL, 0, 0)
        return 1

    def OnCommand(
        self,
        hwnd,
        msg,
        wparam,
        lparam,
    ):
        id = win32api.LOWORD(wparam)
        if id == 1023:
            self.status.append(self.EnumStatus.TOGGLE)
        elif id == 1024:
            self.status.append(self.EnumStatus.START)
        elif id == 1025:
            self.status.append(self.EnumStatus.RESTART)
        elif id == 1026:
            self.status.append(self.EnumStatus.STOP)
        elif id == 1027:
            self.status.append(self.EnumStatus.QUIT)
            self.Destroy()
        else:
            print 'Unknown command -', id

    def Destroy(self):
        win32gui.DestroyWindow(self.hwnd)

    def SetServerRunning(self):
        self.serverState = self.EnumServerState.RUNNING
        self.__createIcon()

    def SetServerStopped(self):
        self.serverState = self.EnumServerState.STOPPED
        self.__createIcon()

    def __getIconRunning(self):
        return 'AAABAAEAEBAQAAAAAAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAAAAAAgAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAIXMGAABe/wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABERAgAAIAAAEAACAAAgAAABEAIiACIgAAABAgAgIAIAEAECACAgAgABEAIiACACAAAAAAAAAAAAICACIiAiIAICAgIAACACAgICAgAAIAICAgICIiAiIAICAgIAACACAgICAgAAIAICAgICIiAiIAAAAAAAAAAAD//wAAhe8AAL3vAADMYwAA9a0AALWtAADMbQAA//8AAKwjAABV7QAAVe0AAFQjAABV7QAAVe0AAFQjAAD//wAA'

    def __getIconStopped(self):
        return 'AAABAAEAEBAQAAEABAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAJCdIAIXMGAABe/wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAzMzMzMzMzAwERMjMzIzAzEDMyMzMjAzMxAzIiMyAjMzMwMjMjAzIzEzECMyAjMjMxEzAiAyMyMzMzMwAzMzMzIyMyACMiIzIyMjAzAyMyMjIyAjMwIzIyMjAyIiMCIzIyAjIzMyAyMjAyMjMzIwIyAjIyIiMiIDAzMzMzMzMzB//gAAhe0AAJ3rAADMYwAA9a0AALGNAADMLQAA/n8AAKwjAABVrQAAUc0AAFQjAABF5QAAVekAABQhAAB//gAA'

    def __loadFromFile(self, iconPath):
        hinst = win32api.GetModuleHandle(None)
        icon_flags = win32con.LR_LOADFROMFILE | win32con.LR_DEFAULTSIZE
        hicon = win32gui.LoadImage(
            hinst,
            iconPath,
            win32con.IMAGE_ICON,
            0,
            0,
            icon_flags,
        )
        return hicon

    class EnumStatus:

        TOGGLE = 0
        START = 1
        STOP = 2
        RESTART = 3
        QUIT = 4

    class EnumServerState:

        RUNNING = 0
        STOPPED = 1
