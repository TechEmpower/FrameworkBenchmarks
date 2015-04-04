import logging
import os

try:
    import Tkinter
except:
    Tkinter = None


class MessageBoxHandler(logging.Handler):
    def __init__(self):
        logging.Handler.__init__(self)

    def emit(self, record):
        if Tkinter:
            msg = self.format(record)
            root = Tkinter.Tk()
            root.wm_title("web2py logger message")
            text = Tkinter.Text()
            text["height"] = 12
            text.insert(0.1, msg)
            text.pack()
            button = Tkinter.Button(root, text="OK", command=root.destroy)
            button.pack()
            root.mainloop()


class NotifySendHandler(logging.Handler):
    def __init__(self):
        logging.Handler.__init__(self)

    def emit(self, record):
        if Tkinter:
            msg = self.format(record)
            os.system("notify-send '%s'" % msg)
