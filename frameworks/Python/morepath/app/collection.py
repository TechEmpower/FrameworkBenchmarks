from .model import Fortune


class FortuneCollection(object):
    def query(self):
        return Fortune.select()
