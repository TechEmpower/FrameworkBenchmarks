# Only Python 2.6 and up, because of NamedTuple.
import time
from collections import namedtuple
Score = namedtuple('Score', ['tag', 'stamp'])


class TimeCollector(object):
    def __init__(self):
        '''The first time stamp is created here'''
        self.scores = [Score(tag='start', stamp=time.clock())]

    def addStamp(self, description):
        '''Adds a new time stamp, with a description.'''
        self.scores.append(Score(tag=description, stamp=time.clock()))

    def _stampDelta(self, index1, index2):
        '''Private utility function to clean up this common calculation.'''
        return self.scores[index1].stamp - self.scores[index2].stamp

    def getReportItems(self, orderByCost=True):
        '''Returns a list of dicts. Each dict has
            start (ms),
            end (ms),
            delta (ms),
            perc (%),
            tag (str)
        '''
        self.scores.append(Score(tag='finish', stamp=time.clock()))
        total_time = self._stampDelta(-1, 0)
        data = []
        for i in range(1, len(self.scores)):
            delta = self._stampDelta(i, i - 1)
            if abs(total_time) < 1e-6:
                perc = 0
            else:
                perc = delta / total_time * 100
            data.append(
                dict(
                    start=self._stampDelta(i - 1, 0) * 1000,
                    end=self._stampDelta(i, 0) * 1000,
                    delta=delta * 1000,
                    perc=perc,
                    tag=self.scores[i].tag
                )
            )
        if orderByCost:
            data.sort(key=lambda x: x['perc'], reverse=True)
        return data

    def getReportLines(self, orderByCost=True):
        '''Produces a report of logged time-stamps as a list of strings.
        if orderByCost is False, then the order of the stamps is
        chronological.'''
        data = self.getReportItems(orderByCost)
        headerTemplate = '%10s | %10s | %10s | %11s | %-30s'
        headerData = ('Start(ms)', 'End(ms)', 'Delta(ms)', 'Time Cost',
                      'Description')
        bodyTemplate = '%(start)10.0f | %(end)10.0f | %(delta)10.0f |' \
            + ' %(perc)10.0f%% | %(tag)-30s'
        return [headerTemplate % headerData] + [bodyTemplate % d for d in data]

    def getReportText(self, **kwargs):
        return '\n'.join(self.getReportLines(**kwargs))

    def restart(self):
        self.scores = [Score(tag='start', stamp=time.clock())]

if __name__ == '__main__':
    print('')
    print('Testing:')
    print('')

    # First create the collector
    t = TimeCollector()
    x = [i for i in range(1000)]
    # Every time some work gets done, add a stamp
    t.addStamp('Initialization Section')
    x = [i for i in range(10000)]
    t.addStamp('A big loop')
    x = [i for i in range(100000)]
    t.addStamp('calling builder function')
    # Finally, obtain the results
    print('')
    print(t.getReportText())
    # If you want to measure something else in the same scope, you can
    # restart the collector.
    t.restart()
    x = [i for i in range(1000000)]
    t.addStamp('Part 2')
    x = [i for i in range(1000000)]
    t.addStamp('Cleanup')
    # And once again report results
    print('')
    print(t.getReportText())
    t.restart()
    for y in range(1, 200, 20):
        x = [i for i in range(10000) * y]
        t.addStamp('Iteration when y = ' + str(y))

    print('')
    # You can turn off ordering of results
    print(t.getReportText(orderByCost=False))
