#include "root.h"

#include <QElapsedTimer>

using namespace Cutelyst;

Root::Root(QObject *parent) : Controller(parent)
{
}

Root::~Root()
{
}

QElapsedTimer timerSetup(Context *c)
{
    QElapsedTimer timer;
    timer.start();
    return timer;
}

QString setupHeader(Context *c)
{
    Headers &headers = c->response()->headers();
    headers.setDateWithDateTime(QDateTime::currentDateTimeUtc());
    return headers.header(QStringLiteral("date"));
}

void Root::End(Context *c)
{
    static QString lastDate = setupHeader(c);
    static QElapsedTimer timer = timerSetup(c);
    if (timer.hasExpired(1000)) {
        lastDate = setupHeader(c);
        timer.restart();
    }
}
