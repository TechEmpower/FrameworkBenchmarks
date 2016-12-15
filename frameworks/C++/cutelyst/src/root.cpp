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
    return c->response()->headers().setDateWithDateTime(QDateTime::currentDateTimeUtc());
}

void Root::End(Context *c)
{
    static thread_local QString lastDate = setupHeader(c);
    static thread_local QElapsedTimer timer = timerSetup(c);
    if (timer.hasExpired(1000)) {
        lastDate = setupHeader(c);
        timer.restart();
    } else {
        c->response()->setHeader(QStringLiteral("date"), lastDate);
    }
}
