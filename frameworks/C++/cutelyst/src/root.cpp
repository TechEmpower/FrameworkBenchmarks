#include "root.h"

using namespace Cutelyst;

Root::Root(QObject *parent) : Controller(parent)
{
}

Root::~Root()
{
}

void Root::End(Context *c)
{
    c->response()->headers().setDateWithDateTime(QDateTime::currentDateTimeUtc());
}

