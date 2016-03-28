#include "root.h"

using namespace Cutelyst;

Root::Root(QObject *parent) : Controller(parent)
{
}

Root::~Root()
{
}

void Root::defaultPage(Context *c)
{
    c->response()->body() = "Page not found!";
    c->response()->setStatus(404);
}

void Root::End(Context *c)
{
    c->response()->headers().setDateWithDateTime(QDateTime::currentDateTimeUtc());
}

