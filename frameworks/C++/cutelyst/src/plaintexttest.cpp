#include "plaintexttest.h"

PlaintextTest::PlaintextTest(QObject *parent) : Controller(parent)
{

}

void PlaintextTest::plaintext(Context *c)
{
    Response *res = c->response();
    res->setBody(QByteArrayLiteral("Hello, World!"));
    res->setContentType(QStringLiteral("text/plain"));
}
