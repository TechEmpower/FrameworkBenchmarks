#include "plaintexttest.h"

PlaintextTest::PlaintextTest(QObject *parent) : Controller(parent)
{

}

void PlaintextTest::plaintext(Context *c)
{
    Response *res = c->response();
    res->setBody("Hello, World!"_qba);
    res->setContentType("text/plain"_qba);
}
