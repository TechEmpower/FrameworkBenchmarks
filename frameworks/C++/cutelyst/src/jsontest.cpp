#include "jsontest.h"

#include <QJsonDocument>
#include <QJsonObject>

JsonTest::JsonTest(QObject *parent) : Controller(parent)
{

}

void JsonTest::json(Context *c)
{
    c->response()->setJsonObjectBody({ {QStringLiteral("message"), QStringLiteral("Hello, World!")} });
}
