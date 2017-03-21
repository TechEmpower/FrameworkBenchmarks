#include "jsontest.h"

#include <QJsonDocument>
#include <QJsonObject>

JsonTest::JsonTest(QObject *parent) : Controller(parent)
{

}

void JsonTest::json(Context *c)
{
    QJsonObject obj;
    obj.insert(QStringLiteral("message"), QStringLiteral("Hello, World!"));

    c->response()->setJsonBody(QJsonDocument(obj));
}
