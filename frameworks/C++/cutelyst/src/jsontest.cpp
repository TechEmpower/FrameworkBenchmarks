#include "jsontest.h"

#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>

JsonTest::JsonTest(QObject *parent) : Controller(parent)
{

}

void JsonTest::json(Context *c)
{
    QJsonDocument doc;
    QJsonObject obj;
    obj.insert(QStringLiteral("message"), QStringLiteral("Hello, World!"));
    doc.setObject(obj);

    Response *res = c->response();
    res->body() = doc.toJson(QJsonDocument::Compact);
    res->setContentType(QStringLiteral("application/json"));
}
