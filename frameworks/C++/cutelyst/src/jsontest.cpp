#include "jsontest.h"

#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>

JsonTest::JsonTest(QObject *parent) : Controller(parent)
{

}

void JsonTest::json(Context *c)
{
    QJsonObject obj;
    obj.insert(QStringLiteral("message"), QStringLiteral("Hello, World!"));

    Response *res = c->response();
    res->body() = QJsonDocument(obj).toJson(QJsonDocument::Compact);
    res->setContentType(QStringLiteral("application/json"));
}
