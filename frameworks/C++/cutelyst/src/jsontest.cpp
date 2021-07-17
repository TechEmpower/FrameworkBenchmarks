#include "jsontest.h"

#include "picojson.h"

#include <QJsonDocument>
#include <QJsonObject>

JsonTest::JsonTest(QObject *parent) : Controller(parent)
{

}

void JsonTest::json(Context *c)
{
    c->response()->setJsonObjectBody({ {QStringLiteral("message"), QStringLiteral("Hello, World!")} });
}

void JsonTest::pson(Context *c)
{
    c->response()->setJsonBody(QByteArray::fromStdString(
                                picojson::value(picojson::object{
                                                    {"message", picojson::value("Hello, World!")}
                                                }).serialize()));
}
