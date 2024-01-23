#include "jsontest.h"

#include "picojson.h"

#include <QJsonDocument>
#include <QJsonObject>

JsonTest::JsonTest(QObject *parent) : Controller(parent)
{

}

void JsonTest::json(Context *c)
{
    c->response()->setJsonObjectBody({ {u"message"_qs, u"Hello, World!"_qs} });
}

void JsonTest::pson(Context *c)
{
    c->response()->setJsonBody(QByteArray::fromStdString(
                                picojson::value(picojson::object{
                                                    {"message", picojson::value("Hello, World!")}
                                                }).serialize()));
}
