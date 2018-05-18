#include "multipledatabasequeriestest.h"

#include <Cutelyst/Plugins/Utils/Sql>

#include <QSqlQuery>

#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>

MultipleDatabaseQueriesTest::MultipleDatabaseQueriesTest(QObject *parent) : Controller(parent)
{

}

void MultipleDatabaseQueriesTest::query_postgres(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryThreadForDB(
                QLatin1String("SELECT id, randomNumber FROM world WHERE id = :id"),
                QStringLiteral("postgres"));
    processQuery(c, query);
}

void MultipleDatabaseQueriesTest::query_mysql(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryThreadForDB(
                QLatin1String("SELECT id, randomNumber FROM world WHERE id = :id"),
                QStringLiteral("mysql"));
    processQuery(c, query);
}

void MultipleDatabaseQueriesTest::processQuery(Context *c, QSqlQuery &query)
{
    QJsonArray array;

    int queries = c->request()->queryParam(QStringLiteral("queries"), QStringLiteral("1")).toInt();
    if (queries < 1) {
        queries = 1;
    } else if (queries > 500) {
        queries = 500;
    }

    for (int i = 0; i < queries; ++i) {
        int id = (qrand() % 10000) + 1;

        query.bindValue(QStringLiteral(":id"), id);
        if (Q_UNLIKELY(!query.exec() || !query.next())) {
            c->res()->setStatus(Response::InternalServerError);
            return;
        }

        array.append(QJsonObject{
                         {QStringLiteral("id"), query.value(0).toInt()},
                         {QStringLiteral("randomNumber"), query.value(1).toInt()}
                     });
    }

    c->response()->setJsonArrayBody(array);
}
