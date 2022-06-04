#include "multipledatabasequeriestest.h"

#include <Cutelyst/Plugins/Utils/Sql>

#include <apool.h>
#include <aresult.h>
#include <apreparedquery.h>

#include <QSqlQuery>

#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>

MultipleDatabaseQueriesTest::MultipleDatabaseQueriesTest(QObject *parent) : Controller(parent)
{

}

void MultipleDatabaseQueriesTest::queriesp(Context *c)
{
    int queries = c->request()->queryParam(QStringLiteral("queries")).toInt();
    if (queries < 1) {
        queries = 1;
    } else if (queries > 500) {
        queries = 500;
    }

    auto array = std::shared_ptr<QJsonArray>(new QJsonArray);
    ASync async(c);
    static thread_local auto db = APool::database();
    for (int i = 0; i < queries; ++i) {
        const int id = (qrand() % 10000) + 1;

        db.exec(APreparedQueryLiteral(u"SELECT id, randomNumber FROM world WHERE id=$1"),
                               {id}, [c, async, i, queries, array] (AResult &result) {
            if (Q_LIKELY(!result.error() && result.size())) {
                auto it = result.begin();
                array->append(QJsonObject{
                                  {QStringLiteral("id"), it[0].toInt()},
                                  {QStringLiteral("randomNumber"), it[1].toInt()}
                              });

                if (i + 1 == queries) {
                    c->response()->setJsonArrayBody(*array);
                }
                return;
            }

            c->res()->setStatus(Response::InternalServerError);
        }, c);
    }
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

    int queries = c->request()->queryParam(QStringLiteral("queries")).toInt();
    if (queries < 1) {
        queries = 1;
    } else if (queries > 500) {
        queries = 500;
    }

    for (int i = 0; i < queries; ++i) {
        const int id = (qrand() % 10000) + 1;

        query.bindValue(QStringLiteral(":id"), id);
        if (Q_LIKELY(query.exec() && query.next())) {
            array.append(QJsonObject{
                             {QStringLiteral("id"), query.value(0).toInt()},
                             {QStringLiteral("randomNumber"), query.value(1).toInt()}
                         });
        } else {
            c->res()->setStatus(Response::InternalServerError);
            return;
        }
    }

    c->response()->setJsonArrayBody(array);
}
