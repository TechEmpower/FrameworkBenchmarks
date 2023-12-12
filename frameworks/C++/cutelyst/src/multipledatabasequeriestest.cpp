#include "multipledatabasequeriestest.h"

#include <Cutelyst/Plugins/Utils/Sql>

#include <apool.h>
#include <aresult.h>
#include <apreparedquery.h>

#include <QSqlQuery>

#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>

using namespace ASql;

MultipleDatabaseQueriesTest::MultipleDatabaseQueriesTest(QObject *parent) : Controller(parent)
{

}

void MultipleDatabaseQueriesTest::queriesp(Context *c)
{
    int queries = c->request()->queryParam(u"queries"_qs).toInt();
    if (queries < 1) {
        queries = 1;
    } else if (queries > 500) {
        queries = 500;
    }

    auto array = std::shared_ptr<QJsonArray>(new QJsonArray);
    ASync async(c);
    static thread_local auto db = APool::database();
    for (int i = 0; i < queries; ++i) {
        const int id = (rand() % 10000) + 1;

        db.exec(APreparedQueryLiteral(u8"SELECT id, randomNumber FROM world WHERE id=$1"),
                               {id}, c, [c, async, i, queries, array] (AResult &result) {
            if (Q_LIKELY(!result.error() && result.size())) {
                auto it = result.begin();
                array->append(QJsonObject{
                                  {u"id"_qs, it[0].toInt()},
                                  {u"randomNumber"_qs, it[1].toInt()}
                              });

                if (i + 1 == queries) {
                    c->response()->setJsonArrayBody(*array);
                }
                return;
            }

            c->res()->setStatus(Response::InternalServerError);
        });
    }
}

void MultipleDatabaseQueriesTest::query_postgres(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryThreadForDB(
                u"SELECT id, randomNumber FROM world WHERE id = :id"_qs,
                u"postgres"_qs);
    processQuery(c, query);
}

void MultipleDatabaseQueriesTest::query_mysql(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryThreadForDB(
                u"SELECT id, randomNumber FROM world WHERE id = :id"_qs,
                u"mysql"_qs);
    processQuery(c, query);
}

void MultipleDatabaseQueriesTest::processQuery(Context *c, QSqlQuery &query)
{
    QJsonArray array;

    int queries = c->request()->queryParam(u"queries"_qs).toInt();
    if (queries < 1) {
        queries = 1;
    } else if (queries > 500) {
        queries = 500;
    }

    for (int i = 0; i < queries; ++i) {
        const int id = (rand() % 10000) + 1;

        query.bindValue(u":id"_qs, id);
        if (Q_LIKELY(query.exec() && query.next())) {
            array.append(QJsonObject{
                             {u"id"_qs, query.value(0).toInt()},
                             {u"randomNumber"_qs, query.value(1).toInt()}
                         });
        } else {
            c->res()->setStatus(Response::InternalServerError);
            return;
        }
    }

    c->response()->setJsonArrayBody(array);
}
