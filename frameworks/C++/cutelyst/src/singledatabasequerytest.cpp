#include "singledatabasequerytest.h"

#include <Cutelyst/Plugins/Utils/Sql>

#include <apool.h>
#include <aresult.h>
#include <apreparedquery.h>

#include <QSqlQuery>

#include <QJsonDocument>
#include <QJsonObject>

SingleDatabaseQueryTest::SingleDatabaseQueryTest(QObject *parent) : Controller(parent)
{

}

void SingleDatabaseQueryTest::dbp(Context *c)
{
    const int id = (qrand() % 10000) + 1;

    ASync async(c);
    static thread_local auto db = APool::database();
    db.exec(APreparedQueryLiteral(u"SELECT id, randomNumber FROM world WHERE id=$1"),
                           {id}, [c, async] (AResult &result) {
        if (Q_LIKELY(!result.error() && result.size())) {
            auto it = result.begin();
            c->response()->setJsonObjectBody({
                                                 {QStringLiteral("id"), it[0].toInt()},
                                                 {QStringLiteral("randomNumber"), it[1].toInt()}
                                             });
            return;
        }

        c->res()->setStatus(Response::InternalServerError);
    }, c);
}

void SingleDatabaseQueryTest::db_postgres(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryThreadForDB(
                QLatin1String("SELECT id, randomNumber FROM world WHERE id = :id"),
                QStringLiteral("postgres"));
    processQuery(c, query);
}

void SingleDatabaseQueryTest::db_mysql(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryThreadForDB(
                QLatin1String("SELECT id, randomNumber FROM world WHERE id = :id"),
                QStringLiteral("mysql"));
    processQuery(c, query);
}

void SingleDatabaseQueryTest::processQuery(Context *c, QSqlQuery &query)
{
    int id = (qrand() % 10000) + 1;

    query.bindValue(QStringLiteral(":id"), id);
    if (Q_UNLIKELY(!query.exec() || !query.next())) {
        c->res()->setStatus(Response::InternalServerError);
        return;
    }

    c->response()->setJsonObjectBody({
                                         {QStringLiteral("id"), query.value(0).toInt()},
                                         {QStringLiteral("randomNumber"), query.value(1).toInt()}
                                     });
}
