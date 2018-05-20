#include "singledatabasequerytest.h"

#include <Cutelyst/Plugins/Utils/Sql>

#include <QSqlQuery>

#include <QJsonDocument>
#include <QJsonObject>

SingleDatabaseQueryTest::SingleDatabaseQueryTest(QObject *parent) : Controller(parent)
{

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
