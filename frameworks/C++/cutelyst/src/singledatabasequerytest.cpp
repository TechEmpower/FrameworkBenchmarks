#include "singledatabasequerytest.h"

#include <Cutelyst/Plugins/Utils/Sql>

#include <QtSql/QSqlQuery>

#include <QtCore/QThread>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>

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
    if (!query.exec() || !query.next()) {
        c->res()->setStatus(Response::InternalServerError);
        return;
    }

    QJsonObject obj;
    obj.insert(QStringLiteral("id"), query.value(0).toInt());
    obj.insert(QStringLiteral("randomNumber"), query.value(1).toInt());

    c->response()->setJsonBody(QJsonDocument(obj));
}
