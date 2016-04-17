#include "singledatabasequerytest.h"

#include <Cutelyst/Plugins/Utils/Sql/Sql>

#include <QtSql/QSqlQuery>

#include <QtCore/QThread>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>

SingleDatabaseQueryTest::SingleDatabaseQueryTest(QObject *parent) : Controller(parent)
{
    qsrand(QDateTime::currentMSecsSinceEpoch());
}

void SingleDatabaseQueryTest::db_postgres(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryForDatabase(
                QLatin1String("SELECT id, randomNumber FROM world WHERE id = :id"),
                QSqlDatabase::database(QLatin1String("postgres")));
    processQuery(c, query);
}

void SingleDatabaseQueryTest::db_mysql(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryForDatabase(
                QLatin1String("SELECT id, randomNumber FROM world WHERE id = :id"),
                QSqlDatabase::database(QLatin1String("mysql")));
    processQuery(c, query);
}

void SingleDatabaseQueryTest::processQuery(Context *c, QSqlQuery &query)
{
    int id = qrand() % 9999;

    query.bindValue(QStringLiteral(":id"), id + 1);
    if (!query.exec() || !query.next()) {
        c->res()->setStatus(Response::InternalServerError);
        return;
    }

    QJsonObject obj;
    obj.insert(QStringLiteral("id"), query.value(0).toInt());
    obj.insert(QStringLiteral("randomNumber"), query.value(1).toInt());



    Response *res = c->response();
    res->body() = QJsonDocument(obj).toJson(QJsonDocument::Compact);
    res->setContentType(QStringLiteral("application/json"));
}
