#include "multipledatabasequeriestest.h"

#include <Cutelyst/Plugins/Utils/Sql>

#include <QtSql/QSqlQuery>

#include <QtCore/QThread>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>
#include <QtCore/QJsonArray>

MultipleDatabaseQueriesTest::MultipleDatabaseQueriesTest(QObject *parent) : Controller(parent)
{

}

void MultipleDatabaseQueriesTest::query_postgres(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryForDatabase(
                QLatin1String("SELECT id, randomNumber FROM world WHERE id = :id"),
                QSqlDatabase::database(QLatin1String("postgres")));
    processQuery(c, query);
}

void MultipleDatabaseQueriesTest::query_mysql(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryForDatabase(
                QLatin1String("SELECT id, randomNumber FROM world WHERE id = :id"),
                QSqlDatabase::database(QLatin1String("mysql")));
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
        int id = qrand() % 9999;

        query.bindValue(QStringLiteral(":id"), id + 1);
        if (!query.exec() || !query.next()) {
            c->res()->setStatus(Response::InternalServerError);
            return;
        }

        QJsonObject obj;
        obj.insert(QStringLiteral("id"), query.value(0).toInt());
        obj.insert(QStringLiteral("randomNumber"), query.value(1).toInt());
        array.append(obj);
    }

    c->response()->setJsonBody(QJsonDocument(array));
}
