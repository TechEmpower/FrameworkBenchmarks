#include "databaseupdatestest.h"

#include <Cutelyst/Plugins/Utils/Sql>

#include <QSqlQuery>

#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>

DatabaseUpdatesTest::DatabaseUpdatesTest(QObject *parent) : Controller(parent)
{

}

void DatabaseUpdatesTest::updates_postgres(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryThreadForDB(
                QLatin1String("SELECT randomNumber FROM world WHERE id = :id"),
                QStringLiteral("postgres"));
    QSqlQuery updateQuery = CPreparedSqlQueryThreadForDB(
                QLatin1String("UPDATE world SET randomNumber = :randomNumber WHERE id = :id"),
                QStringLiteral("postgres"));
    processQuery(c, query, updateQuery);
}

void DatabaseUpdatesTest::updates_mysql(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryThreadForDB(
                QLatin1String("SELECT randomNumber FROM world WHERE id = :id"),
                QStringLiteral("mysql"));
    QSqlQuery updateQuery = CPreparedSqlQueryThreadForDB(
                QLatin1String("UPDATE world SET randomNumber = :randomNumber WHERE id = :id"),
                QStringLiteral("mysql"));
    processQuery(c, query, updateQuery);
}

void DatabaseUpdatesTest::processQuery(Context *c, QSqlQuery &query, QSqlQuery &updateQuery)
{
    QJsonArray array;

    int queries = c->request()->queryParam(QStringLiteral("queries"), QStringLiteral("1")).toInt();
    if (queries < 1) {
        queries = 1;
    } else if (queries > 500) {
        queries = 500;
    }

    QVariantList ids, randomNumbers;
    ids.reserve(queries);
    randomNumbers.reserve(queries);
    for (int i = 0; i < queries; ++i) {
        int id = (qrand() % 10000) + 1;

        query.bindValue(QStringLiteral(":id"), id);
        if (Q_UNLIKELY(!query.exec() || !query.next())) {
            c->res()->setStatus(Response::InternalServerError);
            return;
        }

        int randomNumber = (qrand() % 10000) + 1;
        ids.append(id);
        randomNumbers.append(randomNumber);

        array.append(QJsonObject{
                         {QStringLiteral("id"), id},
                         {QStringLiteral("randomNumber"), randomNumber}
                     });
    }

    updateQuery.bindValue(QStringLiteral(":id"), ids);
    updateQuery.bindValue(QStringLiteral(":randomNumber"), randomNumbers);
    if (Q_LIKELY(updateQuery.execBatch())) {
        c->response()->setJsonArrayBody(array);
    } else {
        c->res()->setStatus(Response::InternalServerError);
    }
}
