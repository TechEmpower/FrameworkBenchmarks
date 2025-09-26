#include "databaseupdatestest.h"

#include <Cutelyst/Plugins/Utils/Sql>

#include <apool.h>
#include <aresult.h>
#include <apreparedquery.h>

#include <QSqlQuery>

#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>

#include <QLoggingCategory>

#include "picojson.h"

DatabaseUpdatesTest::DatabaseUpdatesTest(QObject *parent) : Controller(parent)
{

}

void DatabaseUpdatesTest::updatep(Context *c)
{
    int queries = c->request()->queryParam(QStringLiteral("queries"), QStringLiteral("1")).toInt();
    if (queries < 1) {
        queries = 1;
    } else if (queries > 500) {
        queries = 500;
    }

    picojson::array array;
    ASync async(c);
    static thread_local auto db = APool::database();
    for (int i = 0; i < queries; ++i) {
        int id = (qrand() % 10000) + 1;

        int randomNumber = (qrand() % 10000) + 1;

        array.emplace_back(picojson::object({
                            {"id", picojson::value(double(id))},
                            {"randomNumber", picojson::value(double(randomNumber))}
                        }));

        db.exec(APreparedQueryLiteral(u"SELECT randomNumber, id FROM world WHERE id=$1"),
                               {id}, [c, async] (AResult &result) {
            if (Q_UNLIKELY(result.error() || !result.size())) {
                c->res()->setStatus(Response::InternalServerError);
                return;
            }
        }, c);
        db.exec(APreparedQueryLiteral(u"UPDATE world SET randomNumber=$1 WHERE id=$2"),
                               {randomNumber, id}, [c, async] (AResult &result) {
            if (Q_UNLIKELY(result.error())) {
                c->res()->setStatus(Response::InternalServerError);
                return;
            }
        }, c);
    }

    c->response()->setJsonBody(QByteArray::fromStdString(picojson::value(array).serialize()));
}

void DatabaseUpdatesTest::updateb(Context *c)
{
    int queries = c->request()->queryParam(QStringLiteral("queries"), QStringLiteral("1")).toInt();
    if (queries < 1) {
        queries = 1;
    } else if (queries > 500) {
        queries = 500;
    }

    QVariantList args;
    QVariantList argsIds;

    picojson::array array;
    ASync async(c);
    static thread_local auto db = APool::database();
    for (int i = 0; i < queries; ++i) {
        int id = (qrand() % 10000) + 1;

        int randomNumber = (qrand() % 10000) + 1;

        argsIds.append(id);
        args.append(id);
        args.append(randomNumber);

        array.emplace_back(picojson::object({
                            {"id", picojson::value(double(id))},
                            {"randomNumber", picojson::value(double(randomNumber))}
                        }));

        db.exec(APreparedQueryLiteral(u"SELECT randomNumber, id FROM world WHERE id=$1"),
                               {id}, [c, async] (AResult &result) {
            if (Q_UNLIKELY(result.error() || !result.size())) {
                c->res()->setStatus(Response::InternalServerError);
                return;
            }
        }, c);
    }
    args.append(argsIds);

    const APreparedQuery pq = getSql(queries);
    db.exec(pq, args, [c, async] (AResult &result) {
        if (Q_UNLIKELY(result.error())) {
            c->res()->setStatus(Response::InternalServerError);
            return;
        }
    }, c);

    c->response()->setJsonBody(QByteArray::fromStdString(picojson::value(array).serialize()));
}

void DatabaseUpdatesTest::updates_postgres(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryThreadForDB(
                QLatin1String("SELECT id, randomNumber FROM world WHERE id = :id"),
                QStringLiteral("postgres"));
    QSqlQuery updateQuery = CPreparedSqlQueryThreadForDB(
                QLatin1String("UPDATE world SET randomNumber = :randomNumber WHERE id = :id"),
                QStringLiteral("postgres"));
    processQuery(c, query, updateQuery);
}

void DatabaseUpdatesTest::updates_mysql(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryThreadForDB(
                QLatin1String("SELECT randomNumber, id FROM world WHERE id = :id"),
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

APreparedQuery DatabaseUpdatesTest::getSql(int count)
{
    auto iter = m_sqlMap.find(count);
    if (iter != m_sqlMap.end())
    {
        return iter.value();
    }
    QString sql = QStringLiteral("UPDATE WORLD SET randomnumber=CASE id ");
    sql.reserve(80 + count * 25);
    int placeholdersCounter = 1;
    for (int i = 0; i < count; i++) {
        sql.append(QStringLiteral("WHEN $%1 THEN $%2 ").arg(placeholdersCounter).arg(placeholdersCounter + 1));
        placeholdersCounter += 2;
    }
    sql.append(QStringLiteral("ELSE randomnumber END WHERE id IN ("));

    for (int i = 0; i < count; i++) {
        sql.append(QLatin1Char('$') + QString::number(placeholdersCounter) + QLatin1Char(','));
        ++placeholdersCounter;
    }

    if (count) {
        sql.remove(sql.size() - 1, 1);
    }
    sql.append(QLatin1Char(')'));
    m_sqlMap.insert(count, sql);

    return sql;
}
