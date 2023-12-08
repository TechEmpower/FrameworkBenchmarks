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

using namespace ASql;

DatabaseUpdatesTest::DatabaseUpdatesTest(QObject *parent) : Controller(parent)
{

}

void DatabaseUpdatesTest::updatep(Context *c)
{
    int queries = c->request()->queryParam(u"queries"_qs, u"1"_qs).toInt();
    if (queries < 1) {
        queries = 1;
    } else if (queries > 500) {
        queries = 500;
    }

    picojson::array array;
    ASync async(c);
    static thread_local auto db = APool::database();
    for (int i = 0; i < queries; ++i) {
        int id = (rand() % 10000) + 1;

        int randomNumber = (rand() % 10000) + 1;

        array.emplace_back(picojson::object({
                            {"id", picojson::value(double(id))},
                            {"randomNumber", picojson::value(double(randomNumber))}
                        }));

        db.exec(APreparedQueryLiteral(u8"SELECT randomNumber, id FROM world WHERE id=$1"),
                               {id}, c, [c, async] (AResult &result) {
            if (Q_UNLIKELY(result.error() || !result.size())) {
                c->res()->setStatus(Response::InternalServerError);
                return;
            }
        });
        db.exec(APreparedQueryLiteral(u8"UPDATE world SET randomNumber=$1 WHERE id=$2"),
                               {randomNumber, id}, c, [c, async] (AResult &result) {
            if (Q_UNLIKELY(result.error())) {
                c->res()->setStatus(Response::InternalServerError);
                return;
            }
        });
    }

    c->response()->setJsonBody(QByteArray::fromStdString(picojson::value(array).serialize()));
}

void DatabaseUpdatesTest::updateb(Context *c)
{
    int queries = c->request()->queryParam(u"queries"_qs, u"1"_qs).toInt();
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
        int id = (rand() % 10000) + 1;

        int randomNumber = (rand() % 10000) + 1;

        argsIds.append(id);
        args.append(id);
        args.append(randomNumber);

        array.emplace_back(picojson::object({
                            {"id", picojson::value(double(id))},
                            {"randomNumber", picojson::value(double(randomNumber))}
                        }));

        db.exec(APreparedQueryLiteral(u8"SELECT randomNumber, id FROM world WHERE id=$1"),
                               {id}, c, [c, async] (AResult &result) {
            if (Q_UNLIKELY(result.error() || !result.size())) {
                c->res()->setStatus(Response::InternalServerError);
                return;
            }
        });
    }
    args.append(argsIds);

    const APreparedQuery pq = getSql(queries);
    db.exec(pq, args, c, [c, async] (AResult &result) {
        if (Q_UNLIKELY(result.error())) {
            c->res()->setStatus(Response::InternalServerError);
            return;
        }
    });

    c->response()->setJsonBody(QByteArray::fromStdString(picojson::value(array).serialize()));
}

void DatabaseUpdatesTest::updates_postgres(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryThreadForDB(
                u"SELECT id, randomNumber FROM world WHERE id = :id"_qs,
                u"postgres"_qs);
    QSqlQuery updateQuery = CPreparedSqlQueryThreadForDB(
                u"UPDATE world SET randomNumber = :randomNumber WHERE id = :id"_qs,
                u"postgres"_qs);
    processQuery(c, query, updateQuery);
}

void DatabaseUpdatesTest::updates_mysql(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryThreadForDB(
                u"SELECT randomNumber, id FROM world WHERE id = :id"_qs,
                u"mysql"_qs);
    QSqlQuery updateQuery = CPreparedSqlQueryThreadForDB(
                u"UPDATE world SET randomNumber = :randomNumber WHERE id = :id"_qs,
                u"mysql"_qs);
    processQuery(c, query, updateQuery);
}

void DatabaseUpdatesTest::processQuery(Context *c, QSqlQuery &query, QSqlQuery &updateQuery)
{
    QJsonArray array;

    int queries = c->request()->queryParam(u"queries"_qs, u"1"_qs).toInt();
    if (queries < 1) {
        queries = 1;
    } else if (queries > 500) {
        queries = 500;
    }

    QVariantList ids, randomNumbers;
    ids.reserve(queries);
    randomNumbers.reserve(queries);
    for (int i = 0; i < queries; ++i) {
        int id = (rand() % 10000) + 1;

        query.bindValue(u":id"_qs, id);
        if (Q_UNLIKELY(!query.exec() || !query.next())) {
            c->res()->setStatus(Response::InternalServerError);
            return;
        }

        int randomNumber = (rand() % 10000) + 1;
        ids.append(id);
        randomNumbers.append(randomNumber);

        array.append(QJsonObject{
                         {u"id"_qs, id},
                         {u"randomNumber"_qs, randomNumber}
                     });
    }

    updateQuery.bindValue(u":id"_qs, ids);
    updateQuery.bindValue(u":randomNumber"_qs, randomNumbers);
    if (Q_LIKELY(updateQuery.execBatch())) {
        c->response()->setJsonArrayBody(array);
    } else {
        c->res()->setStatus(Response::InternalServerError);
    }
}

APreparedQuery DatabaseUpdatesTest::getSql(int count)
{
    auto iter = m_sqlMap.find(count);
    if (Q_UNLIKELY(iter == m_sqlMap.end())) {
        QString sql = u"UPDATE WORLD SET randomnumber=CASE id "_qs;
        sql.reserve(80 + count * 25);
        int placeholdersCounter = 1;
        for (int i = 0; i < count; i++) {
            sql.append(u"WHEN $%1 THEN $%2 "_qs.arg(placeholdersCounter).arg(placeholdersCounter + 1));
            placeholdersCounter += 2;
        }
        sql.append(u"ELSE randomnumber END WHERE id IN (");

        for (int i = 0; i < count; i++) {
            sql.append(u'$' + QString::number(placeholdersCounter) + u',');
            ++placeholdersCounter;
        }

        if (count) {
            sql.remove(sql.size() - 1, 1);
        }
        sql.append(u')');

        iter = m_sqlMap.insert(count, APreparedQuery(sql));
    }

    return iter.value();
}
