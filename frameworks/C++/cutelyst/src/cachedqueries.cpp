#include "cachedqueries.h"

#include <apool.h>
#include <aresult.h>
#include <apreparedquery.h>

#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>

#include <QCache>

using namespace ASql;

CachedQueries::CachedQueries(QObject *parent)
    : Controller{parent}
{

}

void CachedQueries::cached_queries(Context *c)
{
    int queries = c->request()->queryParam(QStringLiteral("count")).toInt();
    if (queries < 1) {
        queries = 1;
    } else if (queries > 500) {
        queries = 500;
    }

    static thread_local QCache<int, QJsonObject> cache(1024);

    auto array = std::shared_ptr<QJsonArray>(new QJsonArray);

    ASync async(c);
    static thread_local auto db = APool::database();
    for (int i = 0; i < queries; ++i) {
        const int id = (rand() % 10000) + 1;

        QJsonObject *obj = cache[id];
        if (obj) {
            array->append(*obj);
            continue;
        }

        db.exec(APreparedQueryLiteral(u8"SELECT id, randomNumber FROM world WHERE id=$1"),
                               {id}, c, [c, async, i, queries, array] (AResult &result) {
            if (Q_LIKELY(!result.error() && result.size())) {
                auto it = result.begin();
                int id = it[0].toInt();
                auto obj = new QJsonObject({
                                               {QStringLiteral("id"), id},
                                               {QStringLiteral("randomNumber"), it[1].toInt()}
                                           });
                array->append(*obj);
                cache.insert(id, obj, 1);

                if (array->size() == queries) {
                    c->response()->setJsonArrayBody(*array);
                }
                return;
            }

            c->res()->setStatus(Response::InternalServerError);
        });
    }

    if (array->size() == queries) {
        c->response()->setJsonArrayBody(*array);
    }
}
