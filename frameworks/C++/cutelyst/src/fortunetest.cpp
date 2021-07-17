#include "fortunetest.h"

#include <Cutelyst/Plugins/Utils/Sql>
#include <Cutelyst/View>

#include <apool.h>
#include <aresult.h>
#include <apreparedquery.h>

#include <QSqlQuery>

FortuneTest::FortuneTest(QObject *parent) : Controller(parent)
{

}

void FortuneTest::fortunes_raw_p(Context *c)
{
    ASync async(c);
    static thread_local auto db = APool::database();
    db.exec(APreparedQueryLiteral(u"SELECT id, message FROM fortune"), [c, async, this] (AResult &result) {
        if (Q_UNLIKELY(result.error() && !result.size())) {
            c->res()->setStatus(Response::InternalServerError);
            return;
        }

        FortuneList fortunes;
        fortunes.reserve(result.size());
        auto it = result.begin();
        while (it != result.end()) {
            fortunes.push_back({it[0].toInt(), it[1].toString()});
            ++it;
        }
        fortunes.push_back({0, QStringLiteral("Additional fortune added at request time.")});

        std::sort(fortunes.begin(), fortunes.end(), [] (const Fortune &a1, const Fortune &a2) {
            return a1.message < a2.message;
        });

        renderRaw(c, fortunes);
    }, c);
}

void FortuneTest::fortunes_raw_postgres(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryThreadForDB(
                QLatin1String("SELECT id, message FROM fortune"),
                QStringLiteral("postgres"));
    auto fortunes = processQuery(c, query);
    renderRaw(c, fortunes);
}

void FortuneTest::fortunes_raw_mysql(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryThreadForDB(
                QLatin1String("SELECT id, message FROM fortune"),
                QStringLiteral("mysql"));
    auto fortunes = processQuery(c, query);
    renderRaw(c, fortunes);
}

void FortuneTest::fortunes_c_p(Context *c)
{
    ASync async(c);
    static thread_local auto db = APool::database();
    db.exec(APreparedQueryLiteral(u"SELECT id, message FROM fortune"), [c, async] (AResult &result) {
        if (Q_UNLIKELY(result.error() && !result.size())) {
            c->res()->setStatus(Response::InternalServerError);
            return;
        }

        QVariantList fortunes;
        fortunes.reserve(result.size());
        auto it = result.begin();
        while (it != result.end()) {
            fortunes.append(QVariant::fromValue(QVariantList{
                                                    {it[0].toInt(), it[1].toString()},
                                                }));
            ++it;
        }

        fortunes.append(QVariant::fromValue(QVariantList{
                            {0, QStringLiteral("Additional fortune added at request time.")},
                        }));
        std::sort(fortunes.begin(), fortunes.end(), [] (const QVariant &a1, const QVariant &a2) {
            return a1.toList()[1].toString() < a2.toList()[1].toString();
        });

        c->setStash(QStringLiteral("template"), QStringLiteral("fortunes.html"));
        c->setStash(QStringLiteral("fortunes"), fortunes);
        static thread_local View *view = c->view();
        view->execute(c);
        c->response()->setContentType(QStringLiteral("text/html; charset=UTF-8"));
    }, c);
}

void FortuneTest::fortunes_cutelee_postgres(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryThreadForDB(
                QLatin1String("SELECT id, message FROM fortune"),
                QStringLiteral("postgres"));
    if (query.exec()) {
        QVariantList fortunes = Sql::queryToList(query);
        fortunes.append(QVariant::fromValue(QVariantList{
                            {0, QStringLiteral("Additional fortune added at request time.")},
                        }));
        std::sort(fortunes.begin(), fortunes.end(), [] (const QVariant &a1, const QVariant &a2) {
            return a1.toList()[1].toString() < a2.toList()[1].toString();
        });
        c->setStash(QStringLiteral("template"), QStringLiteral("fortunes.html"));
        c->setStash(QStringLiteral("fortunes"), fortunes);
        static thread_local View *view = c->view();
        view->execute(c);
        c->response()->setContentType(QStringLiteral("text/html; charset=UTF-8"));
    }
}

void FortuneTest::fortunes_cutelee_mysql(Context *c)
{
    QSqlQuery query = CPreparedSqlQueryThreadForDB(
                QLatin1String("SELECT id, message FROM fortune"),
                QStringLiteral("mysql"));
    if (query.exec()) {
        QVariantList fortunes = Sql::queryToList(query);
        fortunes.append(QVariant::fromValue(QVariantList{
                            {0, QStringLiteral("Additional fortune added at request time.")},
                        }));
        std::sort(fortunes.begin(), fortunes.end(), [] (const QVariant &a1, const QVariant &a2) {
            return a1.toList()[1].toString() < a2.toList()[1].toString();
        });
        c->setStash(QStringLiteral("template"), QStringLiteral("fortunes.html"));
        c->setStash(QStringLiteral("fortunes"), fortunes);
        static thread_local View *view = c->view();
        view->execute(c);
        c->response()->setContentType(QStringLiteral("text/html; charset=UTF-8"));
    }
}

FortuneList FortuneTest::processQuery(Context *c, QSqlQuery &query)
{
    FortuneList fortunes;

    if (Q_UNLIKELY(!query.exec())) {
        c->res()->setStatus(Response::InternalServerError);
        return fortunes;
    }

    fortunes.reserve(query.size());
    while (query.next()) {
        fortunes.push_back({query.value(0).toInt(), query.value(1).toString()});
    }
    fortunes.push_back({0, QStringLiteral("Additional fortune added at request time.")});

    std::sort(fortunes.begin(), fortunes.end(), [] (const Fortune &a1, const Fortune &a2) {
        return a1.message < a2.message;
    });

    return fortunes;
}

void FortuneTest::renderRaw(Context *c, const FortuneList &fortunes) const
{
    QString out;
    out.append(QStringLiteral("<!DOCTYPE html>"
                              "<html>"
                              "<head><title>Fortunes</title></head>"
                              "<body>"
                              "<table>"
                              "<tr><th>id</th><th>message</th></tr>"));
    out.reserve(4096);

    for (const Fortune &fortune : fortunes) {
        out.append(QStringLiteral("<tr><td>"))
                .append(QString::number(fortune.id))
                .append(QStringLiteral("</td><td>"))
                .append(fortune.message.toHtmlEscaped())
                .append(QStringLiteral("</td></tr>"));
    }

    out.append(QStringLiteral("</table></body></html>"));

    auto response = c->response();
    response->setBody(out);
    response->setContentType(QStringLiteral("text/html; charset=UTF-8"));
}
