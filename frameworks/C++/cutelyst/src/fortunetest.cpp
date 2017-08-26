#include "fortunetest.h"

#include <Cutelyst/Plugins/Utils/Sql>

#include <QSqlQuery>

#include <QThread>

FortuneTest::FortuneTest(QObject *parent) : Controller(parent)
{

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

FortuneList FortuneTest::processQuery(Context *c, QSqlQuery &query)
{
    FortuneList fortunes;

    if (!query.exec()) {
        c->res()->setStatus(Response::InternalServerError);
        return fortunes;
    }

    while (query.next()) {
        fortunes.push_back({query.value(0).toInt(), query.value(1).toString()});
    }
    fortunes.push_back({0, QStringLiteral("Additional fortune added at request time.")});

    std::sort(fortunes.begin(), fortunes.end(), [] (const Fortune &a1, const Fortune &a2) {
        return a1.second < a2.second;
    });

    return fortunes;
}

void FortuneTest::renderRaw(Context *c, const FortuneList &fortunes)
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
                .append(QString::number(fortune.first))
                .append(QStringLiteral("</td><td>"))
                .append(fortune.second.toHtmlEscaped())
                .append(QStringLiteral("</td></tr>"));
    }

    out.append(QStringLiteral("</table></body></html>"));

    auto response = c->response();
    response->setBody(out);
    response->setContentType(QStringLiteral("text/html; charset=UTF-8"));
}
