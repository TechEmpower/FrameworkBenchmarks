#include "fortunetest.h"

#include <Cutelyst/Plugins/Utils/Sql>

#include <QStringBuilder>

#include <QtSql/QSqlQuery>

#include <QtCore/QThread>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>
#include <QtCore/QJsonArray>

FortuneTest::FortuneTest(QObject *parent) : Controller(parent)
{

}

void FortuneTest::fortunes_raw_postgres(Context *c)
{
    QSqlQuery query = postgresQuery();
    auto fortunes = processQuery(c, query);
    renderRaw(c, fortunes);
}

void FortuneTest::fortunes_raw_mysql(Context *c)
{
    QSqlQuery query = mysqlQuery();
    auto fortunes = processQuery(c, query);
    renderRaw(c, fortunes);
}

QSqlQuery FortuneTest::postgresQuery()
{
    return CPreparedSqlQueryForDatabase(
                QLatin1String("SELECT id, message FROM fortune"),
                QSqlDatabase::database(QLatin1String("postgres-") + QThread::currentThread()->objectName()));
}

QSqlQuery FortuneTest::mysqlQuery()
{
    return CPreparedSqlQueryForDatabase(
                QLatin1String("SELECT id, message FROM fortune"),
                QSqlDatabase::database(QLatin1String("mysql-") + QThread::currentThread()->objectName()));
}

static bool caseSensitiveLessThan(const Fortune &a1, const Fortune &a2)
{
    return a1.second < a2.second;
}

FortuneList FortuneTest::processQuery(Context *c, QSqlQuery &query)
{
    FortuneList fortunes;

    if (!query.exec()) {
        c->res()->setStatus(Response::InternalServerError);
        return fortunes;
    }

    while (query.next()) {
        fortunes.append(qMakePair(query.value(0).toInt(), query.value(1).toString()));
    }
    fortunes.append(qMakePair(0, QStringLiteral("Additional fortune added at request time.")));

    qSort(fortunes.begin(), fortunes.end(), caseSensitiveLessThan);

    c->response()->setContentType(QStringLiteral("text/html; charset=UTF-8"));

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

    Q_FOREACH (const Fortune &fortune, fortunes) {
        out.append(QLatin1String("<tr><td>") % QString::number(fortune.first) % QLatin1String("</td><td>") % fortune.second.toHtmlEscaped() % QLatin1String("</td></tr>"));
    }

    out.append(QStringLiteral("</table></body></html>"));

    c->response()->setBody(out);
}
