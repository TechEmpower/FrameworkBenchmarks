#include "cutelyst-benchmarks.h"

#include <Cutelyst/Plugins/StaticSimple/staticsimple.h>

#include <QtSql/QSqlDatabase>
#include <QtSql/QSqlError>
#include <QDebug>

#include "root.h"
#include "jsontest.h"
#include "singledatabasequerytest.h"
#include "multipledatabasequeriestest.h"
#include "databaseupdatestest.h"
#include "fortunetest.h"
#include "plaintexttest.h"

using namespace Cutelyst;

cutelyst_benchmarks::cutelyst_benchmarks(QObject *parent) : Application(parent)
{
}

cutelyst_benchmarks::~cutelyst_benchmarks()
{
}

bool cutelyst_benchmarks::init()
{
    if (config(QLatin1String("SendDate")).value<bool>()) {
        qDebug() << "Manually send date";
        new Root(this);
    }

    new JsonTest(this);
    new SingleDatabaseQueryTest(this);
    new MultipleDatabaseQueriesTest(this);
    new DatabaseUpdatesTest(this);
    new FortuneTest(this);
    new PlaintextTest(this);

    defaultHeaders().setServer(QLatin1String("Cutelyst"));
    defaultHeaders().removeHeader(QLatin1String("X-Cutelyst"));

    return true;
}

bool cutelyst_benchmarks::postFork()
{
    QSqlDatabase db;
    db = QSqlDatabase::addDatabase(QLatin1String("QPSQL"), QLatin1String("postgres"));
    db.setDatabaseName(QLatin1String("hello_world"));
    db.setUserName(QLatin1String("benchmarkdbuser"));
    db.setPassword(QLatin1String("benchmarkdbpass"));
    db.setHostName(config(QLatin1String("DatabaseHostName")).toString());
    if (!db.open()) {
        qDebug() << "Error opening db:" << db << db.lastError().databaseText();
        return false;
    }

    db = QSqlDatabase::addDatabase(QLatin1String("QMYSQL"), QLatin1String("mysql"));
    db.setDatabaseName(QLatin1String("hello_world"));
    db.setUserName(QLatin1String("benchmarkdbuser"));
    db.setPassword(QLatin1String("benchmarkdbpass"));
    db.setHostName(config(QLatin1String("DatabaseHostName")).toString());
    if (!db.open()) {
        qDebug() << "Error opening db:" << db << db.lastError().databaseText();
        return false;
    }

//    db = QSqlDatabase::addDatabase(QLatin1String("QSQLITE"), QLatin1String("sqlite"));
//    if (!db.open()) {
//        qDebug() << "Error opening db:" << db << db.lastError().databaseText();
//        return false;
//    }

    return true;
}

