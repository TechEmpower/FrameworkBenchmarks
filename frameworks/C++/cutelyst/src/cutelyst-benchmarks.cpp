#include "cutelyst-benchmarks.h"

#include <Cutelyst/Plugins/Utils/Sql>
#include <Cutelyst/Plugins/View/Grantlee/grantleeview.h>

#include <QtSql/QSqlDatabase>
#include <QtSql/QSqlError>
#include <QCoreApplication>
#include <QThread>
#include <QDebug>
#include <QMutexLocker>
#include <QDir>

#include "root.h"
#include "jsontest.h"
#include "singledatabasequerytest.h"
#include "multipledatabasequeriestest.h"
#include "databaseupdatestest.h"
#include "fortunetest.h"
#include "plaintexttest.h"

using namespace Cutelyst;

static QMutex mutex;

cutelyst_benchmarks::cutelyst_benchmarks(QObject *parent) : Application(parent)
{
    qsrand(QDateTime::currentMSecsSinceEpoch());
}

cutelyst_benchmarks::~cutelyst_benchmarks()
{
}

bool cutelyst_benchmarks::init()
{
    if (config(QStringLiteral("SendDate")).value<bool>()) {
        qDebug() << "Manually send date";
        new Root(this);
    }

    auto view = new GrantleeView(this);
    view->setIncludePaths({ QString::fromLatin1(qgetenv("TROOT")), QDir::currentPath() });
    view->preloadTemplates();

    new JsonTest(this);
    new SingleDatabaseQueryTest(this);
    new MultipleDatabaseQueriesTest(this);
    new DatabaseUpdatesTest(this);
    new FortuneTest(this);
    new PlaintextTest(this);

    if (defaultHeaders().server().isEmpty()) {
        defaultHeaders().setServer(QStringLiteral("Cutelyst"));
    }
    defaultHeaders().removeHeader(QStringLiteral("X-Cutelyst"));

    return true;
}

bool cutelyst_benchmarks::postFork()
{
    QMutexLocker locker(&mutex); // QSqlDatabase::addDatabase is not thread-safe

    QSqlDatabase db;
    const auto driver = config(QStringLiteral("Driver")).toString();
    if (driver == QLatin1String("QPSQL")) {
        db = QSqlDatabase::addDatabase(driver, Sql::databaseNameThread(QStringLiteral("postgres")));
        db.setDatabaseName(QStringLiteral("hello_world"));
        db.setUserName(QStringLiteral("benchmarkdbuser"));
        db.setPassword(QStringLiteral("benchmarkdbpass"));
        db.setHostName(config(QStringLiteral("DatabaseHostName")).toString());
        if (!db.open()) {
            qDebug() << "Error opening PostgreSQL db:" << db << db.connectionName() << db.lastError().databaseText();
            return false;
        }
    } else if (driver == QLatin1String("QMYSQL")) {
        db = QSqlDatabase::addDatabase(driver, Sql::databaseNameThread(QStringLiteral("mysql")));
        db.setDatabaseName(QStringLiteral("hello_world"));
        db.setUserName(QStringLiteral("benchmarkdbuser"));
        db.setPassword(QStringLiteral("benchmarkdbpass"));
        db.setHostName(config(QStringLiteral("DatabaseHostName")).toString());
        if (!db.open()) {
            qDebug() << "Error opening MySQL db:" << db << db.connectionName() << db.lastError().databaseText();
            return false;
        }
    }

    qDebug() << "Connections" << QCoreApplication::applicationPid() << QThread::currentThread() << QSqlDatabase::connectionNames();
//    db = QSqlDatabase::addDatabase(QLatin1String("QSQLITE"), QLatin1String("sqlite"));
//    if (!db.open()) {
//        qDebug() << "Error opening db:" << db << db.lastError().databaseText();
//        return false;
//    }

    return true;
}

