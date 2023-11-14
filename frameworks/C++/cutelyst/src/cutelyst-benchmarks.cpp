#include "cutelyst-benchmarks.h"

#include <Cutelyst/Plugins/Utils/Sql>
#include <Cutelyst/Plugins/View/Cutelee/cuteleeview.h>

#include <QtSql/QSqlDatabase>
#include <QtSql/QSqlError>
#include <QCoreApplication>
#include <QTimer>
#include <QThread>
#include <QDebug>
#include <QMutexLocker>
#include <QDir>

#include <apool.h>
#include <apg.h>

#include "jsontest.h"
#include "singledatabasequerytest.h"
#include "multipledatabasequeriestest.h"
#include "databaseupdatestest.h"
#include "fortunetest.h"
#include "plaintexttest.h"
#include "cachedqueries.h"

using namespace Cutelyst;
using namespace ASql;

static QMutex mutex;

cutelyst_benchmarks::cutelyst_benchmarks(QObject *parent) : Application(parent)
{
    static std::once_flag once;
    std::call_once(once, []() { srand(time(NULL)); });
}

cutelyst_benchmarks::~cutelyst_benchmarks()
{
}

bool cutelyst_benchmarks::init()
{
    if (config(u"SendDate"_qs).value<bool>()) {
        qDebug() << "Manually send date";
        auto dateT = new QTimer(this);
        dateT->setInterval(1000);
        dateT->setSingleShot(false);
        connect(dateT, &QTimer::timeout, this, [=] {
            defaultHeaders().setDateWithDateTime(QDateTime::currentDateTimeUtc());
        });
    }

    auto view = new CuteleeView(this);
    view->setIncludePaths({ QString::fromLatin1(qgetenv("TROOT")), QDir::currentPath() });
    view->preloadTemplates();

    new JsonTest(this);
    new SingleDatabaseQueryTest(this);
    new MultipleDatabaseQueriesTest(this);
    new DatabaseUpdatesTest(this);
    new FortuneTest(this);
    new PlaintextTest(this);
    new CachedQueries(this);

    if (defaultHeaders().server().isEmpty()) {
        defaultHeaders().setServer("Cutelyst"_qba);
    }
    defaultHeaders().removeHeader("X-Cutelyst");

    return true;
}

bool cutelyst_benchmarks::postFork()
{
    QMutexLocker locker(&mutex); // QSqlDatabase::addDatabase is not thread-safe

    QSqlDatabase db;
    const auto driver = config(u"Driver"_qs).toString();
    if (driver == u"QPSQL") {
        db = QSqlDatabase::addDatabase(driver, Sql::databaseNameThread(u"postgres"_qs));
        db.setDatabaseName(u"hello_world"_qs);
        db.setUserName(u"benchmarkdbuser"_qs);
        db.setPassword(u"benchmarkdbpass"_qs);
        db.setHostName(config(u"DatabaseHostName"_qs).toString());
        if (!db.open()) {
            qDebug() << "Error opening PostgreSQL db:" << db << db.connectionName() << db.lastError().databaseText();
            return false;
        }
    } else if (driver == u"QMYSQL") {
        db = QSqlDatabase::addDatabase(driver, Sql::databaseNameThread(u"mysql"_qs));
        db.setDatabaseName(u"hello_world"_qs);
        db.setUserName(u"benchmarkdbuser"_qs);
        db.setPassword(u"benchmarkdbpass"_qs);
        db.setHostName(config(u"DatabaseHostName"_qs).toString());
        if (!db.open()) {
            qDebug() << "Error opening MySQL db:" << db << db.connectionName() << db.lastError().databaseText();
            return false;
        }
    } else if (driver == u"postgres") {
        QUrl uri(u"postgresql://benchmarkdbuser:benchmarkdbpass@server/hello_world"_qs);
        uri.setHost(config(u"DatabaseHostName"_qs).toString());
        qDebug() << "ASql URI:" << uri.toString();

        APool::create(ASql::APg::factory(uri.toString()));
        APool::setMaxIdleConnections(128);
        APool::setSetupCallback([](ADatabase db) {
            // Enable Pipeline mode
            db.enterPipelineMode(500);
        });
    }

    qDebug() << "Connections" << QCoreApplication::applicationPid() << QThread::currentThread() << QSqlDatabase::connectionNames();

    return true;
}

#include "moc_cutelyst-benchmarks.cpp"
