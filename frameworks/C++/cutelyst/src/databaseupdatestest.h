#ifndef DATABASEUPDATESTEST_H
#define DATABASEUPDATESTEST_H

#include <Cutelyst/Controller>

using namespace Cutelyst;

class QSqlQuery;
class DatabaseUpdatesTest : public Controller
{
    Q_OBJECT
    C_NAMESPACE("")
public:
    explicit DatabaseUpdatesTest(QObject *parent = 0);

    C_ATTR(updates_postgres, :Local :AutoArgs)
    void updates_postgres(Context *c);

    C_ATTR(updates_mysql, :Local :AutoArgs)
    void updates_mysql(Context *c);

private:
    inline void processQuery(Context *c, QSqlQuery &query, QSqlQuery &updateQuery);
};

#endif // DATABASEUPDATESTEST_H
