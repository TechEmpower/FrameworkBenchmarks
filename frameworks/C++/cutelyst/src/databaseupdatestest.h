#ifndef DATABASEUPDATESTEST_H
#define DATABASEUPDATESTEST_H

#include <Cutelyst/Controller>
#include <apreparedquery.h>

using namespace Cutelyst;

class QSqlQuery;
class DatabaseUpdatesTest : public Controller
{
    Q_OBJECT
    C_NAMESPACE("")
public:
    explicit DatabaseUpdatesTest(QObject *parent = 0);

    C_ATTR(updatep, :Local :AutoArgs)
    void updatep(Context *c);

    C_ATTR(updateb, :Local :AutoArgs)
    void updateb(Context *c);

    C_ATTR(updates_postgres, :Path('ups_QPG') :AutoArgs)
    void updates_postgres(Context *c);

    C_ATTR(updates_mysql, :Path('ups_QMY') :AutoArgs)
    void updates_mysql(Context *c);

private:
    inline void processQuery(Context *c, QSqlQuery &query, QSqlQuery &updateQuery);
    inline ASql::APreparedQuery getSql(int count);

    QMap<int, ASql::APreparedQuery> m_sqlMap;
};

#endif // DATABASEUPDATESTEST_H
