#ifndef SINGLEDATABASEQUERYTEST_H
#define SINGLEDATABASEQUERYTEST_H

#include <Cutelyst/Controller>

using namespace Cutelyst;

class QSqlQuery;
class SingleDatabaseQueryTest : public Controller
{
    Q_OBJECT
    C_NAMESPACE("")
public:
    explicit SingleDatabaseQueryTest(QObject *parent = 0);

    C_ATTR(db_asql_pg, :Path('pg') :AutoArgs)
    void db_asql_pg(Context *c);

    C_ATTR(db_asql_pipeline_pg, :Path('Pg') :AutoArgs)
    void db_asql_pipeline_pg(Context *c);

    C_ATTR(db_postgres, :Path('PG') :AutoArgs)
    void db_postgres(Context *c);

    C_ATTR(db_mysql, :Path('MY') :AutoArgs)
    void db_mysql(Context *c);

private:
    inline void processQuery(Context *c, QSqlQuery &query);
};

#endif // SINGLEDATABASEQUERYTEST_H
