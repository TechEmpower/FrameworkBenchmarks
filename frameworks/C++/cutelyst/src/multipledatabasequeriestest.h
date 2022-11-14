#ifndef MULTIPLEDATABASEQUERIESTEST_H
#define MULTIPLEDATABASEQUERIESTEST_H

#include <Cutelyst/Controller>

using namespace Cutelyst;

class QSqlQuery;
class MultipleDatabaseQueriesTest : public Controller
{
    Q_OBJECT
    C_NAMESPACE("")
public:
    explicit MultipleDatabaseQueriesTest(QObject *parent = 0);

    C_ATTR(queriesp, :Path('querAPG') :AutoArgs)
    void queriesp(Context *c);

    C_ATTR(query_postgres, :Path('queryPG') :AutoArgs)
    void query_postgres(Context *c);

    C_ATTR(query_mysql, :Path('queryMY') :AutoArgs)
    void query_mysql(Context *c);

private:
    inline void processQuery(Context *c, QSqlQuery &query);
};

#endif // MULTIPLEDATABASEQUERIESTEST_H
