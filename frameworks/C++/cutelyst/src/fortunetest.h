#ifndef FORTUNETEST_H
#define FORTUNETEST_H

#include <Cutelyst/Controller>

using namespace Cutelyst;

typedef std::pair<int, QString> Fortune;
typedef std::vector<Fortune> FortuneList;

class QSqlQuery;
class FortuneTest : public Controller
{
    Q_OBJECT
    C_NAMESPACE("")
public:
    explicit FortuneTest(QObject *parent = 0);

    C_ATTR(fortunes_raw_postgres, :Local :AutoArgs)
    void fortunes_raw_postgres(Context *c);

    C_ATTR(fortunes_raw_mysql, :Local :AutoArgs)
    void fortunes_raw_mysql(Context *c);

    C_ATTR(fortunes_grantlee_postgres, :Local :AutoArgs)
    void fortunes_grantlee_postgres(Context *c);

    C_ATTR(fortunes_grantlee_mysql, :Local :AutoArgs)
    void fortunes_grantlee_mysql(Context *c);

private:
    inline FortuneList processQuery(Context *c, QSqlQuery &query);
    inline void renderRaw(Context *c, const FortuneList &fortunes);
};

#endif // FORTUNETEST_H
