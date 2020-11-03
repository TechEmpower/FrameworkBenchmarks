#ifndef FORTUNETEST_H
#define FORTUNETEST_H

#include <Cutelyst/Controller>

using namespace Cutelyst;

typedef struct {
    int id;
    QString message;
} Fortune;
typedef std::vector<Fortune> FortuneList;

class QSqlQuery;
class FortuneTest : public Controller
{
    Q_OBJECT
    C_NAMESPACE("")
public:
    explicit FortuneTest(QObject *parent = 0);

    C_ATTR(fortunes_raw_p, :Local :AutoArgs)
    void fortunes_raw_p(Context *c);

    C_ATTR(fortunes_raw_postgres, :Local :AutoArgs)
    void fortunes_raw_postgres(Context *c);

    C_ATTR(fortunes_raw_mysql, :Local :AutoArgs)
    void fortunes_raw_mysql(Context *c);

    C_ATTR(fortunes_c_p, :Local :AutoArgs)
    void fortunes_c_p(Context *c);

    C_ATTR(fortunes_cutelee_postgres, :Local :AutoArgs)
    void fortunes_cutelee_postgres(Context *c);

    C_ATTR(fortunes_cutelee_mysql, :Local :AutoArgs)
    void fortunes_cutelee_mysql(Context *c);

private:
    inline FortuneList processQuery(Context *c, QSqlQuery &query);
    inline void renderRaw(Context *c, const FortuneList &fortunes) const;
};

#endif // FORTUNETEST_H
