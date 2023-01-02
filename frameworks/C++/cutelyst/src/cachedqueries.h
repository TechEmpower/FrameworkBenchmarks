#ifndef CACHEDQUERIES_H
#define CACHEDQUERIES_H

#include <Cutelyst/Controller>

using namespace Cutelyst;

class CachedQueries : public Controller
{
    Q_OBJECT
    C_NAMESPACE("")
public:
    explicit CachedQueries(QObject *parent = nullptr);

    C_ATTR(cached_queries, :Local :AutoArgs)
    void cached_queries(Context *c);
};

#endif // CACHEDQUERIES_H
