#ifndef CUTELYST_BENCHMARKS_H
#define CUTELYST_BENCHMARKS_H

#include <Cutelyst/Application>

using namespace Cutelyst;

class cutelyst_benchmarks : public Application
{
    Q_OBJECT
    CUTELYST_APPLICATION(IID "cutelyst_benchmarks")
public:
    Q_INVOKABLE explicit cutelyst_benchmarks(QObject *parent = 0);
    ~cutelyst_benchmarks();

    bool init();

    bool postFork();
};

#endif //CUTELYST_BENCHMARKS_H

