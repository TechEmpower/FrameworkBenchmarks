#include "cutelyst-benchmarks.h"

#include <QCoreApplication>

#include <Cutelyst/WSGI/wsgi.h>

int main(int argc, char *argv[])
{
    QCoreApplication app(argc, argv);

    CWSGI::WSGI wsgi;
    wsgi.parseCommandLine(app.arguments());

    auto bench = new cutelyst_benchmarks;

    return wsgi.exec(bench);
}
