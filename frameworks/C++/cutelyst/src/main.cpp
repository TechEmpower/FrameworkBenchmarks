
#include "cutelyst-benchmarks.h"

#include <QCoreApplication>

#include <Cutelyst/Server/server.h>

int main(int argc, char *argv[])
{
    QCoreApplication app(argc, argv);

    Cutelyst::Server server;
    server.parseCommandLine(app.arguments());

    auto bench = new cutelyst_benchmarks;

    return server.exec(bench);
}
