//
// Created by Mpho Mbotho on 2021-01-15.
//

#include    <suil/base/env.hpp>
#include <suil/http/server/pgsqlmw.hpp>
#include <suil/http/server/endpoint.hpp>
#include <suil/http/server/sysattrs.hpp>


#include "app.scc.hpp"

using suil::net::ServerConfig;
using suil::net::TcpSocketConfig;
using suil::http::server::PgSqlMiddleware;
using suil::http::server::Endpoint;
using suil::http::server::SystemAttrs;
using suil::http::server::Request;
using suil::http::server::Response;
using suil::db::Orm;
using suil::db::PgSqlConnection;
using suil::bench::World;

using WorldOrm = Orm<PgSqlConnection, suil::bench::World>;

#define DEFAULT_POSTGRES_CONN "postgres://build:passwd@postgres:5432/dev"

static inline int randnum()
{
    // from https://stackoverflow.com/questions/1640258/need-a-fast-random-generator-for-c
    constexpr int MAX_VALUE = 10000;
    static int g_seed = 0;
    g_seed = (214013*g_seed+2531011);
    return 1 + ((g_seed>>16)&0x7FFF)%MAX_VALUE;
}

#if SUIL_BENCH_DEV==1
void seedDatabase(PgSqlConnection& conn)
{
    constexpr int MAX_RECORDS = 10000;
    WorldOrm orm("world", conn);
    if (orm.cifne(false)) {
        for (int i = 0; i < MAX_RECORDS; i++) {
            orm.insert(World{.id = i+1, .randomNumber = randnum()});
        }
    }
}
#endif

int main(int argc, char *argv[])
{
    suil::setup(opt(verbose, 1));
    auto config = ServerConfig{
        .socketConfig = TcpSocketConfig {
            .bindAddr = {.name = "0.0.0.0", .port = 8080}
        }
    };

    Endpoint<SystemAttrs, PgSqlMiddleware> ep{"/",
          opt(serverConfig, std::move(config))
    };

    ep.middleware<PgSqlMiddleware>().setup(
            suil::env("POSTGRES_CONN", DEFAULT_POSTGRES_CONN),
            opt(ASYNC,   true),    // connections are async
            opt(TIMEOUT, 10_sec),  // timeout on db transactions
            opt(EXPIRES, 30_sec)   // connections are cached for 30 seconds
    );

#if SUIL_BENCH_DEV == 1
    {
        scoped(conn, ep.middleware<PgSqlMiddleware>().conn(false));
        seedDatabase(conn);
    }
#endif

    Route(ep, "/plaintext")
    ("GET"_method)
    .attrs(opt(ReplyType, "text/plain"))
    ([]() {
        return suil::String{"Hello, World!"};
    });

    Route(ep, "/json")
    ("GET"_method)
    .attrs(opt(ReplyType, "application/json"))
    ([]() {
        return suil::bench::Object{.message = "Hello, World!"};
    });

    Route(ep, "/db")
    ("GET"_method)
    .attrs(opt(ReplyType, "application/json"))
    ([&ep]() {
        scoped(conn, ep.middleware<PgSqlMiddleware>().conn());
        WorldOrm orm("World", conn);
        suil::bench::World obj{};
        orm.find(opt(id, randnum()), obj);
        return obj;
    });

    Route(ep, "/query")
    ("GET"_method)
    .attrs(opt(ReplyType, "application/json"))
    ([&ep](const Request& req, Response& resp) {
        auto queries = req.query().get<int>("queries") or int(1);
        int count = std::max(1, std::min(*queries, 500));

        scoped(conn, ep.context<PgSqlMiddleware>(req).conn());
        std::vector<World> objects(count);
        WorldOrm orm("World", conn);
        for (auto& obj: objects) {
            orm.find(opt(id, randnum()), obj);
        }

        resp.append(objects);
        resp.end();
    });

    Route(ep, "/update")
    ("GET"_method)
    .attrs(opt(ReplyType, "application/json"))
    ([&ep](const Request& req, Response& resp) {
        auto queries = req.query().get<int>("queries") or int(1);
        int count = std::max(1, std::min(*queries, 500));

        scoped(conn, ep.context<PgSqlMiddleware>(req).conn());
        std::vector<World> objects(count);

        WorldOrm orm("World", conn);

        for (auto& obj: objects) {
            orm.find(opt(id, randnum()), obj);
            obj.randomNumber = randnum();
        }

        {
            suil::db::PgSqlTransaction txn(conn.get());
            txn.begin();

            // Sorting transactions to avoid postgres deadlock
            std::sort(objects.begin(), objects.end(),
                      [](const World& a, const World& b) { return a.id < b.id; });
            for (auto& obj: objects) {
                orm.update(obj);
            }
        }

        resp.append(objects);
        resp.end();
    });

    return ep.start();
}