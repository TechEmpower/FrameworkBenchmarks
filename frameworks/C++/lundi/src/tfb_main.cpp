#include <lundi.hpp>
#include "pg_pipeline.hpp"
#include "json_writer.hpp"
#include <lundi/engine/date_cache.hpp>

#include <asio/experimental/channel.hpp>

#include <array>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <random>
#include <vector>

using namespace lundi;
using namespace lundi::bench;

using pool_channel_t = asio::experimental::channel< void( asio::error_code, int ) >;

struct pg_thread_pool {
    std::vector< std::unique_ptr< pg_connection > > conns;
    std::unique_ptr< pool_channel_t > ch;
    bool ready = false;
};

static pg_thread_pool& tl_pool()
{
    thread_local pg_thread_pool pool;
    return pool;
}

static int g_pool_size = 4;

struct pg_lease {
    pool_channel_t* ch_ = nullptr;
    pg_connection*  conn_ = nullptr;
    int             idx_ = -1;

    pg_lease() = default;
    pg_lease( pool_channel_t* ch, pg_connection* c, int idx )
        : ch_( ch ), conn_( c ), idx_( idx ) {}

    pg_lease( pg_lease&& o ) noexcept
        : ch_( o.ch_ ), conn_( o.conn_ ), idx_( o.idx_ )
    {
        o.ch_ = nullptr;
        o.conn_ = nullptr;
        o.idx_ = -1;
    }

    pg_lease& operator=( pg_lease&& o ) noexcept
    {
        release();
        ch_ = o.ch_;  conn_ = o.conn_;  idx_ = o.idx_;
        o.ch_ = nullptr;  o.conn_ = nullptr;  o.idx_ = -1;
        return *this;
    }

    pg_lease( const pg_lease& ) = delete;
    pg_lease& operator=( const pg_lease& ) = delete;

    ~pg_lease() { release(); }

    void release()
    {
        if( ch_ && idx_ >= 0 )
        {
            ch_->try_send( asio::error_code{}, idx_ );
            ch_ = nullptr;
            idx_ = -1;
        }
    }

    pg_connection* operator->() { return conn_; }
    pg_connection& operator*()  { return *conn_; }
    explicit operator bool() const { return conn_ != nullptr; }
};

// Globals
static std::array< int, 10001 > cached_worlds{};
static std::string g_conninfo;

static inline std::string buf_to_string( const engine::write_buffer& buf )
{
    return std::string( buf.data(), buf.size() );
}

static inline int clamp_queries( int n )
{
    if( n < 1 )   return 1;
    if( n > 500 ) return 500;
    return n;
}

static asio::awaitable< pg_lease > acquire_pg()
{
    auto& pool = tl_pool();

    if( __builtin_expect( !pool.ready, false ) )
    {
        auto exec = co_await asio::this_coro::executor;
        int n = g_pool_size;

        pool.conns.reserve( n );
        pool.ch = std::make_unique< pool_channel_t >( exec, n );

        for( int i = 0; i < n; ++i )
        {
            auto c = std::make_unique< pg_connection >();
            if( !c->connect( exec, g_conninfo ) )
            {
                std::cerr << "[pg] pool: failed to connect slot " << i << "\n";
                co_return pg_lease{};
            }
            pool.conns.push_back( std::move( c ) );
            pool.ch->try_send( asio::error_code{}, i );
        }
        pool.ready = true;
    }

    // Suspend until a connection is free
    int idx = co_await pool.ch->async_receive( asio::use_awaitable );
    co_return pg_lease{ pool.ch.get(), pool.conns[ idx ].get(), idx };
}

int main()
{
    uint16_t port = 8080;
    if( auto* env = std::getenv( "PORT" ) )
        port = static_cast< uint16_t >( std::atoi( env ) );

    int threads = 0;
    if( auto* env = std::getenv( "THREADS" ) )
        threads = std::atoi( env );

    if( auto* env = std::getenv( "PG_POOL_SIZE" ) )
        g_pool_size = std::max( 1, std::min( std::atoi( env ), 32 ) );

    g_conninfo = tfb_connection_string();

    lundi::app server;

    // JSON serialization
    server.get( "/json", []( request& ) -> asio::awaitable< response > {
        engine::global_date_cache().update();
        co_return response::json( R"({"message":"Hello, World!"})" );
    } );

    // Plaintext
    server.get( "/plaintext", []( request& ) -> asio::awaitable< response > {
        engine::global_date_cache().update();
        co_return response::text( "Hello, World!" );
    } );

    // Single DB query
    server.get( "/db", []( request& ) -> asio::awaitable< response > {
        engine::global_date_cache().update();

        auto lease = co_await acquire_pg();
        if( !lease )
            co_return response::json( R"({"error":"DB connection failed"})", 500 );

        auto row = co_await lease->query_world( pg_connection::random_world_id() );

        engine::write_buffer buf( 64 );
        write_world_json( buf, row.id, row.random_number );
        co_return response::json( buf_to_string( buf ) );
    } );

    // Multiple DB queries
    server.get( "/queries", []( request& req ) -> asio::awaitable< response > {
        engine::global_date_cache().update();

        auto lease = co_await acquire_pg();
        if( !lease )
            co_return response::json( R"({"error":"DB connection failed"})", 500 );

        int count = clamp_queries( req.query_int( "queries", 1 ) );
        auto rows = co_await lease->query_worlds( count );

        engine::write_buffer buf( count * 40 );
        write_worlds_json( buf, rows );
        co_return response::json( buf_to_string( buf ) );
    } );

    // Fortunes
    server.get( "/fortunes", []( request& ) -> asio::awaitable< response > {
        engine::global_date_cache().update();

        auto lease = co_await acquire_pg();
        if( !lease )
            co_return response::text( "DB connection failed", 500 );

        auto fortunes = co_await lease->query_fortunes();

        engine::write_buffer buf( 4096 );
        write_fortunes_html( buf, fortunes );
        co_return response::html( buf_to_string( buf ) );
    } );

    // Updates
    server.get( "/updates", []( request& req ) -> asio::awaitable< response > {
        engine::global_date_cache().update();

        auto lease = co_await acquire_pg();
        if( !lease )
            co_return response::json( R"({"error":"DB connection failed"})", 500 );

        int count = clamp_queries( req.query_int( "queries", 1 ) );
        auto rows = co_await lease->update_worlds( count );

        engine::write_buffer buf( count * 40 );
        write_worlds_json( buf, rows );
        co_return response::json( buf_to_string( buf ) );
    } );

    // Cached queries
    server.get( "/cached-worlds", []( request& req ) -> asio::awaitable< response > {
        engine::global_date_cache().update();
        thread_local std::mt19937 rng( std::random_device{}() );
        std::uniform_int_distribution< int > dist( 1, 10000 );

        int count = clamp_queries( req.query_int( "count", 1 ) );
        std::vector< world_row > rows;
        rows.reserve( count );
        for( int i = 0; i < count; ++i )
        {
            int id = dist( rng );
            rows.push_back( { id, cached_worlds[ id ] } );
        }

        engine::write_buffer buf( count * 40 );
        write_worlds_json( buf, rows );
        co_return response::json( buf_to_string( buf ) );
    } );

    // Pre-load world cache
    {
        auto* conn = PQconnectdb( g_conninfo.c_str() );
        if( conn && PQstatus( conn ) == CONNECTION_OK )
        {
            auto* r = PQexec( conn, "SELECT id, randomNumber FROM World" );
            if( r && PQresultStatus( r ) == PGRES_TUPLES_OK )
            {
                int n = PQntuples( r );
                for( int i = 0; i < n; ++i )
                {
                    int id = std::atoi( PQgetvalue( r, i, 0 ) );
                    int rn = std::atoi( PQgetvalue( r, i, 1 ) );
                    if( id >= 1 && id <= 10000 )
                        cached_worlds[ id ] = rn;
                }
                std::cout << "[lundi] cached " << n << " world rows\n";
            }
            if( r )
                PQclear( r );
            PQfinish( conn );
        }
        else
        {
            std::cerr << "[lundi] WARNING: cache load failed, "
                      << "cached-worlds will return zeros\n";
            if( conn )
                PQfinish( conn );
        }
    }

    std::cout << "[lundi] pg pool size: " << g_pool_size << " per thread\n";

    server.listen( {
        .port = port,
        .threads = threads,
        .max_keepalive_requests = 0,
    } );

    return 0;
}