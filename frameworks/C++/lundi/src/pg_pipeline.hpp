#ifndef LUNDI_BENCH_PG_PIPELINE_HPP
#define LUNDI_BENCH_PG_PIPELINE_HPP

#include <libpq-fe.h>

#if __has_include(<pg_config_ext.h>)
    #include <pg_config_ext.h>
#elif __has_include(<pg_config.h>)
    #include <pg_config.h>
#endif

#if defined(PG_VERSION_NUM)
    #define LUNDI_PG_VERSION PG_VERSION_NUM
#elif defined(PQ_VERSION_NUM)
    #define LUNDI_PG_VERSION PQ_VERSION_NUM
#else
    #define LUNDI_PG_VERSION 0
#endif

#include <lundi/engine/buffer.hpp>
#include <lundi/engine/fast_itoa.hpp>

#include <asio.hpp>
#include <asio/awaitable.hpp>
#include <asio/posix/stream_descriptor.hpp>
#include <asio/use_awaitable.hpp>

#include <cstdlib>
#include <functional>
#include <iostream>
#include <memory>
#include <random>
#include <string>
#include <string_view>
#include <vector>

namespace lundi::bench
{

struct world_row {
    int id;
    int random_number;
};

struct fortune_row {
    int id;
    std::string message;
};

class pg_connection
{
public:
    pg_connection() = default;
    ~pg_connection() { disconnect(); }

    pg_connection( const pg_connection& ) = delete;
    pg_connection& operator=( const pg_connection& ) = delete;

    bool connect( const asio::any_io_executor& exec, const std::string& conninfo )
    {
        conn_ = PQconnectdb( conninfo.c_str() );
        if( PQstatus( conn_ ) != CONNECTION_OK )
        {
            std::cerr << "[pg] connection failed: " << PQerrorMessage( conn_ ) << "\n";
            return false;
        }

        // Prepare while still in blocking mode
        if( !prepare( "select_world",
                      "SELECT id, randomNumber FROM World WHERE id = $1", 1 ) )
            return false;
        if( !prepare( "update_world",
                      "UPDATE World SET randomNumber = $1 WHERE id = $2", 2 ) )
            return false;
        if( !prepare( "select_fortunes",
                      "SELECT id, message FROM Fortune", 0 ) )
            return false;

        // Switch to non-blocking
        if( PQsetnonblocking( conn_, 1 ) != 0 )
        {
            std::cerr << "[pg] failed to set non-blocking: " << PQerrorMessage( conn_ ) << "\n";
            return false;
        }

        int fd = PQsocket( conn_ );
        if( fd < 0 )
            return false;
        sd_ = std::make_unique< asio::posix::stream_descriptor >( exec, fd );

        #if LUNDI_PG_VERSION >= 140000
        if( PQenterPipelineMode( conn_ ) == 1 )
            pipeline_ = true;
        else
            std::cerr << "[pg] PQenterPipelineMode failed: " << PQerrorMessage( conn_ ) << "\n";
        #endif

        std::cerr << "[pg] connected, pipeline=" << ( pipeline_ ? "yes" : "no" )
                  << " LUNDI_PG_VERSION=" << LUNDI_PG_VERSION << "\n";

        return true;
    }

    bool connect( asio::io_context& ctx, const std::string& conninfo )
    {
        return connect( ctx.get_executor(), conninfo );
    }

    void disconnect()
    {
        sd_.reset();
        if( conn_ ) { PQfinish( conn_ ); conn_ = nullptr; }
    }

    // Single DB query
    asio::awaitable< world_row > query_world( int id )
    {
        char id_str[ 12 ];
        snprintf( id_str, sizeof( id_str ), "%d", id );
        const char* params[] = { id_str };

        if( !PQsendQueryPrepared( conn_, "select_world", 1, params, nullptr, nullptr, 0 ) )
            co_return world_row{ 0, 0 };

        if( pipeline_ ) pipeline_sync();

        co_await flush();

        world_row row{ 0, 0 };
        auto result = co_await async_get_result();
        if( result && PQresultStatus( result.get() ) == PGRES_TUPLES_OK &&
            PQntuples( result.get() ) > 0 )
        {
            row.id = std::atoi( PQgetvalue( result.get(), 0, 0 ) );
            row.random_number = std::atoi( PQgetvalue( result.get(), 0, 1 ) );
        }

        co_await async_get_result(); // NULL terminator

        if( pipeline_ ) co_await async_get_result(); // PGRES_PIPELINE_SYNC

        co_return row;
    }

    // Multiple DB queries
    asio::awaitable< std::vector< world_row > > query_worlds( int count )
    {
        thread_local std::mt19937 rng( std::random_device{}() );
        std::uniform_int_distribution< int > dist( 1, 10000 );

        if( pipeline_ )
        {
            // Pipeline: send all, sync, flush, read all
            std::vector< world_row > rows;
            rows.reserve( count );

            int sent = 0;
            for( int i = 0; i < count; ++i )
            {
                char id_str[ 12 ];
                snprintf( id_str, sizeof( id_str ), "%d", dist( rng ) );
                const char* params[] = { id_str };
                if( PQsendQueryPrepared( conn_, "select_world", 1, params, nullptr, nullptr, 0 ) )
                    ++sent;
            }

            pipeline_sync();
            co_await flush();

            for( int i = 0; i < sent; ++i )
            {
                auto result = co_await async_get_result();
                if( result && PQresultStatus( result.get() ) == PGRES_TUPLES_OK &&
                    PQntuples( result.get() ) > 0 )
                {
                    rows.push_back( {
                        std::atoi( PQgetvalue( result.get(), 0, 0 ) ),
                        std::atoi( PQgetvalue( result.get(), 0, 1 ) )
                    } );
                }
                co_await async_get_result(); // NULL separator
            }

            co_await async_get_result(); // PGRES_PIPELINE_SYNC
            co_return rows;
        }

        // Fallback: one query at a time
        std::vector< world_row > rows;
        rows.reserve( count );
        for( int i = 0; i < count; ++i )
        {
            auto row = co_await query_world( dist( rng ) );
            if( row.id != 0 )
                rows.push_back( row );
        }
        co_return rows;
    }

    // Updates
    asio::awaitable< std::vector< world_row > > update_worlds( int count )
    {
        auto rows = co_await query_worlds( count );

        thread_local std::mt19937 rng( std::random_device{}() );
        std::uniform_int_distribution< int > dist( 1, 10000 );

        if( pipeline_ )
        {
            for( auto& row : rows )
            {
                row.random_number = dist( rng );
                char rn_str[ 12 ], id_str[ 12 ];
                snprintf( rn_str, sizeof( rn_str ), "%d", row.random_number );
                snprintf( id_str, sizeof( id_str ), "%d", row.id );
                const char* params[] = { rn_str, id_str };
                PQsendQueryPrepared( conn_, "update_world", 2, params, nullptr, nullptr, 0 );
            }

            pipeline_sync();
            co_await flush();

            for( size_t i = 0; i < rows.size(); ++i )
            {
                co_await async_get_result(); // PGRES_COMMAND_OK
                co_await async_get_result(); // NULL separator
            }

            co_await async_get_result(); // PGRES_PIPELINE_SYNC
            co_return rows;
        }

        // Fallback: one update at a time
        for( auto& row : rows )
        {
            row.random_number = dist( rng );
            char rn_str[ 12 ], id_str[ 12 ];
            snprintf( rn_str, sizeof( rn_str ), "%d", row.random_number );
            snprintf( id_str, sizeof( id_str ), "%d", row.id );
            const char* params[] = { rn_str, id_str };

            PQsendQueryPrepared( conn_, "update_world", 2, params, nullptr, nullptr, 0 );
            co_await flush();
            co_await async_get_result(); // PGRES_COMMAND_OK
            co_await async_get_result(); // NULL
        }
        co_return rows;
    }

    // Fortunes
    asio::awaitable< std::vector< fortune_row > > query_fortunes()
    {
        if( !PQsendQueryPrepared( conn_, "select_fortunes", 0, nullptr, nullptr, nullptr, 0 ) )
            co_return std::vector< fortune_row >{};

        if( pipeline_ ) pipeline_sync();

        co_await flush();

        auto result = co_await async_get_result();
        std::vector< fortune_row > fortunes;

        if( result && PQresultStatus( result.get() ) == PGRES_TUPLES_OK )
        {
            int n = PQntuples( result.get() );
            fortunes.reserve( n + 1 );
            for( int i = 0; i < n; ++i )
            {
                fortunes.push_back( {
                    std::atoi( PQgetvalue( result.get(), i, 0 ) ),
                    PQgetvalue( result.get(), i, 1 )
                } );
            }
        }

        co_await async_get_result(); // NULL

        if( pipeline_ ) co_await async_get_result(); // PGRES_PIPELINE_SYNC

        co_return fortunes;
    }

    static int random_world_id()
    {
        thread_local std::mt19937 rng( std::random_device{}() );
        std::uniform_int_distribution< int > dist( 1, 10000 );
        return dist( rng );
    }

    bool has_pipeline() const { return pipeline_; }

private:
    PGconn* conn_ = nullptr;
    std::unique_ptr< asio::posix::stream_descriptor > sd_;
    bool pipeline_ = false;

    // Wrapper so callers don't need #if guards
    void pipeline_sync()
    {
        #if LUNDI_PG_VERSION >= 140000
        PQpipelineSync( conn_ );
        #endif
    }

    bool prepare( const char* name, const char* query, int nparams )
    {
        auto* res = PQprepare( conn_, name, query, nparams, nullptr );
        if( !res || PQresultStatus( res ) != PGRES_COMMAND_OK )
        {
            std::cerr << "[pg] prepare '" << name << "' failed: "
                      << PQerrorMessage( conn_ ) << "\n";
            if( res ) PQclear( res );
            return false;
        }
        PQclear( res );
        return true;
    }

    struct pg_result_deleter {
        void operator()( PGresult* r ) { if( r ) PQclear( r ); }
    };
    using pg_result_ptr = std::unique_ptr< PGresult, pg_result_deleter >;

    asio::awaitable< pg_result_ptr > async_get_result()
    {
        while( true )
        {
            if( !PQconsumeInput( conn_ ) )
                co_return pg_result_ptr( nullptr );

            if( !PQisBusy( conn_ ) )
                co_return pg_result_ptr( PQgetResult( conn_ ) );

            co_await sd_->async_wait(
                asio::posix::stream_descriptor::wait_read,
                asio::use_awaitable );
        }
    }

    asio::awaitable< void > flush()
    {
        while( true )
        {
            int r = PQflush( conn_ );
            if( r == 0 ) co_return;
            if( r == -1 ) co_return;

            co_await sd_->async_wait(
                asio::posix::stream_descriptor::wait_write,
                asio::use_awaitable );
        }
    }
};

inline std::string tfb_connection_string()
{
    auto host = std::getenv( "DBHOST" );
    auto port = std::getenv( "PG_PORT" );
    return std::string( "host=" ) + ( host ? host : "tfb-database" ) +
           " port=" + ( port ? port : "5432" ) +
           " dbname=hello_world"
           " user=benchmarkdbuser"
           " password=benchmarkdbpass"
           " sslmode=disable";
}

} // namespace lundi::bench

#endif // LUNDI_BENCH_PG_PIPELINE_HPP