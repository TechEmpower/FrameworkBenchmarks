#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/beast/version.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/config.hpp>
#include <boost/json/src.hpp>

#include <algorithm>
#include <cstdlib>
#include <ctime>
#include <iostream>
#include <memory>
#include <string>
#include <thread>
#include <vector>
#include <random>

#if defined(BOOST_ASIO_HAS_CO_AWAIT)

#include <libpq-fe.h>

namespace beast = boost::beast;   // from <boost/beast.hpp>
namespace http = beast::http;     // from <boost/beast/http.hpp>
namespace net = boost::asio;      // from <boost/asio.hpp>
using tcp = boost::asio::ip::tcp; // from <boost/asio/ip/tcp.hpp>
namespace json = boost::json;     // from <boost/json.hpp>

using tcp_stream = typename beast::tcp_stream::rebind_executor<
		net::use_awaitable_t<>::executor_with_default<net::any_io_executor>>::other;

namespace becpp
{

using result_ptr = std::unique_ptr<PGresult, decltype(&PQclear)>;

//https://gist.github.com/ictlyh/12fe787ec265b33fd7e4b0bd08bc27cb
result_ptr prepare(PGconn* conn,
                   const char* stmtName,
                   const char* command,
                   uint8_t nParams)
{
	auto res = PQprepare(conn, stmtName, command, nParams, nullptr);
	if (PQresultStatus(res) != PGRES_COMMAND_OK)
	{
		std::cerr << "PQprepare failed: " << PQresultErrorMessage(res)
			        << std::endl;
		PQclear(res);
		return {nullptr, nullptr};
	}

	return {res, &PQclear};
}

result_ptr execute(PGconn* conn,
                   const char* stmtName,
                   uint8_t nParams,
                   const char* const* paramValues,
                   const int* paramLengths = nullptr,
                   const int* paramFormats = nullptr,
                   int resultFormat = 0)
{
	auto res = PQexecPrepared(conn, stmtName, nParams, paramValues, paramLengths,
	                          paramFormats, resultFormat);
	const auto status = PQresultStatus(res);
	if (status != PGRES_COMMAND_OK &&
	    status != PGRES_TUPLES_OK  &&
	    status != PGRES_SINGLE_TUPLE)
	{
		std::cerr << "PQexecPrepared failed: " << PQresultErrorMessage(res)
		          << std::endl;
		PQclear(res);
		return {nullptr, nullptr};
	}

	return {res, &PQclear};
}

const char* env(const char* env_var, const char* default_value)
{
	if (const char* env_p = std::getenv(env_var))
		return env_p;

	return default_value;
}

std::string now_string()
{
	std::time_t time = std::time(nullptr);
	char timeString[std::size("Wed, 17 Apr 2013 12:00:00 GMT")];
	std::strftime(std::data(timeString), std::size(timeString), "%a, %d %b %Y %X %Z", std::localtime(&time));
	return timeString;
}

template <class Body, class Allocator>
http::message_generator
handle_error(
	http::request<Body, http::basic_fields<Allocator>>&& req,
	http::status status,
	beast::string_view msg)
{
	http::response<http::string_body> res{status, req.version()};
	res.set(http::field::server, BOOST_BEAST_VERSION_STRING);
	res.set(http::field::content_type, "text/html");
	res.set(http::field::date, now_string());
	res.keep_alive(req.keep_alive());
	res.body() = std::string(msg);
	res.prepare_payload();
	return res;
}

template <class Body, class Allocator>
http::message_generator
handle_target(
	http::request<Body, http::basic_fields<Allocator>>&& req,
	PGconn* conn = nullptr)
{
	//std::cout << "handle_target: " << req.target() << std::endl;
	http::response<http::string_body> res{http::status::ok, req.version()};
	res.set(http::field::server, BOOST_BEAST_VERSION_STRING);
	res.set(http::field::content_type, "application/json");
	res.set(http::field::date, now_string());
	res.keep_alive(req.keep_alive());
	
	if (req.target() == "/json")
	{
		// {"message":"Hello, World!"}
		json::object obj;
		obj["message"] = "Hello, World!";
		res.body() = json::serialize(obj);
	}
	else if (req.target() == "/plaintext")
	{
		res.set(http::field::content_type, "text/plain");
		res.body() = "Hello, World!";
	}
	else if (req.target() == "/db" || req.target().starts_with("/queries/"))
	{
		static std::random_device rd;
		static std::minstd_rand gen(rd());
		static std::uniform_int_distribution<> distrib(1, 10000);
		static const char* word_query = "SELECT randomNumber FROM world WHERE id=$1";
		thread_local auto stmt = prepare(conn, "word_query_stmt", word_query, 1);

		if (req.target() == "/db")
		{
			const unsigned uint_id = distrib(gen);
			const auto str_id =  std::to_string(uint_id);
			const char* char_ptr_id = str_id.c_str();
			if (auto rs = execute(conn, "word_query_stmt", 1, &char_ptr_id))
			{
				// {"id":3217,"randomNumber":2149}
				json::object obj;
				obj["id"] = uint_id;
				obj["randomNumber"] = std::atoi(PQgetvalue(rs.get(), 0, 0));
				res.body() = json::serialize(obj);
				//std::cout << "res.body(): " << res.body() << std::endl;
			}
			else
				return handle_error(std::move(req),
				                    http::status::internal_server_error,
				                    "internal_server_error");
		}
		else if (req.target().starts_with("/queries/"))
		{
			int n_queries = 1;
			try
			{
				const int n = std::stoi(req.target().substr(req.target().find_last_of('/')+1));
				if (n > 1) n_queries = n;
			} catch(...) {}
			if (n_queries > 500) n_queries = 500;
			json::array objs;
			for (auto i = 0; i < n_queries; ++i)
			{
				const unsigned uint_id = distrib(gen);
				const auto str_id =  std::to_string(uint_id);
				const char* char_ptr_id = str_id.c_str();
				if (auto rs = execute(conn, "word_query_stmt", 1, &char_ptr_id))
				{
					// {"id":3217,"randomNumber":2149}
					json::object obj;
					obj["id"] = uint_id;
					obj["randomNumber"] = std::atoi(PQgetvalue(rs.get(), 0, 0));
					objs.push_back(obj);
				}
				else
					return handle_error(std::move(req),
					                    http::status::internal_server_error,
					                    "internal_server_error");
			}
			res.body() = json::serialize(objs);
			//std::cout << "res.body(): " << res.body() << std::endl;
		}
	}
	else
	{
		return handle_error(std::move(req),
		                    http::status::not_found,
		                    "Unhandled target: '" + std::string(req.target()) + "'");
	}

	res.prepare_payload();
	return res;
}

// Return a response for the given request.
//
// The concrete type of the response message (which depends on the
// request), is type-erased in message_generator.
template <class Body, class Allocator>
http::message_generator
handle_request(
	http::request<Body, http::basic_fields<Allocator>>&& req)
{
	// Make sure we can handle the method
	if (req.method() != http::verb::get)
		return handle_error(std::move(req),
		                    http::status::not_found,
		                    "Unhandled method: '" + std::string(req.method_string()) + "'");

	if (req.target() == "/json" || req.target() == "/plaintext")
		return handle_target(std::move(req));

	thread_local std::unique_ptr<PGconn, decltype(&PQfinish)> conn
	{
		PQconnectdb(env("BCPP_PG_CONN_STR", "")),
		&PQfinish
	};
	if (PQstatus(conn.get()) != CONNECTION_OK)
	{
		auto msg = PQerrorMessage(conn.get());
		std::cerr << "PQerrorMessage: " << msg << std::endl;
		return handle_error(std::move(req),
		                    http::status::internal_server_error,
		                    msg);
	}
	return handle_target(std::move(req), conn.get());
}

}
//------------------------------------------------------------------------------


// Handles an HTTP server connection
net::awaitable<void>
do_session(tcp_stream stream)
{
	// This buffer is required to persist across reads
	beast::flat_buffer buffer;

	// This lambda is used to send messages
	try
	{
		for(;;)
		{
			// Set the timeout.
			stream.expires_after(std::chrono::seconds(30));

			// Read a request
			http::request<http::string_body> req;
			co_await http::async_read(stream, buffer, req);

			// Handle the request
			http::message_generator msg =
				becpp::handle_request(std::move(req));

			// Determine if we should close the connection
			bool keep_alive = msg.keep_alive();

			// Send the response
			co_await beast::async_write(stream, std::move(msg), net::use_awaitable);

			if(! keep_alive)
			{
				// This means we should close the connection, usually because
				// the response indicated the "Connection: close" semantic.
				break;
			}
		}
	}
	catch (boost::system::system_error & se)
	{
		if (se.code() != http::error::end_of_stream )
			throw ;
	}

	// Send a TCP shutdown
	beast::error_code ec;
	stream.socket().shutdown(tcp::socket::shutdown_send, ec);

	// At this point the connection is closed gracefully
	// we ignore the error because the client might have
	// dropped the connection already.
}

//------------------------------------------------------------------------------

// Accepts incoming connections and launches the sessions
net::awaitable<void>
do_listen(tcp::endpoint endpoint)
{
	// Open the acceptor
	auto acceptor = net::use_awaitable.as_default_on(tcp::acceptor(co_await net::this_coro::executor));
	acceptor.open(endpoint.protocol());

	// Allow address reuse
	acceptor.set_option(net::socket_base::reuse_address(true));

	// Bind to the server address
	acceptor.bind(endpoint);

	// Start listening for connections
	acceptor.listen(net::socket_base::max_listen_connections);

	for(;;)
		boost::asio::co_spawn(
			acceptor.get_executor(),
				do_session(tcp_stream(co_await acceptor.async_accept())),
				[](std::exception_ptr e)
				{
					if (e)
						try
						{
							std::rethrow_exception(e);
						}
						catch (std::exception&){}
//						catch (std::exception &e) {
//							std::cerr << "Error in session: " << e.what() << "\n";
//						}
				});

}

int main(int argc, char* argv[])
{
	auto const address = net::ip::make_address(becpp::env("BCPP_ADDRESS", "0.0.0.0"));
	auto const port = static_cast<unsigned short>(std::atoi(becpp::env("BCPP_PORT", "8000")));
	auto env_threads = std::atoi(becpp::env("BCPP_N_THREADS", "0"));
	if (env_threads == 0)
	{
		env_threads = std::thread::hardware_concurrency();
		std::cout << "Using number of cores: " << env_threads << '\n';
	}
	auto const threads = std::max<int>(1, env_threads);

	std::cout << "__GNUG__=" << __GNUG__ << '\n';
	std::cout << "__cplusplus=" << __cplusplus << '\n';
	std::cout << "__TIMESTAMP__=" << __TIMESTAMP__ << '\n';
	std::cout << "__GNUC_EXECUTION_CHARSET_NAME=" << __GNUC_EXECUTION_CHARSET_NAME << '\n';
	std::cout << "Listening " << address << ':' << port << " threads=" << threads << std::endl;

	// The io_context is required for all I/O
	net::io_context ioc{threads};

	// Spawn a listening port
	boost::asio::co_spawn(ioc,
						  do_listen(tcp::endpoint{address, port}),
						  [](std::exception_ptr e)
						  {
							  if (e)
								  try
								  {
									  std::rethrow_exception(e);
								  }
								  catch(std::exception & e)
								  {
									  std::cerr << "Error in acceptor: " << e.what() << "\n";
								  }
						  });

	// Run the I/O service on the requested number of threads
	try {
	std::vector<std::thread> v;
	v.reserve(threads - 1);
	for(auto i = threads - 1; i > 0; --i)
		v.emplace_back(
		[&ioc]
		{
			ioc.run();
		});
	ioc.run();
	} catch (std::exception& e)
	{
		std::cerr << "Error in main: " << e.what() << "\n";
	}

	return EXIT_SUCCESS;
}

#else

int main(int, char * [])
{
	std::printf("awaitables require C++20\n");
	return 1;
}

#endif
