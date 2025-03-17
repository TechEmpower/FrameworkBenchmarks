<?php
/**
 * Workerman HTTP server startup file
 * Based on Workerman to implement an HTTP server, integrated with the ThinkPHP framework
 */

use Workerman\Worker;
use Workerman\Connection\TcpConnection;
use Workerman\Protocols\Http\Request as WorkermanRequest;
use Cyber\Response;

// Load Composer autoload file
require __DIR__ . '/vendor/autoload.php';

// Create a Worker listening on port 8099, using HTTP protocol for communication
$http_worker = new Worker("http://0.0.0.0:8080");

// Set the number of processes (set according to the number of CPU cores, recommended to be 1-4 times the number of CPU cores)
$http_worker->count = 4;

// Initialize ThinkPHP application
$app = \Cyber\App::bootstrap(__DIR__.'/bootstrap.php');

/**
 * Callback function to handle HTTP requests
 * @param TcpConnection $connection Client connection object
 * @param WorkermanRequest $request HTTP request object
 */
$http_worker->onMessage = function(TcpConnection $connection, WorkermanRequest $request) use ($app) {
    // Initialize request object
    $_GET = $request->get();  // Get GET parameters
    $_POST = $request->post(); // Get POST parameters
    $_FILES = $request->file(); // Get file uploads
    $_COOKIE = $request->cookie(); // Get COOKIE

    // Merge server variables
    $_SERVER = array_merge($_SERVER, [
        'RAW_BODY' => $request->rawBody(), // Raw request body
        'REQUEST_METHOD' => $request->method(), // Request method
        'REQUEST_URI' => $request->uri(), // Request URI
        'QUERY_STRING' => $request->queryString(), // Query string
        'REMOTE_ADDR' => $connection->getRemoteIp(), // Client IP
        'REMOTE_PORT' => $connection->getRemotePort(), // Client port
        'SERVER_PROTOCOL' => 'HTTP/'.$request->protocolVersion(), // Protocol version
    ]);

    // Handle request headers
    foreach ($request->header() as $key => $value) {
        $_SERVER['HTTP_' . strtoupper(str_replace('-', '_', $key))] = $value;
    }

    try {
        ob_start(); // Start output buffering
        $response = $app->run(); // Run ThinkPHP application

        // Handle response
        if(!$response instanceof Response){
            // If not a Response object, directly output content
            echo $response;
            $content = ob_get_clean();
            $connection->send($content);
        }else{
            // If it is a Response object, send HTTP response
            echo $response->send();
            $content = ob_get_clean();
            $connection->send(new Workerman\Protocols\Http\Response(
                $response->getStatusCode(), // Status code
                $response->getHeaders(), // Response headers
                $content // Response content
            ));
        }
    } catch (Exception $e) {
        // Catch exceptions and render error page
        $connection->send(renderExceptionPage($e));
    } catch (Throwable $e) {
        // Catch all errors
        $connection->send(renderExceptionPage($e));
    }
};

/**
 * Run all Worker instances
 * This method will block the current process until all Workers stop
 */
Worker::runAll();
