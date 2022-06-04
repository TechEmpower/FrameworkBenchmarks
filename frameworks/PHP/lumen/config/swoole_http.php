<?php
return [
	/*
	 * |--------------------------------------------------------------------------
	 * | HTTP server configurations.
	 * |--------------------------------------------------------------------------
	 * |
	 * | @see https://wiki.swoole.com/wiki/page/274.html
	 * |
	 */
	'server' => [
		'host' => env('SWOOLE_HTTP_HOST', '0.0.0.0'),
		'port' => env('SWOOLE_HTTP_PORT', '8080'),
		'options' => [
			'pid_file' => env('SWOOLE_HTTP_PID_FILE', base_path('swoole_http.pid')),
			// 'log_file' => env('SWOOLE_HTTP_LOG_FILE', base_path('storage/logs/swoole_http.log')),
			'daemonize' => env('SWOOLE_HTTP_DAEMONIZE', 0),
			// 'handle_static_files' => env('SWOOLE_HTTP_STATIC', 0),
			// 'public_path' => storage_path('app/public'),
			// 'ob_output' => env('SWOOLE_HTTP_OB_OUTPUT', 0),
			// Normally this value should be 1~4 times lager according to your cpu cores
			'reactor_num' => env('SWOOLE_HTTP_REACTOR_NUM', swoole_cpu_num() * 2),
			'worker_num' => env('SWOOLE_HTTP_WORKER_NUM', swoole_cpu_num() * 2),
			'task_worker_num' => env('SWOOLE_HTTP_TASK_WORKER_NUM', swoole_cpu_num() * 2),
			// This value should be larger than `post_max_size` and `upload_max_filesize` in `php.ini`.
			// This equals to 10 MB
			'package_max_length' => 10 * 1024 * 1024,
			'buffer_output_size' => 10 * 1024 * 1024,
			// Max buffer size for socket connections
			'socket_buffer_size' => 128 * 1024 * 1024,
			// Worker will restart after processing this number of request
			'max_request' => 3000
		]
	],
	'providers' => [ // App\Providers\AuthServiceProvider::class,
	]
];
