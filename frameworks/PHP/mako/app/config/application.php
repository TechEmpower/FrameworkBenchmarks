<?php

return
[
	/*
	 * ---------------------------------------------------------
	 * Base URL.
	 * ---------------------------------------------------------
	 *
	 * Base URL of your application.
	 *
	 * It will be auto-detected in a web environment if set to null
	 * but should be configured if you plan to build URLs in the
	 * command line.
	 */
	'base_url' => null,

	/*
	 * ---------------------------------------------------------
	 * Clean URLs
	 * ---------------------------------------------------------
	 *
	 * Set to true to hide "index.php" from your urls.
	 */
	'clean_urls' => true,

	/*
	 * ---------------------------------------------------------
	 * Timezone
	 * ---------------------------------------------------------
	 *
	 * Set the default timezone used by various PHP date functions.
	 */
	'timezone' => 'UTC',

	/*
	 * ---------------------------------------------------------
	 * Charset
	 * ---------------------------------------------------------
	 *
	 * Default character set used internally in the framework.
	 */
	'charset' => 'UTF-8',

	/*
	 * ---------------------------------------------------------
	 * Language
	 * ---------------------------------------------------------
	 *
	 * Default application language and locale.
	 */
	'default_language' => ['strings' => 'en_US', 'locale' => [LC_ALL => ['en_US.UTF-8', 'en_US.utf8', 'C.UTF-8', 'C'], LC_NUMERIC => 'C']],

	/*
	 * ---------------------------------------------------------
	 * Languages
	 * ---------------------------------------------------------
	 *
	 * If the first segment of the request path matches the language (array key)
	 * then the default language will be set to the mapped language (array value).
	 */
	'languages' =>
	[
		//'no' => ['strings' => 'nb_NO', 'locale' => [LC_ALL => ['nb_NO.UTF-8', 'nb_NO.utf8', 'C.UTF-8', 'C'], LC_NUMERIC => 'C']],
	],

	/*
	 * ---------------------------------------------------------
	 * Commands
	 * ---------------------------------------------------------
	 *
	 * This is where you register your reactor commands.
	 * You can either let the framework auto-detect and register the commands
	 * for you or register them manually using the 'commands' array.
	 *
	 * To disable auto-detection just comment out the 'commands_directory' key.
	 *
	 * When using the 'commands' array the key is the command name and the array value
	 * is the command class.
	 */
	'commands_directory' => MAKO_APPLICATION_PATH . '/console/commands',

	/*
	 * ---------------------------------------------------------
	 * Services
	 * ---------------------------------------------------------
	 *
	 * Services to register in the dependecy injection container.
	 * They will be registered in the order that they are defined.
	 *
	 * core: Services that are required for both the web an the command line interface
	 * web : Services that are only required for the web
	 * cli : Services that are only required for the command line interface
	 */
	'services' =>
	[
		'core' =>
		[
			mako\application\services\SignerService::class,
			mako\application\services\HTTPService::class,
			//mako\application\services\LoggerService::class,
			mako\application\services\ViewFactoryService::class,
			//mako\application\services\SessionService::class,
			mako\application\services\DatabaseService::class,
			//mako\application\services\RedisService::class,
			//mako\application\services\I18nService::class,
			//mako\application\services\HumanizerService::class,
			//mako\application\services\CacheService::class,
			//mako\application\services\CryptoService::class,
			//mako\application\services\ValidatorFactoryService::class,
			//mako\application\services\PaginationFactoryService::class,
			//mako\application\services\GatekeeperService::class,
			//mako\application\services\EventService::class,
			//mako\application\services\CommandBusService::class,
		],
		'web' =>
		[
			mako\application\services\web\ErrorHandlerService::class,
		],
		'cli' =>
		[
			mako\application\services\cli\ErrorHandlerService::class,
		],
	],

	/*
	 * ---------------------------------------------------------
	 * Packages
	 * ---------------------------------------------------------
	 *
	 * Packages to boot during the application boot sequence.
	 * They will be booted in the order that they are defined.
	 *
	 * core: Packages that are required for both the web an the command line interface
	 * web : Packages that are only required for the web
	 * cli : Packages that are only required for the command line interface
	 */
	'packages' =>
	[
		'core' =>
		[

		],
		'web' =>
		[

		],
		'cli' =>
		[

		],
	],

	/*
	 * ---------------------------------------------------------
	 * Storage path
	 * ---------------------------------------------------------
	 *
	 * Application storage base path.
	 */
	'storage_path' => MAKO_APPLICATION_PATH . '/storage',

	/*
	 * ---------------------------------------------------------
	 * Secret
	 * ---------------------------------------------------------
	 *
	 * The secret is used to provide cryptographic signing, and should be set to a unique, unpredictable value.
	 * You should NOT use the secret included with the framework in a production environment!
	 */
	'secret' => 'hex:fef26e19745fb4d4f6183466fcde2288cc9a85a889babf437b0dfb1ef1b5364c',

	/*
	 * ---------------------------------------------------------
	 * Trusted proxies
	 * ---------------------------------------------------------
	 *
	 * If your application isn't behind a proxy you trust then you can (and should) leave this empty.
	 * If it is behind a proxy then you can help the framework return the correct client IP (using the X-Forwarded-For header)
	 * by listing your proxy IP address(es) here.
	 */
	'trusted_proxies' =>
	[

	],

	/*
	 * ---------------------------------------------------------
	 * Serialization whitelist.
	 * ---------------------------------------------------------
	 *
	 * Array of classes that you'll allow the framework to deserialize.
	 * Set to FALSE for none and TRUE for all.
	 */
	'deserialization_whitelist' => false,

	/*
	 * ---------------------------------------------------------
	 * Logger
	 * ---------------------------------------------------------
	 *
	 * channel: Log channel name
	 * handler: Log handler(s) to use. The avaiable options out of the box are 'ErrorLog', 'Stream' and 'Syslog'.
	 * syslog : Syslog specific options (https://linux.die.net/man/3/syslog).
	 */
	'logger' =>
	[
		'channel' => 'mako',
		'handler' => ['Stream'],
		'syslog'  =>
		[
			'identifier' => 'Mako',
			'facility'   => LOG_USER,
		],
	],

	/*
	 * ---------------------------------------------------------
	 * Error handling
	 * ---------------------------------------------------------
	 *
	 * log_errors     : Set to true if you want to log errors caught by the Mako errors handler.
	 * display_errors : Set to true to display detailed information about errors caught by the mako error handlers.
	 * keep           : Specify a list of cookies or headers to keep when an exception has been handled.
	 * dont_log       : Array of exception types to ignore when logging errors.
	 */
	'error_handler' =>
	[
		'log_errors'     => true,
		'display_errors' => true,
		'keep'           => [], // E.g. ['headers' => ['Access-Control-.*']]
		'dont_log'       =>
		[
			mako\http\exceptions\HttpStatusException::class,
		],
	],
];
