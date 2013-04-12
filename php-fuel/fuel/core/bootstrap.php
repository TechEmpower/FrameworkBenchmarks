<?php
/**
 * Part of the Fuel framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

define('DS', DIRECTORY_SEPARATOR);
define('CRLF', chr(13).chr(10));

setup_autoloader();

// Load the base functions
require COREPATH.'base.php';

/**
 * Do we have access to mbstring?
 * We need this in order to work with UTF-8 strings
 */
define('MBSTRING', function_exists('mb_get_info'));

/**
 * Register all the error/shutdown handlers
 */
register_shutdown_function(function ()
{
	// reset the autoloader
	\Autoloader::_reset();

	// Fire off the shutdown events
	Event::shutdown();

	return \Error::shutdown_handler();
});

set_exception_handler(function (\Exception $e)
{
	// reset the autoloader
	\Autoloader::_reset();

	return \Error::exception_handler($e);
});

set_error_handler(function ($severity, $message, $filepath, $line)
{
	// reset the autoloader
	\Autoloader::_reset();

	return \Error::error_handler($severity, $message, $filepath, $line);
});

function setup_autoloader()
{
	Autoloader::add_namespace('Fuel\\Core', COREPATH.'classes/');

	Autoloader::add_namespace('PHPSecLib', COREPATH.'vendor'.DS.'phpseclib'.DS, true);

	Autoloader::add_classes(array(
		'Fuel\\Core\\Agent'           => COREPATH.'classes/agent.php',

		'Fuel\\Core\\Arr'             => COREPATH.'classes/arr.php',

		'Fuel\\Core\\Asset'           => COREPATH.'classes/asset.php',
		'Fuel\\Core\\Asset_Instance'  => COREPATH.'classes/asset/instance.php',

		'Fuel\\Core\\Cache'                     => COREPATH.'classes/cache.php',
		'Fuel\\Core\\CacheNotFoundException'    => COREPATH.'classes/cache/notfound.php',
		'Fuel\\Core\\CacheExpiredException'     => COREPATH.'classes/cache.php',
		'Fuel\\Core\\Cache_Handler_Driver'      => COREPATH.'classes/cache/handler/driver.php',
		'Fuel\\Core\\Cache_Handler_Json'        => COREPATH.'classes/cache/handler/json.php',
		'Fuel\\Core\\Cache_Handler_Serialized'  => COREPATH.'classes/cache/handler/serialized.php',
		'Fuel\\Core\\Cache_Handler_String'      => COREPATH.'classes/cache/handler/string.php',
		'Fuel\\Core\\Cache_Storage_Driver'      => COREPATH.'classes/cache/storage/driver.php',
		'Fuel\\Core\\Cache_Storage_Apc'         => COREPATH.'classes/cache/storage/apc.php',
		'Fuel\\Core\\Cache_Storage_File'        => COREPATH.'classes/cache/storage/file.php',
		'Fuel\\Core\\Cache_Storage_Memcached'   => COREPATH.'classes/cache/storage/memcached.php',
		'Fuel\\Core\\Cache_Storage_Redis'       => COREPATH.'classes/cache/storage/redis.php',

		'Fuel\\Core\\Config'               => COREPATH.'classes/config.php',
		'Fuel\\Core\\ConfigException'      => COREPATH.'classes/config.php',
		'Fuel\\Core\\Config_File'          => COREPATH.'classes/config/file.php',
		'Fuel\\Core\\Config_Ini'           => COREPATH.'classes/config/ini.php',
		'Fuel\\Core\\Config_Json'          => COREPATH.'classes/config/json.php',
		'Fuel\\Core\\Config_Interface'     => COREPATH.'classes/config/interface.php',
		'Fuel\\Core\\Config_Php'           => COREPATH.'classes/config/php.php',
		'Fuel\\Core\\Config_Yml'          => COREPATH.'classes/config/yml.php',

		'Fuel\\Core\\Controller'           => COREPATH.'classes/controller.php',
		'Fuel\\Core\\Controller_Rest'      => COREPATH.'classes/controller/rest.php',
		'Fuel\\Core\\Controller_Template'  => COREPATH.'classes/controller/template.php',
		'Fuel\\Core\\Controller_Hybrid'    => COREPATH.'classes/controller/hybrid.php',

		'Fuel\\Core\\Cookie'               => COREPATH.'classes/cookie.php',

		'Fuel\\Core\\DB'      => COREPATH.'classes/db.php',
		'Fuel\\Core\\DBUtil'  => COREPATH.'classes/dbutil.php',

		'Fuel\\Core\\Database_Connection'            => COREPATH.'classes/database/connection.php',
		'Fuel\\Core\\Database_Exception'             => COREPATH.'classes/database/exception.php',
		'Fuel\\Core\\Database_Expression'            => COREPATH.'classes/database/expression.php',
		'Fuel\\Core\\Database_Pdo_Connection'        => COREPATH.'classes/database/pdo/connection.php',
		'Fuel\\Core\\Database_Query'                 => COREPATH.'classes/database/query.php',
		'Fuel\\Core\\Database_Query_Builder'         => COREPATH.'classes/database/query/builder.php',
		'Fuel\\Core\\Database_Query_Builder_Insert'  => COREPATH.'classes/database/query/builder/insert.php',
		'Fuel\\Core\\Database_Query_Builder_Delete'  => COREPATH.'classes/database/query/builder/delete.php',
		'Fuel\\Core\\Database_Query_Builder_Update'  => COREPATH.'classes/database/query/builder/update.php',
		'Fuel\\Core\\Database_Query_Builder_Select'  => COREPATH.'classes/database/query/builder/select.php',
		'Fuel\\Core\\Database_Query_Builder_Where'   => COREPATH.'classes/database/query/builder/where.php',
		'Fuel\\Core\\Database_Query_Builder_Join'    => COREPATH.'classes/database/query/builder/join.php',
		'Fuel\\Core\\Database_Result'                => COREPATH.'classes/database/result.php',
		'Fuel\\Core\\Database_Result_Cached'         => COREPATH.'classes/database/result/cached.php',
		'Fuel\\Core\\Database_Mysql_Connection'      => COREPATH.'classes/database/mysql/connection.php',
		'Fuel\\Core\\Database_MySQL_Result'          => COREPATH.'classes/database/mysql/result.php',
		'Fuel\\Core\\Database_Mysqli_Connection'     => COREPATH.'classes/database/mysqli/connection.php',
		'Fuel\\Core\\Database_MySQLi_Result'         => COREPATH.'classes/database/mysqli/result.php',

		'Fuel\\Core\\Fuel'           => COREPATH.'classes/fuel.php',
		'Fuel\\Core\\FuelException'  => COREPATH.'classes/fuel.php',

		'Fuel\\Core\\Finder'         => COREPATH.'classes/finder.php',

		'Fuel\\Core\\Date' => COREPATH.'classes/date.php',

		'Fuel\\Core\\Debug'   => COREPATH.'classes/debug.php',

		'Fuel\\Core\\Cli'     => COREPATH.'classes/cli.php',

		'Fuel\\Core\\Crypt'   => COREPATH.'classes/crypt.php',

		'Fuel\\Core\\Event'            => COREPATH.'classes/event.php',
		'Fuel\\Core\\Event_Instance'   => COREPATH.'classes/event/instance.php',

		'Fuel\\Core\\Error'               => COREPATH.'classes/error.php',
		'Fuel\\Core\\PhpErrorException'   => COREPATH.'classes/error.php',

		'Fuel\\Core\\Format'  => COREPATH.'classes/format.php',

		'Fuel\\Core\\Fieldset'        => COREPATH.'classes/fieldset.php',
		'Fuel\\Core\\Fieldset_Field'  => COREPATH.'classes/fieldset/field.php',

		'Fuel\\Core\\File'                    => COREPATH.'classes/file.php',
		'Fuel\\Core\\FileAccessException'     => COREPATH.'classes/file.php',
		'Fuel\\Core\\OutsideAreaException'    => COREPATH.'classes/file.php',
		'Fuel\\Core\\InvalidPathException'    => COREPATH.'classes/file.php',
		'Fuel\\Core\\File_Area'               => COREPATH.'classes/file/area.php',
		'Fuel\\Core\\File_Handler_File'       => COREPATH.'classes/file/handler/file.php',
		'Fuel\\Core\\File_Handler_Directory'  => COREPATH.'classes/file/handler/directory.php',

		'Fuel\\Core\\Form'           => COREPATH.'classes/form.php',
		'Fuel\\Core\\Form_Instance'  => COREPATH.'classes/form/instance.php',

		'Fuel\\Core\\Ftp'                     => COREPATH.'classes/ftp.php',
		'Fuel\\Core\\FtpConnectionException'  => COREPATH.'classes/ftp.php',
		'Fuel\\Core\\FtpFileAccessException'  => COREPATH.'classes/ftp.php',

		'Fuel\\Core\\HttpException'             => COREPATH.'classes/httpexception.php',
		'Fuel\\Core\\HttpNotFoundException'     => COREPATH.'classes/httpexceptions.php',
		'Fuel\\Core\\HttpServerErrorException'  => COREPATH.'classes/httpexceptions.php',

		'Fuel\\Core\\Html'  => COREPATH.'classes/html.php',

		'Fuel\\Core\\Image'              => COREPATH.'classes/image.php',
		'Fuel\\Core\\Image_Driver'       => COREPATH.'classes/image/driver.php',
		'Fuel\\Core\\Image_Gd'           => COREPATH.'classes/image/gd.php',
		'Fuel\\Core\\Image_Imagemagick'  => COREPATH.'classes/image/imagemagick.php',
		'Fuel\\Core\\Image_Imagick'      => COREPATH.'classes/image/imagick.php',

		'Fuel\\Core\\Inflector'  => COREPATH.'classes/inflector.php',

		'Fuel\\Core\\Input'      => COREPATH.'classes/input.php',

		'Fuel\\Core\\Lang'               => COREPATH.'classes/lang.php',
		'Fuel\\Core\\LangException'      => COREPATH.'classes/lang.php',
		'Fuel\\Core\\Lang_File'          => COREPATH.'classes/lang/file.php',
		'Fuel\\Core\\Lang_Ini'           => COREPATH.'classes/lang/ini.php',
		'Fuel\\Core\\Lang_Json'          => COREPATH.'classes/lang/json.php',
		'Fuel\\Core\\Lang_Interface'     => COREPATH.'classes/lang/interface.php',
		'Fuel\\Core\\Lang_Php'           => COREPATH.'classes/lang/php.php',
		'Fuel\\Core\\Lang_Yml'           => COREPATH.'classes/lang/yml.php',

		'Fuel\\Core\\Markdown'   => COREPATH.'classes/markdown.php',

		'Fuel\\Core\\Migrate'    => COREPATH.'classes/migrate.php',

		'Fuel\\Core\\Model'      => COREPATH.'classes/model.php',
		'Fuel\\Core\\Model_Crud' => COREPATH.'classes/model/crud.php',

		'Fuel\\Core\\Module'                    => COREPATH.'classes/module.php',
		'Fuel\\Core\\ModuleNotFoundException'   => COREPATH.'classes/module.php',

		'Fuel\\Core\\Mongo_Db'           => COREPATH.'classes/mongo/db.php',
		'Fuel\\Core\\Mongo_DbException'  => COREPATH.'classes/mongo/db.php',

		'Fuel\\Core\\Output'               => COREPATH.'classes/output.php',

		'Fuel\\Core\\Package'                   => COREPATH.'classes/package.php',
		'Fuel\\Core\\PackageNotFoundException'  => COREPATH.'classes/package.php',

		'Fuel\\Core\\Pagination'           => COREPATH.'classes/pagination.php',

		'Fuel\\Core\\Profiler'             => COREPATH.'classes/profiler.php',

		'Fuel\\Core\\Request'                 => COREPATH.'classes/request.php',
		'Fuel\\Core\\Request_Driver'          => COREPATH.'classes/request/driver.php',
		'Fuel\\Core\\RequestException'        => COREPATH.'classes/request/driver.php',
		'Fuel\\Core\\RequestStatusException'  => COREPATH.'classes/request/driver.php',
		'Fuel\\Core\\Request_Curl'            => COREPATH.'classes/request/curl.php',
		'Fuel\\Core\\Request_Soap'            => COREPATH.'classes/request/soap.php',

		'Fuel\\Core\\Redis'                   => COREPATH.'classes/redis.php',
		'Fuel\\Core\\RedisException'          => COREPATH.'classes/redis.php',

		'Fuel\\Core\\Response'  => COREPATH.'classes/response.php',

		'Fuel\\Core\\Route'     => COREPATH.'classes/route.php',
		'Fuel\\Core\\Router'    => COREPATH.'classes/router.php',

		'Fuel\\Core\\Security'  => COREPATH.'classes/security.php',

		'Fuel\\Core\\Session'            => COREPATH.'classes/session.php',
		'Fuel\\Core\\Session_Driver'     => COREPATH.'classes/session/driver.php',
		'Fuel\\Core\\Session_Db'         => COREPATH.'classes/session/db.php',
		'Fuel\\Core\\Session_Cookie'     => COREPATH.'classes/session/cookie.php',
		'Fuel\\Core\\Session_File'       => COREPATH.'classes/session/file.php',
		'Fuel\\Core\\Session_Memcached'  => COREPATH.'classes/session/memcached.php',
		'Fuel\\Core\\Session_Redis'      => COREPATH.'classes/session/redis.php',
		'Fuel\\Core\\Session_Exception'  => COREPATH.'classes/session/exception.php',

		'Fuel\\Core\\Num'       => COREPATH.'classes/num.php',

		'Fuel\\Core\\Str'       => COREPATH.'classes/str.php',

		'Fuel\\Core\\TestCase'  => COREPATH.'classes/testcase.php',

		'Fuel\\Core\\Theme'          => COREPATH.'classes/theme.php',
		'Fuel\\Core\\ThemeException' => COREPATH.'classes/theme.php',

		'Fuel\\Core\\Uri'       => COREPATH.'classes/uri.php',

		'Fuel\\Core\\Unzip'     => COREPATH.'classes/unzip.php',

		'Fuel\\Core\\Upload'    => COREPATH.'classes/upload.php',

		'Fuel\\Core\\Validation'        => COREPATH.'classes/validation.php',
		'Fuel\\Core\\Validation_Error'  => COREPATH.'classes/validation/error.php',

		'Fuel\\Core\\View'       => COREPATH.'classes/view.php',
		'Fuel\\Core\\ViewModel'  => COREPATH.'classes/viewmodel.php',
	));
};
