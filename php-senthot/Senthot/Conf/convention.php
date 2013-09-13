<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

/**
 * Senthot practice profile
 * Please do not modify the file , if you want to overwrite the configured value convention , the configuration file in the project settings and practices inconsistent configuration items
 * Case of arbitrary configuration name , the system will unify converted to lowercase
 * All configuration parameters can be dynamically changed before the commencement
 * @category Sen
 * @package		Common
 * @author		ms134n <ms134n@gmail.com>
 * @version  $Id: convention.php 3088 2012-07-29 09:12:19Z ettynind@gmail.com $
 */
defined('SEN_PATH') or exit();
return  array(
    /* Item Settings */
    'APP_STATUS'            => 'debug',  // Application debugging mode status Debug mode is turned on effective The default is debug Scalable And automatically loads the corresponding configuration file
    'APP_FILE_CASE'         => false,   // Whether to check the case file Valid for Windows platforms
    'APP_AUTOLOAD_PATH'     => '',// Automatic loading mechanism of automatic path search, Note that the search order
    'APP_TAGS_ON'           => true, // System tab expansion switch
    'APP_SUB_DOMAIN_DEPLOY' => false,   // Whether to open the sub-domain deployment
    'APP_SUB_DOMAIN_RULES'  => array(), // Subdomain deployment rules
    'APP_SUB_DOMAIN_DENY'   => array(), //  Subdomain disabled list
    'APP_GROUP_LIST'        => '',      // Project grouping setting, Multiple groups with commas,For example,'Home,Admin'
    'APP_GROUP_MODE'        =>  0,  // Packet Mode 0 General Packet 1 Independent groups
    'APP_GROUP_PATH'        =>  'Modules', // Grouping directory Independent groupsMode following effective
    'ACTION_SUFFIX'         =>  '', // Operation suffix

    /* Cookie settings */
    'COOKIE_EXPIRE'         => 0,    // Cookie validity
    'COOKIE_DOMAIN'         => '',      // Cookie valid domain name
    'COOKIE_PATH'           => '/',     // Cookie Path
    'COOKIE_PREFIX'         => '',      // Cookie prefix Avoid conflicts

    /* Default setting */
    'DEFAULT_M_LAYER'       =>  'Model', // Default name of the model layer
    'DEFAULT_C_LAYER'       =>  'Action', // Default controller layer name
    'DEFAULT_APP'           => '@',     // Default project name, @ indicates that the current project
    'DEFAULT_LANG'          => 'id-id', // Default language
    'DEFAULT_THEME'         => '',	// Default template Subject Name
    'DEFAULT_GROUP'         => 'Home',  // Default grouping
    'DEFAULT_MODULE'        => 'Index', // Default module name
    'DEFAULT_ACTION'        => 'index', // Default operation name
    'DEFAULT_CHARSET'       => 'utf-8', // Default output encoding
    'DEFAULT_TIMEZONE'      => 'PRC',	// Default time zone
    'DEFAULT_AJAX_RETURN'   => 'JSON',  // Default AJAX Return data format, Optional JSON XML ...
    'DEFAULT_JSONP_HANDLER' => 'jsonpReturn', // The default format returned JSONP approach
    'DEFAULT_FILTER'        => 'htmlspecialchars', // Default parameter filtering methods Use for $this->_get('Variable name');$this->_post('Variable name')...

    /* Database Settings */
    'DB_TYPE'               => 'mysql',     // Database Type
    'DB_HOST'               => 'localhost', // Server Address
    'DB_NAME'               => '',          // Database name
    'DB_USER'               => 'root',      // Username
    'DB_PWD'                => '',          // Password
    'DB_PORT'               => '',        // Port
    'DB_PREFIX'             => 'sen_',    // Database table prefix
    'DB_FIELDTYPE_CHECK'    => false,       // Checking whether a field type
    'DB_FIELDS_CACHE'       => true,        // Enabling field cache
    'DB_CHARSET'            => 'utf8',      // Database encoding defaults to utf8
    'DB_DEPLOY_TYPE'        => 0, // Database deployment:0 Centralized(Single Server), 1 Distributed(Master-slave server)
    'DB_RW_SEPARATE'        => false,       // Whether separate database literacy Master-slave effective
    'DB_MASTER_NUM'         => 1, // Read and write after separation Number of master server
    'DB_SLAVE_NO'           => '', // Specifies the Server serial number
    'DB_SQL_BUILD_CACHE'    => false, // Create a SQL database query cache
    'DB_SQL_BUILD_QUEUE'    => 'file',   // SQL cache buffer queue mode Support file xcache and apc
    'DB_SQL_BUILD_LENGTH'   => 20, // SQL cache queue length
    'DB_SQL_LOG'            => false, // SQL execution logging

    /* Data cache settings */
    'DATA_CACHE_TIME'       => 0,      // Valid data cache 0 indicates persistent cache
    'DATA_CACHE_COMPRESS'   => false,   // Whether to compress data buffer cache
    'DATA_CACHE_CHECK'      => false,   // Whether parity cache data cache
    'DATA_CACHE_PREFIX'     => '',     // Cache prefix
    'DATA_CACHE_TYPE'       => 'File',  // Data cache type, support:File|Db|Apc|Memcache|Shmop|Sqlite|Xcache|Apachenote|Eaccelerator
    'DATA_CACHE_PATH'       => TEMP_PATH,// Cache path settings (File caching is only effective way)
    'DATA_CACHE_SUBDIR'     => false,    // Using cache subdirectory (Automatically creates a subdirectory cache hash mark)
    'DATA_PATH_LEVEL'       => 1,        // Subdirectory cache levels

    /* Incorrect settings */
    'ERROR_MESSAGE'         => 'Page Error ! Please try again later ~',//Error display information, Non-debug mode is active
    'ERROR_PAGE'            => '',	// Misdirected page
    'SHOW_ERROR_MSG'        => false,    // Display an error message
    'TRACE_EXCEPTION'       => false,   // TRACE is throwing an exception error message Methods for trace

    /* Log Settings */
    'LOG_RECORD'            => false,   // Default log does not record
    'LOG_TYPE'              => 3, // Logging Type 0 System 1 Mail 3 File 4 SAPI The default mode for the file
    'LOG_DEST'              => '', // Logging Target
    'LOG_EXTRA'             => '', // Logging additional information
    'LOG_LEVEL'             => 'EMERG,ALERT,CRIT,ERR',// Allows you to record the log level
    'LOG_FILE_SIZE'         => 2097152,	// Log file size limit
    'LOG_EXCEPTION_RECORD'  => false,    // Whether to log exception information log

    /* SESSION settings */
    'SESSION_AUTO_START'    => true,    // Whether to automatically open Session
    'SESSION_OPTIONS'       => array(), // session Configuration Array Supporttype name id path expire domian And other parameters
    'SESSION_TYPE'          => '', // session hander type No need to set the default Unless extended session hander Drive
    'SESSION_PREFIX'        => '', // session Prefix
    //'VAR_SESSION_ID'      => 'session_id',     //Submission of variable sessionID

    /* Template engine settings */
    'TMPL_CONTENT_TYPE'     => 'text/html', // Default Template Output Type
    'TMPL_ACTION_ERROR'     => SEN_PATH.'Tpl/dispatch_jump.tpl', // Jump corresponding default error template file
    'TMPL_ACTION_SUCCESS'   => SEN_PATH.'Tpl/dispatch_jump.tpl', // Successful jumps corresponding default template file
    'TMPL_EXCEPTION_FILE'   => SEN_PATH.'Tpl/sen_exception.tpl',// Exception page template file
    'TMPL_DETECT_THEME'     => false,       // Automatically detects the template theme
    'TMPL_TEMPLATE_SUFFIX'  => '.html',     // The default template file suffix
    'TMPL_FILE_DEPR'        =>  '/', //MODULE_NAME and ACTION_NAME template file delimiters between

    /* URL setting */
    'URL_CASE_INSENSITIVE'  => false,   // Default false indicates that the URL is case sensitive true indicates a case-insensitive
    'URL_MODEL'             => 1,       // URL access mode, optional parameters 0,1,2,3, represent the following four modes:
    // 0 (Normal mode); 1 (PATHINFO Mode); 2 (REWRITE  Mode); 3 (Compatibility Mode)  The default is PATHINFO Mode, provide the best user experience and SEOSupport
    'URL_PATHINFO_DEPR'     => '/',	// PATHINFOMode , the division among the parameters symbols
    'URL_PATHINFO_FETCH'    =>   'ORIG_PATH_INFO,REDIRECT_PATH_INFO,REDIRECT_URL', // Use for compatible judge PATH_INFO SERVER parameter substitution variables list
    'URL_HTML_SUFFIX'       => '',  // URL suffix settings pseudo-static
    'URL_PARAMS_BIND'       =>  true, // URL variables bound to the Action method parameters
    'URL_404_REDIRECT'      =>  '', // 404 Jump page Effective Deployment Mode

    /* System variable name setting */
    'VAR_GROUP'             => 'g',     // Default grouping get variable
    'VAR_MODULE'            => 'm',		// Default module for variable
    'VAR_ACTION'            => 'a',		// Default action for the variable
    'VAR_AJAX_SUBMIT'       => 'ajax',  // The default AJAX submit variables
    'VAR_JSONP_HANDLER'     => 'callback',
    'VAR_PATHINFO'          => 's',	// PATHINFO Compatibility Mode Gets variables such as ?s=/module/action/id/1 Subsequent parameters depend URL_PATHINFO_DEPR
    'VAR_URL_PARAMS'        => '_URL_', // PATHINFO URL parameter variables
    'VAR_TEMPLATE'          => 't',		// The default template switching variable
    'VAR_FILTERS'           =>  'filter_exp',     // Global System variables default filtering method Multiple comma-separated

    'OUTPUT_ENCODE'         =>  true, // Page compressed output
    'HTTP_CACHE_CONTROL'    =>  'private', // Web Cache Control

);