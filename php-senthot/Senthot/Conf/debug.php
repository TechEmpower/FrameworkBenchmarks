<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

/**
 * Senthot The default configuration file Debug Mode
 * If the project has Debug Mode to define your own configuration file, the file is invalid
 * @category	Sen
 * @package		Common
 * @author		ms134n <ms134n@gmail.com>
 * @version  $Id: debug.php 3071 2012-07-15 07:59:23Z ettynind@gmail.com $
 */
defined('SEN_PATH') or exit();
// Debug Mode The following default settings You can redefine the project configuration directory debug.php cover
return  array(
    'LOG_RECORD'			=>	true,  // Logging
    'LOG_EXCEPTION_RECORD'  => 	true,    // Whether to log exception information log
    'LOG_LEVEL'       		=>  'EMERG,ALERT,CRIT,ERR,WARN,NOTIC,INFO,DEBUG,SQL',  // Allows you to record the log level
    'DB_FIELDS_CACHE'		=> 	false, // Field cache information
    'DB_SQL_LOG'			=>	true, // Logging SQL Information
    'APP_FILE_CASE'  		=>  true, // Whether to check the case file Valid for Windows platforms
    'TMPL_CACHE_ON'    		=> 	false,        // Open the compiled template caching, set to false then every will be recompiled
    'TMPL_STRIP_SPACE'      => 	false,       // Removal of HTML template files spaces and line
    'SHOW_ERROR_MSG'        => 	true,    // Display an error message
);