<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

/**
 * Senthot Runtime files Compiled longer loaded
 * @category	Sen
 * @package		Common
 * @author		ms134n <ms134n@gmail.com>
 */
defined('SEN_PATH') or exit();
if(version_compare(PHP_VERSION,'5.2.0','<'))  die('require PHP > 5.2.0 !');

//  Version Information
define('SEN_VERSION', '2.2.2');

//   System Information
if(version_compare(PHP_VERSION,'5.4.0','<')) {
    ini_set('magic_quotes_runtime',0);
    define('MAGIC_QUOTES_GPC',get_magic_quotes_gpc()?True:False);
}else{
    define('MAGIC_QUOTES_GPC',false);
}
define('IS_CGI',substr(PHP_SAPI, 0,3)=='cgi' ? 1 : 0 );
define('IS_WIN',strstr(PHP_OS, 'WIN') ? 1 : 0 );
define('IS_CLI',PHP_SAPI=='cli'? 1   :   0);

// Project Name
defined('APP_NAME') or define('APP_NAME', basename(dirname($_SERVER['SCRIPT_FILENAME'])));

if(!IS_CLI) {
    // Current file name
    if(!defined('_PHP_FILE_')) {
        if(IS_CGI) {
            //CGI/FASTCGIMode under
            $_temp  = explode('.php',$_SERVER['PHP_SELF']);
            define('_PHP_FILE_',    rtrim(str_replace($_SERVER['HTTP_HOST'],'',$_temp[0].'.php'),'/'));
        }else {
            define('_PHP_FILE_',    rtrim($_SERVER['SCRIPT_NAME'],'/'));
        }
    }
    if(!defined('__ROOT__')) {
        // Site URL root
        if( strtoupper(APP_NAME) == strtoupper(basename(dirname(_PHP_FILE_))) ) {
            $_root = dirname(dirname(_PHP_FILE_));
        }else {
            $_root = dirname(_PHP_FILE_);
        }
        define('__ROOT__',   (($_root=='/' || $_root=='\\')?'':$_root));
    }

    //Support the URLMode
    define('URL_COMMON',      0);   //Normal mode
    define('URL_PATHINFO',    1);   //PATHINFOMode
    define('URL_REWRITE',     2);   //REWRITEMode
    define('URL_COMPAT',      3);   // Compatibility Mode
}

// Path settings Can be redefined in the import documents All paths must be in constant/ End
defined('CORE_PATH')    or define('CORE_PATH',      SEN_PATH.'Lib/'); // Core class library System catalog
defined('ADDONS_PATH')  or define('ADDONS_PATH',    SEN_PATH.'Addons/'); // System extensions directory
defined('MODE_PATH')    or define('MODE_PATH',      ADDONS_PATH.'Mode/'); // Mode extension directory
defined('ENGINE_PATH')  or define('ENGINE_PATH',    ADDONS_PATH.'Engine/'); // Engine extensions directory
defined('VENDOR_PATH')  or define('VENDOR_PATH',    ADDONS_PATH.'Vendor/'); // List of third-party class libraries
defined('LIBRARY_PATH') or define('LIBRARY_PATH',   ADDONS_PATH.'Library/'); // Addonsing class library directory
defined('COMMON_PATH')  or define('COMMON_PATH',    APP_PATH.'Common/'); // Public directory
defined('LIB_PATH')     or define('LIB_PATH',       APP_PATH.'Lib/'); // Project class library directory
defined('CONF_PATH')    or define('CONF_PATH',      APP_PATH.'Conf/'); // List of project configurations
defined('LANG_PATH')    or define('LANG_PATH',      APP_PATH.'Lang/'); // Project language pack directory
defined('TMPL_PATH')    or define('TMPL_PATH',      APP_PATH.'Tpl/'); // Project template directory
defined('HTML_PATH')    or define('HTML_PATH',      APP_PATH.'Html/'); // Project static directory
defined('LOG_PATH')     or define('LOG_PATH',       RUNTIME_PATH.'Logs/'); // Project log directory
defined('TEMP_PATH')    or define('TEMP_PATH',      RUNTIME_PATH.'Temp/'); // Project cache directory
defined('DATA_PATH')    or define('DATA_PATH',      RUNTIME_PATH.'Data/'); // Project data directory
defined('CACHE_PATH')   or define('CACHE_PATH',     RUNTIME_PATH.'Cache/'); // Project template cache directory

// In order to facilitate import third-party class libraries Set up Vendor Directory to the include_path
set_include_path(get_include_path() . PATH_SEPARATOR . VENDOR_PATH);

// Load the file you need to run And is responsible for the automatic list generation
function load_runtime_file() {
    // Loading SystemBasic function library
    require SEN_PATH.'Common/common.php';
    // Read the core file list
    $list = array(
        CORE_PATH.'Core/Sen.class.php',
        CORE_PATH.'Core/SenException.class.php',  // Exception class
        CORE_PATH.'Core/Behavior.class.php',
    );
    // Loading Mode file list
    foreach ($list as $key=>$file){
        if(is_file($file))  require_cache($file);
    }
    // System class library is loaded the alias definition
    alias_import(include SEN_PATH.'Conf/alias.php');

    // Check the project directory structure If it does not exist it is created automatically
    if(!is_dir(LIB_PATH)) {
        // To create the project directory structure
        build_app_dir();
    }elseif(!is_dir(CACHE_PATH)){
        // Checking cache directory
        check_runtime();
    }elseif(APP_DEBUG){
        // Toggle Debugging Mode delete the compiler cache
        if(is_file(RUNTIME_FILE))   unlink(RUNTIME_FILE);
    }
}

// Checking cache directory(Runtime) If it does not exist it is created automatically
function check_runtime() {
    if(!is_dir(RUNTIME_PATH)) {
        mkdir(RUNTIME_PATH);
    }elseif(!is_writeable(RUNTIME_PATH)) {
        header('Content-Type:text/html; charset=utf-8');
        exit('Directory [ '.RUNTIME_PATH.' ] Is not writable!');
    }
    mkdir(CACHE_PATH);  // Template cache directory
    if(!is_dir(LOG_PATH))   mkdir(LOG_PATH);    // Log directory
    if(!is_dir(TEMP_PATH))  mkdir(TEMP_PATH);   // Data cache directory
    if(!is_dir(DATA_PATH))  mkdir(DATA_PATH);   // Data file directory
    return true;
}

// Create a compiler cache
function build_runtime_cache($append='') {
    // Generates compiled files
    $defs           = get_defined_constants(TRUE);
    $content        =  '$GLOBALS[\'_beginTime\'] = microtime(TRUE);';
    if(defined('RUNTIME_DEF_FILE')) { // Outside compiled constants file introduction
        file_put_contents(RUNTIME_DEF_FILE,'<?php '.array_define($defs['user']));
        $content   .=  'require \''.RUNTIME_DEF_FILE.'\';';
    }else{
        $content   .= array_define($defs['user']);
    }
    $content       .= 'set_include_path(get_include_path() . PATH_SEPARATOR . VENDOR_PATH);';
    // Core reading list compiled file
    $list = array(
        SEN_PATH.'Common/common.php',
        CORE_PATH.'Core/Sen.class.php',
        CORE_PATH.'Core/SenException.class.php',
        CORE_PATH.'Core/Behavior.class.php',
    );
    foreach ($list as $file){
        $content .= compile($file);
    }
    // Addonsed File System behavior to compile
    $content .= build_tags_cache();
    
    $alias      = include SEN_PATH.'Conf/alias.php';
    $content   .= 'alias_import('.var_export($alias,true).');';
    // Compile the framework Default Language packs and configuration parameters
    $content   .= $append."\nL(".var_export(L(),true).");C(".var_export(C(),true).');G(\'loadTime\');Sen::Start();';
    file_put_contents(RUNTIME_FILE,strip_whitespace('<?php '.str_replace("defined('SEN_PATH') or exit();",' ',$content)));
}

// Compilation System extension class library
function build_tags_cache() {
    $tags = C('extends');
    $content = '';
    foreach ($tags as $tag=>$item){
        foreach ($item as $key=>$name) {
            $content .= is_int($key)?compile(CORE_PATH.'Behavior/'.$name.'Behavior.class.php'):compile($name);
        }
    }
    return $content;
}

// To create the project directory structure
function build_app_dir() {
    // Failed to create project directory is created automatically
    if(!is_dir(APP_PATH)) mkdir(APP_PATH,0755,true);
    if(is_writeable(APP_PATH)) {
        $dirs  = array(
            LIB_PATH,
            RUNTIME_PATH,
            CONF_PATH,
            COMMON_PATH,
            LANG_PATH,
            CACHE_PATH,
            TMPL_PATH,
            TMPL_PATH.C('DEFAULT_THEME').'/',
            LOG_PATH,
            TEMP_PATH,
            DATA_PATH,
            LIB_PATH.'Model/',
            LIB_PATH.'Action/',
            LIB_PATH.'Behavior/',
            LIB_PATH.'Widget/',
            );
        foreach ($dirs as $dir){
            if(!is_dir($dir))  mkdir($dir,0755,true);
        }
        // Write to directory security file
        build_dir_secure($dirs);
        // Write the initial configuration file
        if(!is_file(CONF_PATH.'config.php'))
            file_put_contents(CONF_PATH.'config.php',"<?php\nreturn array(\n\t//'Configuration item'=>'Configuration values'\n);\n?>");
        // Written test Action
        if(!is_file(LIB_PATH.'Action/IndexAction.class.php'))
            build_first_action();
    }else{
        header('Content-Type:text/html; charset=utf-8');
        exit('The project directory is not writable directory cannot be automatically generated!<BR>Please use the project Builder or manually generated list of projects~');
    }
}

// Create a test Action
function build_first_action() {
    $content = file_get_contents(SEN_PATH.'Tpl/default_index.tpl');
    file_put_contents(LIB_PATH.'Action/IndexAction.class.php',$content);
}

// Build directory security file
function build_dir_secure($dirs='') {
    // Write directory security
    if(defined('BUILD_DIR_SECURE') && BUILD_DIR_SECURE) {
        defined('DIR_SECURE_FILENAME')  or define('DIR_SECURE_FILENAME',    'index.html');
        defined('DIR_SECURE_CONTENT')   or define('DIR_SECURE_CONTENT',     ' ');
        // Automatic write to directory security file
        $content = DIR_SECURE_CONTENT;
        $files = explode(',', DIR_SECURE_FILENAME);
        foreach ($files as $filename){
            foreach ($dirs as $dir)
                file_put_contents($dir.$filename,$content);
        }
    }
}

// Required to load the runtime files
load_runtime_file();
// Records the time loading files
G('loadTime');
// Execute the entry
Sen::Start();