<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

/**
 * Senthot Portal category
 * @category	Sen
 * @package		Sen
 * @subpackage  Core
 * @author		ms134n <ms134n@gmail.com>
 */
class Sen {

    private static $_instance = array();

    /**
     * Application initialization
     * @access public
     * @return void
     */
    static public function start() {
        // Setting error and exception handling
        register_shutdown_function(array('Sen','fatalError'));
        set_error_handler(array('Sen','appError'));
        set_exception_handler(array('Sen','appException'));
        // Registered AUTOLOAD method
        spl_autoload_register(array('Sen', 'autoload'));
        //[RUNTIME]
        Sen::buildApp();         // Precompiled project
        //[/RUNTIME]
        // Run the application
        App::run();
        return ;
    }

    //[RUNTIME]
    /**
     * Read configuration information Compile the project
     * @access private
     * @return string
     */
    static private function buildApp() {
        
        // Read operating mode
        if(defined('MODE_NAME')) { // Read mode setting
            $mode   = include MODE_PATH.strtolower(MODE_NAME).'.php';
        }else{
            $mode   =  array();
        }
        // Core Conventions configuration file to load
        C(include SEN_PATH.'Conf/convention.php');
        if(isset($mode['config'])) {// Load mode profile
            C( is_array($mode['config'])?$mode['config']:include $mode['config'] );
        }

        // Load project configuration file
        if(is_file(CONF_PATH.'config.php'))
            C(include CONF_PATH.'config.php');

        // Loading framework underlying language pack
        L(include SEN_PATH.'Lang/'.strtolower(C('DEFAULT_LANG')).'.php');

        // Load model system behavior definition
        if(C('APP_TAGS_ON')) {
            if(isset($mode['extends'])) {
                C('extends',is_array($mode['extends'])?$mode['extends']:include $mode['extends']);
            }else{ // Addonsed definition of default load behavior of the system
                C('extends', include SEN_PATH.'Conf/tags.php');
            }
        }

        // Load application behavior definition
        if(isset($mode['tags'])) {
            C('tags', is_array($mode['tags'])?$mode['tags']:include $mode['tags']);
        }elseif(is_file(CONF_PATH.'tags.php')){
            // Default load the project file defines the configuration directory tags
            C('tags', include CONF_PATH.'tags.php');
        }

        $compile   = '';
        // Core reading list compiled file
        if(isset($mode['core'])) {
            $list  =  $mode['core'];
        }else{
            $list  =  array(
                SEN_PATH.'Common/functions.php', // Standard mode function library
                CORE_PATH.'Core/Log.class.php',    // Log processing class
                CORE_PATH.'Core/Dispatcher.class.php', // URL scheduling class
                CORE_PATH.'Core/App.class.php',   // Application class
                CORE_PATH.'Core/Action.class.php', // Controller class
                CORE_PATH.'Core/View.class.php',  // View class
            );
        }
        // Compile a list of additional core project documents
        if(is_file(CONF_PATH.'core.php')) {
            $list  =  array_merge($list,include CONF_PATH.'core.php');
        }
        foreach ($list as $file){
            if(is_file($file))  {
                require_cache($file);
                if(!APP_DEBUG)   $compile .= compile($file);
            }
        }

        // Load the project public documents
        if(is_file(COMMON_PATH.'common.php')) {
            include COMMON_PATH.'common.php';
            // Compiled file
            if(!APP_DEBUG)  $compile   .= compile(COMMON_PATH.'common.php');
        }

        // Loading mode alias definitions
        if(isset($mode['alias'])) {
            $alias = is_array($mode['alias'])?$mode['alias']:include $mode['alias'];
            alias_import($alias);
            if(!APP_DEBUG) $compile .= 'alias_import('.var_export($alias,true).');';               
        }
     
        // Loading a project alias definitions
        if(is_file(CONF_PATH.'alias.php')){ 
            $alias = include CONF_PATH.'alias.php';
            alias_import($alias);
            if(!APP_DEBUG) $compile .= 'alias_import('.var_export($alias,true).');';
        }

        if(APP_DEBUG) {
            // Debug mode to load the system default configuration file
            C(include SEN_PATH.'Conf/debug.php');
            // Read the debug mode application status
            $status  =  C('APP_STATUS');
            // Project configuration file to load the corresponding
            if(is_file(CONF_PATH.$status.'.php'))
                // Allow the project to increase the development mode configuration definition
                C(include CONF_PATH.$status.'.php');
        }else{
            // Deployment model generates compiled files below
            build_runtime_cache($compile);
        }
        return ;
    }
    //[/RUNTIME]

    /**
     * The system automatically loads the library Senthot
     * And Support configure automatic loading path
     * @param string $class Object class name
     * @return void
     */
    public static function autoload($class) {
        // Check for alias definitions
        if(alias_import($class)) return ;
        $libPath    =   defined('BASE_LIB_PATH')?BASE_LIB_PATH:LIB_PATH;
        $group      =   defined('GROUP_NAME') && C('APP_GROUP_MODE')==0 ?GROUP_NAME.'/':'';
        $file       =   $class.'.class.php';
        if(substr($class,-8)=='Behavior') { // Load Behavior
            if(require_array(array(
                CORE_PATH.'Behavior/'.$file,
                ADDONS_PATH.'Behavior/'.$file,
                LIB_PATH.'Behavior/'.$file,
                $libPath.'Behavior/'.$file),true)
                || (defined('MODE_NAME') && require_cache(MODE_PATH.ucwords(MODE_NAME).'/Behavior/'.$file))) {
                return ;
            }
        }elseif(substr($class,-5)=='Model'){ // Load Model
            if(require_array(array(
                LIB_PATH.'Model/'.$group.$file,
                $libPath.'Model/'.$file,
                ADDONS_PATH.'Model/'.$file),true)) {
                return ;
            }
        }elseif(substr($class,-6)=='Action'){ // Load Controller
            if(require_array(array(
                LIB_PATH.'Action/'.$group.$file,
                $libPath.'Action/'.$file,
                ADDONS_PATH.'Action/'.$file),true)) {
                return ;
            }
        }elseif(substr($class,0,5)=='Cache'){ // Load cache drive
            if(require_array(array(
                ADDONS_PATH.'Driver/Cache/'.$file,
                CORE_PATH.'Driver/Cache/'.$file),true)){
                return ;
            }
        }elseif(substr($class,0,2)=='Db'){ // Load database driver
            if(require_array(array(
                ADDONS_PATH.'Driver/Db/'.$file,
                CORE_PATH.'Driver/Db/'.$file),true)){
                return ;
            }
        }elseif(substr($class,0,8)=='Template'){ // Loading template engine driven
            if(require_array(array(
                ADDONS_PATH.'Driver/Template/'.$file,
                CORE_PATH.'Driver/Template/'.$file),true)){
                return ;
            }
        }elseif(substr($class,0,6)=='TagLib'){ // Load tag library drive
            if(require_array(array(
                ADDONS_PATH.'Driver/TagLib/'.$file,
                CORE_PATH.'Driver/TagLib/'.$file),true)) {
                return ;
            }
        }

        // According to the settings automatically load path try to search
        $paths  =   explode(',',C('APP_AUTOLOAD_PATH'));
        foreach ($paths as $path){
            if(import($path.'.'.$class))
                // If you load the class success, returns
                return ;
        }
    }

    /**
     * Get object instance Support call the class static method
     * @param string $class Object class name
     * @param string $method Static class method name
     * @return object
     */
    static public function instance($class,$method='') {
        $identify   =   $class.$method;
        if(!isset(self::$_instance[$identify])) {
            if(class_exists($class)){
                $o = new $class();
                if(!empty($method) && method_exists($o,$method))
                    self::$_instance[$identify] = call_user_func_array(array(&$o, $method));
                else
                    self::$_instance[$identify] = $o;
            }
            else
                halt(L('_CLASS_NOT_EXIST_').':'.$class);
        }
        return self::$_instance[$identify];
    }

    /**
     * Custom Exception Handling
     * @access public
     * @param mixed $e Exception object
     */
    static public function appException($e) {
        halt($e->__toString());
    }

    /**
     * Custom Error Handling
     * @access public
     * @param int $errno Error Type
     * @param string $errstr Error Messages
     * @param string $errfile Error File
     * @param int $errline Error Rows
     * @return void
     */
    static public function appError($errno, $errstr, $errfile, $errline) {
      switch ($errno) {
          case E_ERROR:
          case E_PARSE:
          case E_CORE_ERROR:
          case E_COMPILE_ERROR:
          case E_USER_ERROR:
            ob_end_clean();
            // Page Compression Output Support
            if(C('OUTPUT_ENCODE')){
                $zlib = ini_get('zlib.output_compression');
                if(empty($zlib)) ob_start('ob_gzhandler');
            }
            $errorStr = "$errstr ".$errfile." subsection $errline line.";
            if(C('LOG_RECORD')) Log::write("[$errno] ".$errorStr,Log::ERR);
            function_exists('halt')?halt($errorStr):exit('ERROR:'.$errorStr);
            break;
          case E_STRICT:
          case E_USER_WARNING:
          case E_USER_NOTICE:
          default:
            $errorStr = "[$errno] $errstr ".$errfile." subsection $errline line.";
            trace($errorStr,'','NOTIC');
            break;
      }
    }
    
    // Fatal error trapping
    static public function fatalError() {
        if ($e = error_get_last()) {
            Sen::appError($e['type'],$e['message'],$e['file'],$e['line']);
        }
    }

}