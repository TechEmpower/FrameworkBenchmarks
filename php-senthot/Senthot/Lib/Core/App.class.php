<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

/**
 * Senthot Application class Execution of the application process management
 * You can redefine schema extensions However, the interface must have a Run method
 * @category	Sen
 * @package		Sen
 * @subpackage  Core
 * @author		ms134n <ms134n@gmail.com>
 */
class App {

    /**
     * Application initialization
     * @access public
     * @return void
     */
    static public function init() {
        // Set the system time zone
        date_default_timezone_set(C('DEFAULT_TIMEZONE'));
        // Public load dynamic project files and configuration
        load_ext_file();
        // URL dispatcher
        Dispatcher::dispatch();

        // System constants defined for the current request
        define('NOW_TIME',      $_SERVER['REQUEST_TIME']);
        define('REQUEST_METHOD',$_SERVER['REQUEST_METHOD']);
        define('IS_GET',        REQUEST_METHOD =='GET' ? true : false);
        define('IS_POST',       REQUEST_METHOD =='POST' ? true : false);
        define('IS_PUT',        REQUEST_METHOD =='PUT' ? true : false);
        define('IS_DELETE',     REQUEST_METHOD =='DELETE' ? true : false);
        define('IS_AJAX',       ((isset($_SERVER['HTTP_X_REQUESTED_WITH']) && strtolower($_SERVER['HTTP_X_REQUESTED_WITH']) == 'xmlhttprequest') || !empty($_POST[C('VAR_AJAX_SUBMIT')]) || !empty($_GET[C('VAR_AJAX_SUBMIT')])) ? true : false);

        // URL dispatcher end tag
        tag('url_dispatch');         
        // Page Compression Output Support
        if(C('OUTPUT_ENCODE')){
            $zlib = ini_get('zlib.output_compression');
            if(empty($zlib)) ob_start('ob_gzhandler');
        }
        // Security filtering system variables
        if(C('VAR_FILTERS')) {
            $filters    =   explode(',',C('VAR_FILTERS'));
            foreach($filters as $filter){
                // Global Parameters filter
                array_walk_recursive($_POST,$filter);
                array_walk_recursive($_GET,$filter);
            }
        }

        /* Get the name of the template topic */
        $templateSet =  C('DEFAULT_THEME');
        if(C('TMPL_DETECT_THEME')) {// Automatically detects the template theme
            $t = C('VAR_TEMPLATE');
            if (isset($_GET[$t])){
                $templateSet = $_GET[$t];
            }elseif(cookie('sen_template')){
                $templateSet = cookie('sen_template');
            }
            if(!in_array($templateSet,explode(',',C('THEME_LIST')))){
                $templateSet =  C('DEFAULT_THEME');
            }
            cookie('sen_template',$templateSet,864000);
        }
        /* Template Related Category constants */
        define('THEME_NAME',   $templateSet);                  // Current template theme name
        $group   =  defined('GROUP_NAME')?GROUP_NAME.'/':'';
        if(1==C('APP_GROUP_MODE')){ // Independent Packet Mode
            define('THEME_PATH',   BASE_LIB_PATH.basename(TMPL_PATH).'/'.(THEME_NAME?THEME_NAME.'/':''));
            define('APP_TMPL_PATH',__ROOT__.'/'.APP_NAME.(APP_NAME?'/':'').C('APP_GROUP_PATH').'/'.$group.basename(TMPL_PATH).'/'.(THEME_NAME?THEME_NAME.'/':''));
        }else{ 
            define('THEME_PATH',   TMPL_PATH.$group.(THEME_NAME?THEME_NAME.'/':''));
            define('APP_TMPL_PATH',__ROOT__.'/'.APP_NAME.(APP_NAME?'/':'').basename(TMPL_PATH).'/'.$group.(THEME_NAME?THEME_NAME.'/':''));
        }        

        C('CACHE_PATH',CACHE_PATH.$group);
        //Dynamic Configuration TMPL_EXCEPTION_FILE, to absolute address
        C('TMPL_EXCEPTION_FILE',realpath(C('TMPL_EXCEPTION_FILE')));
        return ;
    }

    /**
     * Execution of the application
     * @access public
     * @return void
     */
    static public function exec() {
        if(!preg_match('/^[A-Za-z](\w)*$/',MODULE_NAME)){ // Safety testing
            $module  =  false;
        }else{
            //Creating Action controller instance
            $group   =  defined('GROUP_NAME') && C('APP_GROUP_MODE')==0 ? GROUP_NAME.'/' : '';
            $module  =  A($group.MODULE_NAME);
        }

        if(!$module) {
            if('4e5e5d7364f443e28fbf0d3ae744a59a' == MODULE_NAME) {
                header("Content-type:image/png");
                exit(base64_decode(App::logo()));
            }
            if(function_exists('__hack_module')) {
                // hack Define the expansion module Back Action object
                $module = __hack_module();
                if(!is_object($module)) {
                    // No longer continue Direct return
                    return ;
                }
            }else{
                // Whether the definition of Empty module
                $module = A($group.'Empty');
                if(!$module){
                    _404(L('_MODULE_NOT_EXIST_').':'.MODULE_NAME);
                }
            }
        }
        // Get the current operation name Support dynamic routing
        $action = C('ACTION_NAME')?C('ACTION_NAME'):ACTION_NAME;
        C('TEMPLATE_NAME',THEME_PATH.MODULE_NAME.C('TMPL_FILE_DEPR').$action.C('TMPL_TEMPLATE_SUFFIX'));
        $action .=  C('ACTION_SUFFIX');
        try{
            if(!preg_match('/^[A-Za-z](\w)*$/',$action)){
                // Illegal Operation
                throw new ReflectionException();
            }
            //Perform the current operation
            $method =   new ReflectionMethod($module, $action);
            if($method->isPublic()) {
                $class  =   new ReflectionClass($module);
                // Pre-operation
                if($class->hasMethod('_before_'.$action)) {
                    $before =   $class->getMethod('_before_'.$action);
                    if($before->isPublic()) {
                        $before->invoke($module);
                    }
                }
                // URL parameter binding detection
                if(C('URL_PARAMS_BIND') && $method->getNumberOfParameters()>0){
                    switch($_SERVER['REQUEST_METHOD']) {
                        case 'POST':
                            $vars    =  $_POST;
                            break;
                        case 'PUT':
                            parse_str(file_get_contents('php://input'), $vars);
                            break;
                        default:
                            $vars  =  $_GET;
                    }
                    $params =  $method->getParameters();
                    foreach ($params as $param){
                        $name = $param->getName();
                        if(isset($vars[$name])) {
                            $args[] =  $vars[$name];
                        }elseif($param->isDefaultValueAvailable()){
                            $args[] = $param->getDefaultValue();
                        }else{
                            throw_exception(L('_PARAM_ERROR_').':'.$name);
                        }
                    }
                    $method->invokeArgs($module,$args);
                }else{
                    $method->invoke($module);
                }
                // Rear Operation
                if($class->hasMethod('_after_'.$action)) {
                    $after =   $class->getMethod('_after_'.$action);
                    if($after->isPublic()) {
                        $after->invoke($module);
                    }
                }
            }else{
                // Method of operation is not Public throw an exception
                throw new ReflectionException();
            }
        } catch (ReflectionException $e) { 
            // Method call after an exception occurs Directed to __call approach
            $method = new ReflectionMethod($module,'__call');
            $method->invokeArgs($module,array($action,''));
        }
        return ;
    }

    /**
     * Run Applications Import files using the shortcut method
     * @access public
     * @return void
     */
    static public function run() {
        // Project Initialization tab
        tag('app_init');
        App::init();
        // Project start tag
        tag('app_begin');
        // Session Initialization
        session(C('SESSION_OPTIONS'));
        // Record application initialization time
        G('initTime');
        App::exec();
        // Tag end of the project
        tag('app_end');
        // Save the log records
        if(C('LOG_RECORD')) Log::save();
        return ;
    }

    static public function logo(){
        return 'iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAMAAABg3Am1AAAANlBMVEUAZsz////G099Zn8HE0d7y9/zz+P33+v0ie7wjfLwkfLzu8fNeocNtqsn1+f1xrcp2r8yvzel/Y2NuAAAA20lEQVR4Xt2RSw4DIQxDx8x/+r//ZStNo5rIQgnLllUW7xFjhh86pZMHSicPTJ08UHr4baCR4Xdbk+dtmJI8x5LlaaR5por6sYE7cjyQ2PGl7nAGgvxqlKj/wBDeDEi7zfcC1hO8oTwNG8WQPI/KgLZL/uCVTCVdaX5vzGrovwI3b8wns7v/u3jjWRnGv8jrDtRdyf1inPzVv3izSQzq48dYXf6mwa6EF8O1u7o8oTG28rfaXVyf8Y7F5Y8N4UOD+WND+dhgnpwR8dou+8zuYP6kQT6fquuQ/8PzBqPwA96+q0kmAAAAAElFTkSuQmCC';
    }
}