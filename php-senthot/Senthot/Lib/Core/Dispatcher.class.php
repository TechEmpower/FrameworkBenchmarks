<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

/**
 * Senthot built Dispatcher class
 * Complete URL parsing , routing and scheduling
 * @category	Sen
 * @package		Sen
 * @subpackage  Core
 * @author		ms134n <ms134n@gmail.com>
 */
class Dispatcher {

    /**
     * URL mapping to the controller
     * @access public
     * @return void
     */
    static public function dispatch() {
        $urlMode  =  C('URL_MODEL');
        if(!empty($_GET[C('VAR_PATHINFO')])) { // Determine whether there is compatibility mode inside the URL parameters
            $_SERVER['PATH_INFO']   = $_GET[C('VAR_PATHINFO')];
            unset($_GET[C('VAR_PATHINFO')]);
        }
        if($urlMode == URL_COMPAT ){
            // Compatibility mode judgment
            define('PHP_FILE',_PHP_FILE_.'?'.C('VAR_PATHINFO').'=');
        }elseif($urlMode == URL_REWRITE ) {
            //Current projects address
            $url    =   dirname(_PHP_FILE_);
            if($url == '/' || $url == '\\')
                $url    =   '';
            define('PHP_FILE',$url);
        }else {
            //Current projects address
            define('PHP_FILE',_PHP_FILE_);
        }

        // On sub-domain deployment
        if(C('APP_SUB_DOMAIN_DEPLOY')) {
            $rules      = C('APP_SUB_DOMAIN_RULES');
            $subDomain  = strtolower(substr($_SERVER['HTTP_HOST'],0,strpos($_SERVER['HTTP_HOST'],'.')));
            define('SUB_DOMAIN',$subDomain); // Two domain defined
            if($subDomain && isset($rules[$subDomain])) {
                $rule =  $rules[$subDomain];
            }elseif(isset($rules['*'])){ // Pan-domain name Support
                if('www' != $subDomain && !in_array($subDomain,C('APP_SUB_DOMAIN_DENY'))) {
                    $rule =  $rules['*'];
                }
            }
            if(!empty($rule)) {
                // Subdomain deployment rules 'Subdomain'=>array('Group name/[Module name]','var1=a&var2=b');
                $array  =   explode('/',$rule[0]);
                $module =   array_pop($array);
                if(!empty($module)) {
                    $_GET[C('VAR_MODULE')]  =   $module;
                    $domainModule           =   true;
                }
                if(!empty($array)) {
                    $_GET[C('VAR_GROUP')]   =   array_pop($array);
                    $domainGroup            =   true;
                }
                if(isset($rule[1])) { // Incoming parameters
                    parse_str($rule[1],$parms);
                    $_GET   =  array_merge($_GET,$parms);
                }
            }
        }
        // Analysis PATHINFO information
        if(empty($_SERVER['PATH_INFO'])) {
            $types   =  explode(',',C('URL_PATHINFO_FETCH'));
            foreach ($types as $type){
                if(0===strpos($type,':')) {// Support function to determine
                    $_SERVER['PATH_INFO'] =   call_user_func(substr($type,1));
                    break;
                }elseif(!empty($_SERVER[$type])) {
                    $_SERVER['PATH_INFO'] = (0 === strpos($_SERVER[$type],$_SERVER['SCRIPT_NAME']))?
                        substr($_SERVER[$type], strlen($_SERVER['SCRIPT_NAME']))   :  $_SERVER[$type];
                    break;
                }
            }
        }
        $depr = C('URL_PATHINFO_DEPR');
        if(!empty($_SERVER['PATH_INFO'])) {
            tag('path_info');
            $part =  pathinfo($_SERVER['PATH_INFO']);
            define('__EXT__', isset($part['extension'])?strtolower($part['extension']):'');
            if(C('URL_HTML_SUFFIX')) {
                $_SERVER['PATH_INFO'] = preg_replace('/\.('.trim(C('URL_HTML_SUFFIX'),'.').')$/i', '', $_SERVER['PATH_INFO']);
            }elseif(__EXT__) {
                $_SERVER['PATH_INFO'] = preg_replace('/.'.__EXT__.'$/i','',$_SERVER['PATH_INFO']);
            }
            if(!self::routerCheck()){   // Detect routing rules If you do not press the default rule scheduling URL
                $paths = explode($depr,trim($_SERVER['PATH_INFO'],'/'));
                if(C('VAR_URL_PARAMS')) {
                    // Directly through $_GET['_URL_'][1] $_GET['_URL_'][2] Get URL parameters Convenient access without routing parameters
                    $_GET[C('VAR_URL_PARAMS')]   =  $paths;
                }
                $var  =  array();
                if (C('APP_GROUP_LIST') && !isset($_GET[C('VAR_GROUP')])){
                    $var[C('VAR_GROUP')] = in_array(strtolower($paths[0]),explode(',',strtolower(C('APP_GROUP_LIST'))))? array_shift($paths) : '';
                    if(C('APP_GROUP_DENY') && in_array(strtolower($var[C('VAR_GROUP')]),explode(',',strtolower(C('APP_GROUP_DENY'))))) {
                        // Prohibit direct access to packet
                        exit;
                    }
                }
                if(!isset($_GET[C('VAR_MODULE')])) {// Have not defined module name
                    $var[C('VAR_MODULE')]  =   array_shift($paths);
                }
                $var[C('VAR_ACTION')]  =   array_shift($paths);
                // Parsing URL parameters remaining
                preg_replace('@(\w+)\/([^\/]+)@e', '$var[\'\\1\']=strip_tags(\'\\2\');', implode('/',$paths));
                $_GET   =  array_merge($var,$_GET);
            }
            define('__INFO__',$_SERVER['PATH_INFO']);
        }

        // URL Constants
        define('__SELF__',strip_tags($_SERVER['REQUEST_URI']));
        // Current projects address
        define('__APP__',strip_tags(PHP_FILE));

        // Get Grouping Module and action names
        if (C('APP_GROUP_LIST')) {
            define('GROUP_NAME', self::getGroup(C('VAR_GROUP')));
            // URL address of the packet
            define('__GROUP__',(!empty($domainGroup) || strtolower(GROUP_NAME) == strtolower(C('DEFAULT_GROUP')) )?__APP__ : __APP__.'/'.GROUP_NAME);
        }
        
        // Define the project based on the load path
        define('BASE_LIB_PATH', (defined('GROUP_NAME') && C('APP_GROUP_MODE')==1) ? APP_PATH.C('APP_GROUP_PATH').'/'.GROUP_NAME.'/' : LIB_PATH);
        if(defined('GROUP_NAME')) {
            if(1 == C('APP_GROUP_MODE')){ // Independent Packet Mode
                $config_path    =   BASE_LIB_PATH.'Conf/';
                $common_path    =   BASE_LIB_PATH.'Common/';
            }else{ // General Packet Mode
                $config_path    =   CONF_PATH.GROUP_NAME.'/';
                $common_path    =   COMMON_PATH.GROUP_NAME.'/';             
            }
            // Grouping configuration file to load
            if(is_file($config_path.'config.php'))
                C(include $config_path.'config.php');
            // Loading grouping alias definitions
            if(is_file($config_path.'alias.php'))
                alias_import(include $config_path.'alias.php');            
            // Grouping function to load the file
            if(is_file($common_path.'function.php'))
                include $common_path.'function.php';

        }        
        define('MODULE_NAME',self::getModule(C('VAR_MODULE')));
        define('ACTION_NAME',self::getAction(C('VAR_ACTION')));
        
        // Current module and packet address
        $moduleName    =   defined('MODULE_ALIAS')?MODULE_ALIAS:MODULE_NAME;
        if(defined('GROUP_NAME')) {
            define('__URL__',!empty($domainModule)?__GROUP__.$depr : __GROUP__.$depr.$moduleName);
        }else{
            define('__URL__',!empty($domainModule)?__APP__.'/' : __APP__.'/'.$moduleName);
        }
        // Address the current operation
        define('__ACTION__',__URL__.$depr.(defined('ACTION_ALIAS')?ACTION_ALIAS:ACTION_NAME));
        //Guarantee $_REQUEST Normal values
        $_REQUEST = array_merge($_POST,$_GET);
    }

    /**
     * Routing Detection
     * @access public
     * @return void
     */
    static public function routerCheck() {
        $return   =  false;
        // Routing detection tag
        tag('route_check',$return);
        return $return;
    }

    /**
     * Get the actual name of the module
     * @access private
     * @return string
     */
    static private function getModule($var) {
        $module = (!empty($_GET[$var])? $_GET[$var]:C('DEFAULT_MODULE'));
        unset($_GET[$var]);
        if($maps = C('URL_MODULE_MAP')) {
            if(isset($maps[strtolower($module)])) {
                // Record current alias
                define('MODULE_ALIAS',strtolower($module));
                // Get the actual name of the module
                return   $maps[MODULE_ALIAS];
            }elseif(array_search(strtolower($module),$maps)){
                // Prohibit access to the original module
                return   '';
            }
        }
        if(C('URL_CASE_INSENSITIVE')) {
            // URL address is not case sensitive
            // Intelligent recognition method index.php/user_type/index/ Identified UserTypeAction Module
            $module = ucfirst(parse_name($module,1));
        }
        return strip_tags($module);
    }

    /**
     * Get the actual name of the operation
     * @access private
     * @return string
     */
    static private function getAction($var) {
        $action   = !empty($_POST[$var]) ?
            $_POST[$var] :
            (!empty($_GET[$var])?$_GET[$var]:C('DEFAULT_ACTION'));
        unset($_POST[$var],$_GET[$var]);
        if($maps = C('URL_ACTION_MAP')) {
            if(isset($maps[strtolower(MODULE_NAME)])) {
                $maps =   $maps[strtolower(MODULE_NAME)];
                if(isset($maps[strtolower($action)])) {
                    // Record current alias
                    define('ACTION_ALIAS',strtolower($action));
                    // Get the actual name of the operation
                    return   $maps[ACTION_ALIAS];
                }elseif(array_search(strtolower($action),$maps)){
                    // Prohibit access to the original operating
                    return   '';
                }
            }
        }        
        return strip_tags(C('URL_CASE_INSENSITIVE')?strtolower($action):$action);
    }

    /**
     * Get the actual group name
     * @access private
     * @return string
     */
    static private function getGroup($var) {
        $group   = (!empty($_GET[$var])?$_GET[$var]:C('DEFAULT_GROUP'));
        unset($_GET[$var]);
        return strip_tags(C('URL_CASE_INSENSITIVE') ?ucfirst(strtolower($group)):$group);
    }

}