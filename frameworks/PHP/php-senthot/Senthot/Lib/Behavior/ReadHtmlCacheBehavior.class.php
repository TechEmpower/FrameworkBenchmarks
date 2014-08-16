<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

defined('SEN_PATH') or exit();
/**
 * System behavior extension : static cache reads
 * @category	Sen
 * @package		Sen
 * @subpackage  Behavior
 * @author		ms134n <ms134n@gmail.com>
 */
class ReadHtmlCacheBehavior extends Behavior {
    protected $options   =  array(
            'HTML_CACHE_ON'     =>  false,
            'HTML_CACHE_TIME'   =>  60,
            'HTML_CACHE_RULES'  =>  array(),
            'HTML_FILE_SUFFIX'  =>  '.html',
        );

    // Behavior extension execution entry must be run
    public function run(&$params){
        // On static cache
        if(C('HTML_CACHE_ON'))  {
            $cacheTime = $this->requireHtmlCache();
            if( false !== $cacheTime && $this->checkHTMLCache(HTML_FILE_NAME,$cacheTime)) { //Effective static pages
                // Read static page output
                readfile(HTML_FILE_NAME);
                exit();
            }
        }
    }

    // Determine whether you need static cache
    static private function requireHtmlCache() {
        // Static analysis of the current rules
         $htmls = C('HTML_CACHE_RULES'); // Read static rule
         if(!empty($htmls)) {
            $htmls = array_change_key_case($htmls);
            // Static rules file defines the format actionName=>array('Static rule','Cache Time','Additional rules')
            // 'read'=>array('{id},{name}',60,'md5') Must ensure the uniqueness of static rules and can be judged
            // Detect static rule
            $moduleName = strtolower(MODULE_NAME);
            $actionName = strtolower(ACTION_NAME);
            if(isset($htmls[$moduleName.':'.$actionName])) {
                $html   =   $htmls[$moduleName.':'.$actionName];   // Operation of a module static rule
            }elseif(isset($htmls[$moduleName.':'])){// A module static rules
                $html   =   $htmls[$moduleName.':'];
            }elseif(isset($htmls[$actionName])){
                $html   =   $htmls[$actionName]; // All static rules
            }elseif(isset($htmls['*'])){
                $html   =   $htmls['*']; // Global static rules
            }elseif(isset($htmls['empty:index']) && !class_exists(MODULE_NAME.'Action')){
                $html   =    $htmls['empty:index']; // Empty module static rules
            }elseif(isset($htmls[$moduleName.':_empty']) && $this->isEmptyAction(MODULE_NAME,ACTION_NAME)){
                $html   =    $htmls[$moduleName.':_empty']; // Space operation static rules
            }
            if(!empty($html)) {
                // Understanding static rules
                $rule   = $html[0];
                // By $_ the beginning of system variables
                $rule   = preg_replace('/{\$(_\w+)\.(\w+)\|(\w+)}/e',"\\3(\$\\1['\\2'])",$rule);
                $rule   = preg_replace('/{\$(_\w+)\.(\w+)}/e',"\$\\1['\\2']",$rule);
                // {ID|FUN} GET variable DateFormat
                $rule   = preg_replace('/{(\w+)\|(\w+)}/e',"\\2(\$_GET['\\1'])",$rule);
                $rule   = preg_replace('/{(\w+)}/e',"\$_GET['\\1']",$rule);
                // Special system variables
                $rule   = str_ireplace(
                    array('{:app}','{:module}','{:action}','{:group}'),
                    array(APP_NAME,MODULE_NAME,ACTION_NAME,defined('GROUP_NAME')?GROUP_NAME:''),
                    $rule);
                // {|FUN} Alone function
                $rule  = preg_replace('/{|(\w+)}/e',"\\1()",$rule);
                if(!empty($html[2])) $rule    =   $html[2]($rule); // Apply additional functions
                $cacheTime = isset($html[1])?$html[1]:C('HTML_CACHE_TIME'); // Cache validity period
                // The current cache file
                define('HTML_FILE_NAME',HTML_PATH . $rule.C('HTML_FILE_SUFFIX'));
                return $cacheTime;
            }
        }
        // No cache
        return false;
    }

    /**
     * Check effectiveness of static HTML files
     * If you need to update
     * @access public
     * @param string $cacheFile  A static file name
     * @param integer $cacheTime  Cache validity period
     * @return boolen
     */
    static public function checkHTMLCache($cacheFile='',$cacheTime='') {
        if(!is_file($cacheFile)){
            return false;
        }elseif (filemtime(C('TEMPLATE_NAME')) > filemtime($cacheFile)) {
            // Static files are template files if the update needs to be updated
            return false;
        }elseif(!is_numeric($cacheTime) && function_exists($cacheTime)){
            return $cacheTime($cacheFile);
        }elseif ($cacheTime != 0 && NOW_TIME > filemtime($cacheFile)+$cacheTime) {
            // File is valid
            return false;
        }
        //Static files
        return true;
    }

    //Test whether the action is empty
    static private function isEmptyAction($module,$action) {
        $className  =   $module.'Action';
        $class      =   new $className;
        return !method_exists($class,$action);
    }

}