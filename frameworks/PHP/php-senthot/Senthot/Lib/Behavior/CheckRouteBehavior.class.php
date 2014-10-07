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
 * System behavior extension : Routing Detection
 * @category	Sen
 * @package		Sen
 * @subpackage  Behavior
 * @author		ms134n <ms134n@gmail.com>
 */
class CheckRouteBehavior extends Behavior {
    // Behavioral parameters defined ( the default value ) Covered in the project configuration
    protected $options   =  array(
        'URL_ROUTER_ON'         => false,   // Whether to open the URL routing
        'URL_ROUTE_RULES'       => array(), // The default routing rules, Note : Packet configuration can not be replaced
        );

    // Behavior extension execution entry must be run
    public function run(&$return){
        // Priority detect the presence of PATH_INFO
        $regx = trim($_SERVER['PATH_INFO'],'/');
        if(empty($regx)) return $return = true;
        // Whether to open the route to use
        if(!C('URL_ROUTER_ON')) return $return = false;
        // Routing config definition files take precedence over the configuration definition
        $routes = C('URL_ROUTE_RULES');
        // Route processing
        if(!empty($routes)) {
            $depr = C('URL_PATHINFO_DEPR');
            // Replace separator Ensure that routing defined delimiters using uniform
            $regx = str_replace($depr,'/',$regx);
            foreach ($routes as $rule=>$route){
                if(0===strpos($rule,'/') && preg_match($rule,$regx,$matches)) { // Regular routes
                    return $return = $this->parseRegex($matches,$route,$regx);
                }else{ // Routing rules
                    $len1   =   substr_count($regx,'/');
                    $len2   =   substr_count($rule,'/');
                    if($len1>=$len2) {
                        if('$' == substr($rule,-1,1)) {// Exact hits
                            if($len1 != $len2) {
                                continue;
                            }else{
                                $rule =  substr($rule,0,-1);
                            }
                        }
                        $match  =  $this->checkUrlMatch($regx,$rule);
                        if($match)  return $return = $this->parseRule($rule,$route,$regx);
                    }
                }
            }
        }
        $return = false;
    }

    // Detecting URL and rules route matches
    private function checkUrlMatch($regx,$rule) {
        $m1 = explode('/',$regx);
        $m2 = explode('/',$rule);
        $match = true; // Match
        foreach ($m2 as $key=>$val){
            if(':' == substr($val,0,1)) {// Dynamic variables
                if(strpos($val,'\\')) {
                    $type = substr($val,-1);
                    if('d'==$type && !is_numeric($m1[$key])) {
                        $match = false;
                        break;
                    }
                }elseif(strpos($val,'^')){
                    $array   =  explode('|',substr(strstr($val,'^'),1));
                    if(in_array($m1[$key],$array)) {
                        $match = false;
                        break;
                    }
                }
            }elseif(0 !== strcasecmp($val,$m1[$key])){
                $match = false;
                break;
            }
        }
        return $match;
    }

    // Analytical standard routing address
    // Address Format [Grouping/Module/Operating?]Parameter1=Value1&Parameter2=Value2...
    private function parseUrl($url) {
        $var  =  array();
        if(false !== strpos($url,'?')) { // [Grouping/Module/Operating?]Parameter1=Value1&Parameter2=Value2...
            $info   =  parse_url($url);
            $path   = explode('/',$info['path']);
            parse_str($info['query'],$var);
        }elseif(strpos($url,'/')){ // [Grouping/Module/Operating]
            $path = explode('/',$url);
        }else{ // Parameter1=Value1&Parameter2=Value2...
            parse_str($url,$var);
        }
        if(isset($path)) {
            $var[C('VAR_ACTION')] = array_pop($path);
            if(!empty($path)) {
                $var[C('VAR_MODULE')] = array_pop($path);
            }
            if(!empty($path)) {
                $var[C('VAR_GROUP')]  = array_pop($path);
            }
        }
        return $var;
    }

    // Routing parsing rules
    // 'Routing rules'=>'[Grouping/Module/Operating]?Additional parameters 1=Value1&Additional parameter 2=Value2...'
    // 'Routing rules'=>array('[Grouping/Module/Operating]','Additional parameters 1=Value1&Additional parameter 2=Value2...')
    // 'Routing rules'=>'External Address'
    // 'Routing rules'=>array('External Address','Redirect code')
    // Routing rule :Beginning Represents a dynamic variable
    // External address can be used in a dynamic variable Adoption :1 :2 Way
    // 'news/:month/:day/:id'=>array('News/read?cate=1','status=1'),
    // 'new/:id'=>array('/new.php?id=:1',301), Redirect
    private function parseRule($rule,$route,$regx) {
        // Rules for routing address
        $url   =  is_array($route)?$route[0]:$route;
        // Get the URL address of the parameter
        $paths = explode('/',$regx);
        // Resolution routing rules
        $matches  =  array();
        $rule =  explode('/',$rule);
        foreach ($rule as $item){
            if(0===strpos($item,':')) { // Dynamic variables for
                if($pos = strpos($item,'^') ) {
                    $var  =  substr($item,1,$pos-1);
                }elseif(strpos($item,'\\')){
                    $var  =  substr($item,1,-2);
                }else{
                    $var  =  substr($item,1);
                }
                $matches[$var] = array_shift($paths);
            }else{ // Static variables in the URL filtering
                array_shift($paths);
            }
        }
        if(0=== strpos($url,'/') || 0===strpos($url,'http')) { // Jump Route Redirection
            if(strpos($url,':')) { // Passing dynamic parameters
                $values  =  array_values($matches);
                $url  =  preg_replace('/:(\d+)/e','$values[\\1-1]',$url);
            }
            header("Location: $url", true,(is_array($route) && isset($route[1]))?$route[1]:301);
            exit;
        }else{
            // Resolve routing address
            $var  =  $this->parseUrl($url);
            // Resolve routing address inside the dynamic parameters
            $values  =  array_values($matches);
            foreach ($var as $key=>$val){
                if(0===strpos($val,':')) {
                    $var[$key] =  $values[substr($val,1)-1];
                }
            }
            $var   =   array_merge($matches,$var);
            // Parsing URL parameters remaining
            if($paths) {
                preg_replace('@(\w+)\/([^\/]+)@e', '$var[strtolower(\'\\1\')]=strip_tags(\'\\2\');', implode('/',$paths));
            }
            // Automatic transmission parameter parsing routing
            if(is_array($route) && isset($route[1])) {
                parse_str($route[1],$params);
                $var   =   array_merge($var,$params);
            }
            $_GET   =  array_merge($var,$_GET);
        }
        return true;
    }

    // Parsing regular route
    // 'Regular routes'=>'[Grouping/Module/Operating]?Parameter1=Value1&Parameter2=Value2...'
    // 'Regular routes'=>array('[Grouping/Module/Operating]?Parameter1=Value1&Parameter2=Value2...','Additional parameters 1=Value1&Additional parameter 2=Value2...')
    // 'Regular routes'=>'External Address'
    // 'Regular routes'=>array('External Address','Redirect code')
    // Parameter value and the external address can be used in a dynamic variable Adoption :1 :2 Way
    // '/new\/(\d+)\/(\d+)/'=>array('News/read?id=:1&page=:2&cate=1','status=1'),
    // '/new\/(\d+)/'=>array('/new.php?id=:1&page=:2&status=1','301'), Redirect
    private function parseRegex($matches,$route,$regx) {
        // Rules for routing address
        $url   =  is_array($route)?$route[0]:$route;
        $url   =  preg_replace('/:(\d+)/e','$matches[\\1]',$url);
        if(0=== strpos($url,'/') || 0===strpos($url,'http')) { // Jump Route Redirection
            header("Location: $url", true,(is_array($route) && isset($route[1]))?$route[1]:301);
            exit;
        }else{
            // Resolve routing address
            $var  =  $this->parseUrl($url);
            // Parsing URL parameters remaining
            $regx =  substr_replace($regx,'',0,strlen($matches[0]));
            if($regx) {
                preg_replace('@(\w+)\/([^,\/]+)@e', '$var[strtolower(\'\\1\')]=strip_tags(\'\\2\');', $regx);
            }
            // Automatic transmission parameter parsing routing
            if(is_array($route) && isset($route[1])) {
                parse_str($route[1],$params);
                $var   =   array_merge($var,$params);
            }
            $_GET   =  array_merge($var,$_GET);
        }
        return true;
    }
}