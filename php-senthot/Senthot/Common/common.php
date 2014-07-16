<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

/**
 * Sen Basic function library
 * @category	Sen
 * @package		Common
 * @author		ms134n <ms134n@gmail.com>
 */

/**
 * Records and statistical time ( microseconds ) and memory usage
 * Use method:
 * <code>
 * G('begin'); // Records the start tag
 * // ... Interval running code
 * G('end'); // End of record tag
 * echo G('begin','end',6); // Running time interval statistics Accurate to six decimal
 * echo G('begin','end','m'); // Memory usage statistics interval
 * If the end tag bit is not defined, it will automatically mark the current position as
 * Which requires memory usage statistics MEMORY_LIMIT_ON Constant is true only valid
 * </code>
 * @param string $start Start tag
 * @param string $end End tag
 * @param integer|string $dec Decimal places or m
 * @return mixed
 */
function G($start,$end='',$dec=4) {
    static $_info       =   array();
    static $_mem        =   array();
    if(is_float($end)) { // Record Time
        $_info[$start]  =   $end;
    }elseif(!empty($end)){ // Time and memory usage statistics
        if(!isset($_info[$end])) $_info[$end]       =  microtime(TRUE);
        if(MEMORY_LIMIT_ON && $dec=='m'){
            if(!isset($_mem[$end])) $_mem[$end]     =  memory_get_usage();
            return number_format(($_mem[$end]-$_mem[$start])/1024);
        }else{
            return number_format(($_info[$end]-$_info[$start]),$dec);
        }

    }else{ // Record Time and memory usage
        $_info[$start]  =  microtime(TRUE);
        if(MEMORY_LIMIT_ON) $_mem[$start]           =  memory_get_usage();
    }
}

/**
 * Set and get statistics
 * Use method:
 * <code>
 * N('db',1); // Record the number of database operations
 * N('read',1); // Record reads
 * echo N('db'); // Get the current page number of all database operations
 * echo N('read'); // Get the current page reads
 * </code>
 * @param string $key Identify the location
 * @param integer $step Step value
 * @return mixed
 */
function N($key, $step=0,$save=false) {
    static $_num    = array();
    if (!isset($_num[$key])) {
        $_num[$key] = (false !== $save)? S('N_'.$key) :  0;
    }
    if (empty($step))
        return $_num[$key];
    else
        $_num[$key] = $_num[$key] + (int) $step;
    if(false !== $save){ // Save Results
        S('N_'.$key,$_num[$key],$save);
    }
}

/**
 * Convert string naming style
 * type 0 The Java-style converted to C style 1 The C-style into Java style
 * @param string $name String
 * @param integer $type Conversion Type
 * @return string
 */
function parse_name($name, $type=0) {
    if ($type) {
        return ucfirst(preg_replace("/_([a-zA-Z])/e", "strtoupper('\\1')", $name));
    } else {
        return strtolower(trim(preg_replace("/[A-Z]/", "_\\0", $name), "_"));
    }
}

/**
 * Optimized require_once
 * @param string $filename File address
 * @return boolean
 */
function require_cache($filename) {
    static $_importFiles = array();
    if (!isset($_importFiles[$filename])) {
        if (file_exists_case($filename)) {
            require $filename;
            $_importFiles[$filename] = true;
        } else {
            $_importFiles[$filename] = false;
        }
    }
    return $_importFiles[$filename];
}

/**
 * Batch import file Success, returns
 * @param array $array Array of files
 * @param boolean $return Loaded successfully whether to return
 * @return boolean
 */
function require_array($array,$return=false){
    foreach ($array as $file){
        if (require_cache($file) && $return) return true;
    }
    if($return) return false;
}

/**
 * Judged case-sensitive file exists
 * @param string $filename File address
 * @return boolean
 */
function file_exists_case($filename) {
    if (is_file($filename)) {
        if (IS_WIN && C('APP_FILE_CASE')) {
            if (basename(realpath($filename)) != basename($filename))
                return false;
        }
        return true;
    }
    return false;
}

/**
 * Import the required class libraries With Java Import This function has the cache function
 * @param string $class Class Library namespace string
 * @param string $baseUrl Start Path
 * @param string $ext Importing the file extension
 * @return boolean
 */
function import($class, $baseUrl = '', $ext='.class.php') {
    static $_file = array();
    $class = str_replace(array('.', '#'), array('/', '.'), $class);
    if ('' === $baseUrl && false === strpos($class, '/')) {
        // Check the alias import
        return alias_import($class);
    }
    if (isset($_file[$class . $baseUrl]))
        return true;
    else
        $_file[$class . $baseUrl] = true;
    $class_strut     = explode('/', $class);
    if (empty($baseUrl)) {
        $libPath    =   defined('BASE_LIB_PATH')?BASE_LIB_PATH:LIB_PATH;
        if ('@' == $class_strut[0] || APP_NAME == $class_strut[0]) {
            //Load current project application library
            $baseUrl = dirname($libPath);
            $class   = substr_replace($class, basename($libPath).'/', 0, strlen($class_strut[0]) + 1);
        }elseif ('sen' == strtolower($class_strut[0])){ // sen Official Base Class Library
            $baseUrl = CORE_PATH;
            $class   = substr($class,6);
        }elseif (in_array(strtolower($class_strut[0]), array('org', 'com'))) {
            // org Third-party public libraries com Corporate public library
            $baseUrl = LIBRARY_PATH;
        }else { // Other items loaded application library
            $class   = substr_replace($class, '', 0, strlen($class_strut[0]) + 1);
            $baseUrl = APP_PATH . '../' . $class_strut[0] . '/'.basename($libPath).'/';
        }
    }
    if (substr($baseUrl, -1) != '/')
        $baseUrl    .= '/';
    $classfile       = $baseUrl . $class . $ext;
    if (!class_exists(basename($class),false)) {
        // If the class does not exist The import library file
        return require_cache($classfile);
    }
}

/**
 * Way based on namespaces imported library
 * load('@.Util.Array')
 * @param string $name Library namespace string
 * @param string $baseUrl Start Path
 * @param string $ext Importing the file extension
 * @return void
 */
function load($name, $baseUrl='', $ext='.php') {
    $name = str_replace(array('.', '#'), array('/', '.'), $name);
    if (empty($baseUrl)) {
        if (0 === strpos($name, '@/')) {
            //Load current project library
            $baseUrl    = COMMON_PATH;
            $name       = substr($name, 2);
        } else {
            //Load Senthot System Function Library
            $baseUrl    = ADDONS_PATH . 'Function/';
        }
    }
    if (substr($baseUrl, -1) != '/')
        $baseUrl       .= '/';
    require_cache($baseUrl . $name . $ext);
}

/**
 * Quickly import third-party frameworks library All third-party frameworks into a unified library files System, Vendor directory
 * @param string $class Library
 * @param string $baseUrl Base directory
 * @param string $ext Library suffix
 * @return boolean
 */
function vendor($class, $baseUrl = '', $ext='.php') {
    if (empty($baseUrl))
        $baseUrl = VENDOR_PATH;
    return import($class, $baseUrl, $ext);
}

/**
 * Quickly define and import alias Support batch Definition
 * @param string|array $alias Alias library
 * @param string $classfile Corresponding library
 * @return boolean
 */
function alias_import($alias, $classfile='') {
    static $_alias = array();
    if (is_string($alias)) {
        if(isset($_alias[$alias])) {
            return require_cache($_alias[$alias]);
        }elseif ('' !== $classfile) {
            // Defining aliases import
            $_alias[$alias] = $classfile;
            return;
        }
    }elseif (is_array($alias)) {
        $_alias   =  array_merge($_alias,$alias);
        return;
    }
    return false;
}

/**
 * D Functions Use for instantiating Model Format Project://Grouping/Module
 * @param string $name Model resource address
 * @param string $layer Business layer name
 * @return Model
 */
function D($name='',$layer='') {
    if(empty($name)) return new Model;
    static $_model  =   array();
    $layer          =   $layer?$layer:C('DEFAULT_M_LAYER');
    if(strpos($name,'://')) {// Specify the project
        $name       =   str_replace('://','/'.$layer.'/',$name);
    }else{
        $name       =   C('DEFAULT_APP').'/'.$layer.'/'.$name;
    }
    if(isset($_model[$name]))   return $_model[$name];
    import($name.$layer);
    $class          =   basename($name.$layer);
    if(class_exists($class)) {
        $model      =   new $class();
    }else {
        $model      =   new Model(basename($name));
    }
    $_model[$name]  =  $model;
    return $model;
}

/**
 * M function Use for instantiating a no model file Model
 * @param string $name Model Name Support base model designation For example, MongoModel:User
 * @param string $tablePrefix Table Prefix
 * @param mixed $connection Database connection information
 * @return Model
 */
function M($name='', $tablePrefix='',$connection='') {
    static $_model  = array();
    if(strpos($name,':')) {
        list($class,$name)    =  explode(':',$name);
    }else{
        $class      =   'Model';
    }
    $guid           =   $tablePrefix . $name . '_' . $class;
    if (!isset($_model[$guid]))
        $_model[$guid] = new $class($name,$tablePrefix,$connection);
    return $_model[$guid];
}

/**
 * A function Use for instantiation Action Format:[Project://][Grouping/]Module
 * @param string $name Action Resources Address
 * @param string $layer Control layer name
 * @param boolean $common Whether public directory
 * @return Action|false
 */
function A($name,$layer='',$common=false) {
    static $_action = array();
    $layer      =   $layer?$layer:C('DEFAULT_C_LAYER');
    if(strpos($name,'://')) {// Specify the project
        $name   =  str_replace('://','/'.$layer.'/',$name);
    }else{
        $name   =  '@/'.$layer.'/'.$name;
    }
    if(isset($_action[$name]))  return $_action[$name];
    if($common){ // Independent groups case Loading public directories library
        import(str_replace('@/','',$name).$layer,LIB_PATH);
    }else{
        import($name.$layer);
    }
    $class      =   basename($name.$layer);
    if(class_exists($class,false)) {
        $action             = new $class();
        $_action[$name]     =  $action;
        return $action;
    }else {
        return false;
    }
}

/**
 * Remote method invocation module operation URL Parameter format [Project://][Grouping/]Module/Operating
 * @param string $url Call address
 * @param string|array $vars Call parameters Support strings and arrays
 * @param string $layer To invoke the name of the control layer
 * @return mixed
 */
function R($url,$vars=array(),$layer='') {
    $info   =   pathinfo($url);
    $action =   $info['basename'];
    $module =   $info['dirname'];
    $class  =   A($module,$layer);
    if($class){
        if(is_string($vars)) {
            parse_str($vars,$vars);
        }
        return call_user_func_array(array(&$class,$action.C('ACTION_SUFFIX')),$vars);
    }else{
        return false;
    }
}

/**
 * Get and Set language definition(Insensitive)
 * @param string|array $name Linguistic variables
 * @param string $value Language values
 * @return mixed
 */
function L($name=null, $value=null) {
    static $_lang = array();
    // Empty parameter returns all definitions
    if (empty($name))
        return $_lang;
    // Determine the language for(Or set up)
    // If there, direct return all uppercase $name
    if (is_string($name)) {
        $name = strtoupper($name);
        if (is_null($value))
            return isset($_lang[$name]) ? $_lang[$name] : $name;
        $_lang[$name] = $value; // Language definition
        return;
    }
    // Batch Definition
    if (is_array($name))
        $_lang = array_merge($_lang, array_change_key_case($name, CASE_UPPER));
    return;
}

/**
 * Get and set configuration parameters Support batch Definition
 * @param string|array $name Configuration Variables
 * @param mixed $value Configuration values
 * @return mixed
 */
function C($name=null, $value=null) {
    static $_config = array();
    // No parameters for all
    if (empty($name)) {
        if(!empty($value) && $array = S('c_'.$value)) {
            $_config = array_merge($_config, array_change_key_case($array));
        }
        return $_config;
    }
    // Get or set the precedence assignment
    if (is_string($name)) {
        if (!strpos($name, '.')) {
            $name = strtolower($name);
            if (is_null($value))
                return isset($_config[$name]) ? $_config[$name] : null;
            $_config[$name] = $value;
            return;
        }
        // Dimensional array set and get Support
        $name = explode('.', $name);
        $name[0]   =  strtolower($name[0]);
        if (is_null($value))
            return isset($_config[$name[0]][$name[1]]) ? $_config[$name[0]][$name[1]] : null;
        $_config[$name[0]][$name[1]] = $value;
        return;
    }
    // Batch settings
    if (is_array($name)){
        $_config = array_merge($_config, array_change_key_case($name));
        if(!empty($value)) {// Save the configuration values
            S('c_'.$value,$_config);
        }
        return;
    }
    return null; // Avoid illegal argument
}

/**
 * Tag extension treatment
 * @param string $tag Tag name
 * @param mixed $params Incoming parameters
 * @return mixed
 */
function tag($tag, &$params=NULL) {
    // System tab expansion
    $extends    = C('extends.' . $tag);
    // Apply Tag Expansion
    $tags       = C('tags.' . $tag);
    if (!empty($tags)) {
        if(empty($tags['_overlay']) && !empty($extends)) { // Merge extended
            $tags = array_unique(array_merge($extends,$tags));
        }elseif(isset($tags['_overlay'])){ // By setting '_overlay'=>1 coverSystem tag
            unset($tags['_overlay']);
        }
    }elseif(!empty($extends)) {
        $tags = $extends;
    }
    if($tags) {
        if(APP_DEBUG) {
            G($tag.'Start');
            trace('[ '.$tag.' ] --START--','','INFO');
        }
        // Execute extended
        foreach ($tags as $key=>$name) {
            if(!is_int($key)) { // Specify the full path of the class acts Use forMode extended
                $name   = $key;
            }
            B($name, $params);
        }
        if(APP_DEBUG) { // Record the behavior of execution log
            trace('[ '.$tag.' ] --END-- [ RunTime:'.G($tag.'Start',$tag.'End',6).'s ]','','INFO');
        }
    }else{ // Did not perform any act Return false
        return false;
    }
}

/**
 * Behavior extended to dynamically add a tag
 * @param string $tag Tag name
 * @param string $behavior Behavior name
 * @param string $path Behavior Path
 * @return void
 */
function add_tag_behavior($tag,$behavior,$path='') {
    $array      =  C('tags.'.$tag);
    if(!$array) {
        $array  =  array();
    }
    if($path) {
        $array[$behavior] = $path;
    }else{
        $array[] =  $behavior;
    }
    C('tags.'.$tag,$array);
}

/**
 * Perform an action
 * @param string $name Behavior name
 * @param Mixed $params Transmission parameters
 * @return void
 */
function B($name, &$params=NULL) {
    $class      = $name.'Behavior';
    if(APP_DEBUG) {
        G('behaviorStart');
    }
    $behavior   = new $class();
    $behavior->run($params);
    if(APP_DEBUG) { // Record the behavior of execution log
        G('behaviorEnd');
        trace('Run '.$name.' Behavior [ RunTime:'.G('behaviorStart','behaviorEnd',6).'s ]','','INFO');
    }
}

/**
 * Removing whitespace and comments in the code
 * @param string $content Code Contents
 * @return string
 */
function strip_whitespace($content) {
    $stripStr   = '';
    //Php source code analysis
    $tokens     = token_get_all($content);
    $last_space = false;
    for ($i = 0, $j = count($tokens); $i < $j; $i++) {
        if (is_string($tokens[$i])) {
            $last_space = false;
            $stripStr  .= $tokens[$i];
        } else {
            switch ($tokens[$i][0]) {
                //Filter various PHP comments
                case T_COMMENT:
                case T_DOC_COMMENT:
                    break;
                //Filter spaces
                case T_WHITESPACE:
                    if (!$last_space) {
                        $stripStr  .= ' ';
                        $last_space = true;
                    }
                    break;
                case T_START_HEREDOC:
                    $stripStr .= "<<<SEN\n";
                    break;
                case T_END_HEREDOC:
                    $stripStr .= "SEN;\n";
                    for($k = $i+1; $k < $j; $k++) {
                        if(is_string($tokens[$k]) && $tokens[$k] == ';') {
                            $i = $k;
                            break;
                        } else if($tokens[$k][0] == T_CLOSE_TAG) {
                            break;
                        }
                    }
                    break;
                default:
                    $last_space = false;
                    $stripStr  .= $tokens[$i][1];
            }
        }
    }
    return $stripStr;
}

//[RUNTIME]
// Compiled file
function compile($filename) {
    $content        = file_get_contents($filename);
    // Replace pre-compiler directives
    $content        = preg_replace('/\/\/\[RUNTIME\](.*?)\/\/\[\/RUNTIME\]/s', '', $content);
    $content        = substr(trim($content), 5);
    if ('?>' == substr($content, -2))
        $content    = substr($content, 0, -2);
    return $content;
}

// According to the array generates constant definitions
function array_define($array,$check=true) {
    $content = "\n";
    foreach ($array as $key => $val) {
        $key = strtoupper($key);
        if($check)   $content .= 'defined(\'' . $key . '\') or ';
        if (is_int($val) || is_float($val)) {
            $content .= "define('" . $key . "'," . $val . ');';
        } elseif (is_bool($val)) {
            $val = ($val) ? 'true' : 'false';
            $content .= "define('" . $key . "'," . $val . ');';
        } elseif (is_string($val)) {
            $content .= "define('" . $key . "','" . addslashes($val) . "');";
        }
        $content    .= "\n";
    }
    return $content;
}
//[/RUNTIME]

/**
 * Trace records to add and get the page
 * @param string $value Variable
 * @param string $label Tag
 * @param string $level Log Level
 * @param boolean $record Whether logging
 * @return void
 */
function trace($value='[sen]',$label='',$level='DEBUG',$record=false) {
    static $_trace =  array();
    if('[sen]' === $value){ // Gets the trace information
        return $_trace;
    }else{
        $info   =   ($label?$label.':':'').print_r($value,true);
        if('ERR' == $level && C('TRACE_EXCEPTION')) {// throw an exception
            throw_exception($info);
        }
        $level  =   strtoupper($level);
        if(!isset($_trace[$level])) {
                $_trace[$level] =   array();
            }
        $_trace[$level][]   = $info;
        if((defined('IS_AJAX') && IS_AJAX) || !C('SHOW_PAGE_TRACE')  || $record) {
            Log::record($info,$level,$record);
        }
    }
}