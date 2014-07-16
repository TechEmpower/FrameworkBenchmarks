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
 * Behavior of the system expansion: template parsing
 * @category	Sen
 * @package		Sen
 * @subpackage  Behavior
 * @author		ms134n <ms134n@gmail.com>
 */
class ParseTemplateBehavior extends Behavior {
    // Behavioral parameters defined ( the default value ) Covered in the project configuration
    protected $options   =  array(
        // Layout Settings
        'TMPL_ENGINE_TYPE'		=>  'Sen',     // The default template engine The following settings are only valid with Sen template engine
        'TMPL_CACHFILE_SUFFIX'  =>  '.php',      // The default template cache suffix
        'TMPL_DENY_FUNC_LIST'	=>  'echo,exit',	// Disable function template engine
        'TMPL_DENY_PHP'         =>  false, // Whether to disable the default template engine native PHP code
        'TMPL_L_DELIM'          =>  '{',			// Generic tag template engine start tag
        'TMPL_R_DELIM'          =>  '}',			// Generic tag template engine end tag
        'TMPL_VAR_IDENTIFY'     =>  'array',     // Template variables identified . Blank automatically determine, parameters 'obj' indicates that the object
        'TMPL_STRIP_SPACE'      =>  true,       // Removal of HTML template files spaces and line
        'TMPL_CACHE_ON'			=>  true,        // Open the compiled template caching, set to false then every will be recompiled
        'TMPL_CACHE_PREFIX'     =>  '',         // Template cache prefix designation can be changed dynamically
        'TMPL_CACHE_TIME'		=>	0,         // Template cache validity 0 Permanent, (In figures, the unit: Second)
        'TMPL_LAYOUT_ITEM'      =>  '{__CONTENT__}', // Replace the contents of the layout template logo
        'LAYOUT_ON'             =>  false, // Whether to enable layout
        'LAYOUT_NAME'           =>  'layout', // Name of the current layout Default layout

        // Sen template engine tag library related settings
        'TAGLIB_BEGIN'          =>  '<',  // Start tag tag tag library
        'TAGLIB_END'            =>  '>',  // End tag tag tag library
        'TAGLIB_LOAD'           =>  true, // Whether to use the built-in tag library tag library other than the default auto-detection
        'TAGLIB_BUILD_IN'       =>  'cx', // Built-in tag library name(Use of the tag does not have to specify the name of the tag library), separated by commas Note resolution order
        'TAGLIB_PRE_LOAD'       =>  '',   // Require additional tag libraries loaded(Must specify the name of the tag library), multiple comma-separated
        );

    // Behavior extension execution entry must be run
    public function run(&$_data){
        $engine             =   strtolower(C('TMPL_ENGINE_TYPE'));
        $_content           =   empty($_data['content'])?$_data['file']:$_data['content'];
        $_data['prefix']    =   !empty($_data['prefix'])?$_data['prefix']:C('TMPL_CACHE_PREFIX');
        if('sen'==$engine){ // Template engine using Sen
            if((!empty($_data['content']) && $this->checkContentCache($_data['content'],$_data['prefix'])) 
                ||  $this->checkCache($_data['file'],$_data['prefix'])) { // Cache is valid
                // Decomposition and load the template cache variables
                extract($_data['var'], EXTR_OVERWRITE);
                //Load template cache files
                include C('CACHE_PATH').$_data['prefix'].md5($_content).C('TMPL_CACHFILE_SUFFIX');
            }else{
                $tpl = Sen::instance('SenTemplate');
                // Compile and load the template file
                $tpl->fetch($_content,$_data['var'],$_data['prefix']);
            }
        }else{
            // Called third-party template engine to parse and output
            $class   = 'Template'.ucwords($engine);
            if(class_exists($class)) {
                $tpl   =  new $class;
                $tpl->fetch($_content,$_data['var']);
            }else {  // Class does not define
                throw_exception(L('_NOT_SUPPERT_').': ' . $class);
            }
        }
    }

    /**
     * Check the cache file is valid
     * If this does not need to be recompiled
     * @access public
     * @param string $tmplTemplateFile  Template file name
     * @return boolen
     */
    protected function checkCache($tmplTemplateFile,$prefix='') {
        if (!C('TMPL_CACHE_ON')) // Preferentially detect configuration settings
            return false;
        $tmplCacheFile = C('CACHE_PATH').$prefix.md5($tmplTemplateFile).C('TMPL_CACHFILE_SUFFIX');
        if(!is_file($tmplCacheFile)){
            return false;
        }elseif (filemtime($tmplTemplateFile) > filemtime($tmplCacheFile)) {
            // Template files if you need to update the cache updates
            return false;
        }elseif (C('TMPL_CACHE_TIME') != 0 && time() > filemtime($tmplCacheFile)+C('TMPL_CACHE_TIME')) {
            // Caching is within the validity
            return false;
        }
        // Open layout templates
        if(C('LAYOUT_ON')) {
            $layoutFile  =  THEME_PATH.C('LAYOUT_NAME').C('TMPL_TEMPLATE_SUFFIX');
            if(filemtime($layoutFile) > filemtime($tmplCacheFile)) {
                return false;
            }
        }
        // Cache is valid
        return true;
    }

    /**
     * Check the contents of the cache is valid
     * If this does not need to be recompiled
     * @access public
     * @param string $tmplContent  Template content
     * @return boolen
     */
    protected function checkContentCache($tmplContent,$prefix='') {
        if(is_file(C('CACHE_PATH').$prefix.md5($tmplContent).C('TMPL_CACHFILE_SUFFIX'))){
            return true;
        }else{
            return false;
        }
    }    
}