<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

/**
 * Senthot Widget class Abstract class
 * @category	Sen
 * @package		Sen
 * @subpackage  Core
 * @author		ms134n <ms134n@gmail.com>
 */
abstract class Widget {

    // Use template engine Each Widget can be individually configured without affecting the system
    protected $template =  '';

    /**
     * Render Output render method is unique interface Widget
     * Use string to return Can not have any output
     * @access public
     * @param mixed $data  The data to be rendered
     * @return string
     */
    abstract public function render($data);

    /**
     * Render the template output For the render method is called internally
     * @access public
     * @param string $templateFile  Template File
     * @param mixed $var  Template Variables
     * @return string
     */
    protected function renderFile($templateFile='',$var='') {
        ob_start();
        ob_implicit_flush(0);
        if(!file_exists_case($templateFile)){
            // Automatic positioning template file
            $name   = substr(get_class($this),0,-6);
            $filename   =  empty($templateFile)?$name:$templateFile;
            $templateFile = BASE_LIB_PATH.'Widget/'.$name.'/'.$filename.C('TMPL_TEMPLATE_SUFFIX');
            if(!file_exists_case($templateFile))
                throw_exception(L('_TEMPLATE_NOT_EXIST_').'['.$templateFile.']');
        }
        $template   =  strtolower($this->template?$this->template:(C('TMPL_ENGINE_TYPE')?C('TMPL_ENGINE_TYPE'):'php'));
        if('php' == $template) {
            // Using PHP template
            if(!empty($var)) extract($var, EXTR_OVERWRITE);
            // PHP template loaded directly
            include $templateFile;
        }elseif('sen'==$template){ // Template engine using Sen
            if($this->checkCache($templateFile)) { // Cache is valid
                // Decomposition and load the template cache variables
                extract($var, EXTR_OVERWRITE);
                //Load template cache files
                include C('CACHE_PATH').md5($templateFile).C('TMPL_CACHFILE_SUFFIX');
            }else{
                $tpl = Sen::instance('SenTemplate');
                // Compile and load the template file
                $tpl->fetch($templateFile,$var);
            }
        }else{
            $class   = 'Template'.ucwords($template);
            if(is_file(CORE_PATH.'Driver/Template/'.$class.'.class.php')) {
                // Internal Drive
                $path = CORE_PATH;
            }else{ // Expansion Drive
                $path = ADDONS_PATH;
            }
            require_cache($path.'Driver/Template/'.$class.'.class.php');
            $tpl   =  new $class;
            $tpl->fetch($templateFile,$var);
        }
        $content = ob_get_clean();
        return $content;
    }

    /**
     * Check the cache file is valid
     * If this does not need to be recompiled
     * @access public
     * @param string $tmplTemplateFile  Template file name
     * @return boolen
     */
    protected function checkCache($tmplTemplateFile) {
        if (!C('TMPL_CACHE_ON')) // Preferentially detect configuration settings
            return false;
        $tmplCacheFile = C('CACHE_PATH').md5($tmplTemplateFile).C('TMPL_CACHFILE_SUFFIX');
        if(!is_file($tmplCacheFile)){
            return false;
        }elseif (filemtime($tmplTemplateFile) > filemtime($tmplCacheFile)) {
            // Template files if you need to update the cache updates
            return false;
        }elseif (C('TMPL_CACHE_TIME') != 0 && time() > filemtime($tmplCacheFile)+C('TMPL_CACHE_TIME')) {
            // Caching is within the validity
            return false;
        }
        // Cache is valid
        return true;
    }
}