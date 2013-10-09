<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

/**
 * Senthot View class
 * @category	Sen
 * @package		Sen
 * @subpackage  Core
 * @author		ms134n <ms134n@gmail.com>
 */
class View {
    /**
     * Template output variables
     * @var tVar
     * @access protected
     */       
    protected $tVar        =  array();

    /**
     * Template variable assignment
     * @access public
     * @param mixed $name
     * @param mixed $value
     */
    public function assign($name,$value=''){
        if(is_array($name)) {
            $this->tVar   =  array_merge($this->tVar,$name);
        }else {
            $this->tVar[$name] = $value;
        }
    }

    /**
     * Template variable value obtained
     * @access public
     * @param string $name
     * @return mixed
     */
    public function get($name=''){
        if('' === $name) {
            return $this->tVar;
        }
        return isset($this->tVar[$name])?$this->tVar[$name]:false;
    }

    /**
     * Loaded templates and page output Can return output
     * @access public
     * @param string $templateFile Template file name
     * @param string $charset Template output character set
     * @param string $contentType Output Type
     * @param string $content Template output
     * @param string $prefix Template cache prefix
     * @return mixed
     */
    public function display($templateFile='',$charset='',$contentType='',$content='',$prefix='') {
        G('viewStartTime');
        // View of the start tag
        tag('view_begin',$templateFile);
        // Parse and get the template content
        $content = $this->fetch($templateFile,$content,$prefix);
        // Output template content
        $this->render($content,$charset,$contentType);
        // View closing tag
        tag('view_end');
    }

    /**
     * Html output text can include
     * @access private
     * @param string $content Output
     * @param string $charset Template output character set
     * @param string $contentType Output Type
     * @return mixed
     */
    private function render($content,$charset='',$contentType=''){
        if(empty($charset))  $charset = C('DEFAULT_CHARSET');
        if(empty($contentType)) $contentType = C('TMPL_CONTENT_TYPE');
        // Web Character Encoding
        header('Content-Type:'.$contentType.'; charset='.$charset);
        header('Cache-control: '.C('HTTP_CACHE_CONTROL'));  // Page cache control
        header('X-Powered-By:Senthot');
        // Output template file
        echo $content;
    }

    /**
     * Parse and access template content For outputs
     * @access public
     * @param string $templateFile Template file name
     * @param string $content Template output
     * @param string $prefix Template cache prefix
     * @return string
     */
    public function fetch($templateFile='',$content='',$prefix='') {
        if(empty($content)) {
            // Template file parsing tags
            tag('view_template',$templateFile);
            // Template file does not exist return directly
            if(!is_file($templateFile)) return NULL;
        }
        // Page cache
        ob_start();
        ob_implicit_flush(0);
        if('php' == strtolower(C('TMPL_ENGINE_TYPE'))) { // Using native PHP template
            // Template array variable decomposed into independent variable
            extract($this->tVar, EXTR_OVERWRITE);
            // PHP template loaded directly
            empty($content)?include $templateFile:eval('?>'.$content);
        }else{
            // View Resolution tab
            $params = array('var'=>$this->tVar,'file'=>$templateFile,'content'=>$content,'prefix'=>$prefix);
            tag('view_parse',$params);
        }
        // Obtain and empty the cache
        $content = ob_get_clean();
        // Content Filtering tab
        tag('view_filter',$content);
        // Output template file
        return $content;
    }
}