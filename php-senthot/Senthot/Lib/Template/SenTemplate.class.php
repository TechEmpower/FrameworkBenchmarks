<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

/**
 * Senthot built-in template engine class
 * SupportXML tag and generic tag template parsing
 * Compiled template engine Support dynamic cache
 * @category	Sen
 * @package		Sen
 * @subpackage  Template
 * @author		ms134n <ms134n@gmail.com>
 */
class  SenTemplate {

    // Template page tag library introduced in the list
    protected   $tagLib          =   array();
    // Current template file
    protected   $templateFile    =   '';
    // Template Variables
    public      $tVar            =   array();
    public      $config          =   array();
    private     $literal         =   array();
    private     $block           =   array();

    /**
     * Architecture function
     * @access public
     */
    public function __construct(){
        $this->config['cache_path']         =   C('CACHE_PATH');
        $this->config['template_suffix']    =   C('TMPL_TEMPLATE_SUFFIX');
        $this->config['cache_suffix']       =   C('TMPL_CACHFILE_SUFFIX');
        $this->config['tmpl_cache']         =   C('TMPL_CACHE_ON');
        $this->config['cache_time']         =   C('TMPL_CACHE_TIME');
        $this->config['taglib_begin']       =   $this->stripPreg(C('TAGLIB_BEGIN'));
        $this->config['taglib_end']         =   $this->stripPreg(C('TAGLIB_END'));
        $this->config['tmpl_begin']         =   $this->stripPreg(C('TMPL_L_DELIM'));
        $this->config['tmpl_end']           =   $this->stripPreg(C('TMPL_R_DELIM'));
        $this->config['default_tmpl']       =   C('TEMPLATE_NAME');
        $this->config['layout_item']        =   C('TMPL_LAYOUT_ITEM');
    }

    private function stripPreg($str) {
        return str_replace(
            array('{','}','(',')','|','[',']','-','+','*','.','^','?'),
            array('\{','\}','\(','\)','\|','\[','\]','\-','\+','\*','\.','\^','\?'),
            $str);        
    }

    // Get and set template variables
    public function get($name) {
        if(isset($this->tVar[$name]))
            return $this->tVar[$name];
        else
            return false;
    }

    public function set($name,$value) {
        $this->tVar[$name]= $value;
    }

    /**
     * Load Template
     * @access public
     * @param string $tmplTemplateFile Template File
     * @param array  $templateVar Template Variables
     * @param string $prefix Template identifying prefix
     * @return void
     */
    public function fetch($templateFile,$templateVar,$prefix='') {
        $this->tVar         =   $templateVar;
        $templateCacheFile  =   $this->loadTemplate($templateFile,$prefix);
        // Template array variable decomposed into independent variable
        extract($templateVar, EXTR_OVERWRITE);
        //Load template cache files
        include $templateCacheFile;
    }

    /**
     * Loaded and cached master template
     * @access public
     * @param string $tmplTemplateFile Template File
     * @param string $prefix Template identifying prefix
     * @return string
     * @throws SenExecption
     */
    public function loadTemplate ($tmplTemplateFile,$prefix='') {
        if(is_file($tmplTemplateFile)) {
            $this->templateFile    =  $tmplTemplateFile;
            // Read the contents of the template file
            $tmplContent =  file_get_contents($tmplTemplateFile);
        }else{
            $tmplContent =  $tmplTemplateFile;
        }
         // According to the template file name to locate the cache file
        $tmplCacheFile = $this->config['cache_path'].$prefix.md5($tmplTemplateFile).$this->config['cache_suffix'];

        // Determine whether to enable layout
        if(C('LAYOUT_ON')) {
            if(false !== strpos($tmplContent,'{__NOLAYOUT__}')) { // Do not use the layout can be individually defined
                $tmplContent = str_replace('{__NOLAYOUT__}','',$tmplContent);
            }else{ // Replace the layout of the main content
                $layoutFile  =  THEME_PATH.C('LAYOUT_NAME').$this->config['template_suffix'];
                $tmplContent = str_replace($this->config['layout_item'],$tmplContent,file_get_contents($layoutFile));
            }
        }
        // Compiled template content
        $tmplContent =  $this->compiler($tmplContent);
        // Detection template directory
        $dir         =  dirname($tmplCacheFile);
        if(!is_dir($dir))
            mkdir($dir,0755,true);
        //Rewrite Cache Files
        if( false === file_put_contents($tmplCacheFile,trim($tmplContent)))
            throw_exception(L('_CACHE_WRITE_ERROR_').':'.$tmplCacheFile);
        return $tmplCacheFile;
    }

    /**
     * Compile the template file contents
     * @access protected
     * @param mixed $tmplContent Template content
     * @return string
     */
    protected function compiler($tmplContent) {
        //Template parsing
        $tmplContent =  $this->parse($tmplContent);
        // Restore is replaced Literal tags
        $tmplContent =  preg_replace('/<!--###literal(\d+)###-->/eis',"\$this->restoreLiteral('\\1')",$tmplContent);
        // Add Security Code
        $tmplContent =  '<?php if (!defined(\'SEN_PATH\')) exit();?>'.$tmplContent;
        if(C('TMPL_STRIP_SPACE')) {
            /* Strip html whitespace and newline */
            $find           = array('~>\s+<~','~>(\s+\n|\r)~');
            $replace        = array('><','>');
            $tmplContent    = preg_replace($find, $replace, $tmplContent);
        }
        // Optimize the generated php code
        $tmplContent = str_replace('?><?php','',$tmplContent);
        return strip_whitespace($tmplContent);
    }

    /**
     * Template parsing entrance
     * Support Common tags and TagLib resolution Support custom tag library
     * @access public
     * @param string $content To parse the template content
     * @return string
     */
    public function parse($content) {
        // Content is empty does not resolve
        if(empty($content)) return '';
        $begin      =   $this->config['taglib_begin'];
        $end        =   $this->config['taglib_end'];
        // Check the include syntax
        $content    =   $this->parseInclude($content);
        // Check the PHP syntax
        $content    =   $this->parsePhp($content);
        // Replaced first literalTag contents
        $content    =   preg_replace('/'.$begin.'literal'.$end.'(.*?)'.$begin.'\/literal'.$end.'/eis',"\$this->parseLiteral('\\1')",$content);

        // Gets the list of tag libraries that need to introduce
        // Only need to define a tag library, allow the introduction of more
        // General in the top of the file
        // Format:<taglib name="html,mytag..." />
        // When TAGLIB_LOAD is configured to true only when testing
        if(C('TAGLIB_LOAD')) {
            $this->getIncludeTagLib($content);
            if(!empty($this->tagLib)) {
                // Import TagLib parsing
                foreach($this->tagLib as $tagLibName) {
                    $this->parseTagLib($tagLibName,$content);
                }
            }
        }
        // Preload the tag library Do not need to use taglib in each template tag loading But you must use the tag library XML prefix
        if(C('TAGLIB_PRE_LOAD')) {
            $tagLibs =  explode(',',C('TAGLIB_PRE_LOAD'));
            foreach ($tagLibs as $tag){
                $this->parseTagLib($tag,$content);
            }
        }
        // Built-in tag library Import without using taglib tags you can use And you don't need to use the tag library XML prefix
        $tagLibs =  explode(',',C('TAGLIB_BUILD_IN'));
        foreach ($tagLibs as $tag){
            $this->parseTagLib($tag,$content,true);
        }
        //Resolve common template tags {tagName}
        $content = preg_replace('/('.$this->config['tmpl_begin'].')([^\d\s'.$this->config['tmpl_begin'].$this->config['tmpl_end'].'].+?)('.$this->config['tmpl_end'].')/eis',"\$this->parseTag('\\2')",$content);
        return $content;
    }

    // Check the PHP syntax
    protected function parsePhp($content) {
        if(ini_get('short_open_tag')){
            // Open short tag to be<? Tags echo output Otherwise normal output XML ID
            $content = preg_replace('/(<\?(?!php|=|$))/i', '<?php echo \'\\1\'; ?>'."\n", $content );
        }
        // PHP syntax check
        if(C('TMPL_DENY_PHP') && false !== strpos($content,'<?php')) {
            throw_exception(L('_NOT_ALLOW_PHP_'));
        }
        return $content;
    }

    // Parsing template layout of tags
    protected function parseLayout($content) {
        // Reading layout in the template tags
        $find = preg_match('/'.$this->config['taglib_begin'].'layout\s(.+?)\s*?\/'.$this->config['taglib_end'].'/is',$content,$matches);
        if($find) {
            //Replaces the Layout tag
            $content    =   str_replace($matches[0],'',$content);
            //Resolution Layout tags
            $array      =   $this->parseXmlAttrs($matches[1]);
            if(!C('LAYOUT_ON') || C('LAYOUT_NAME') !=$array['name'] ) {
                // Reading layout template
                $layoutFile =   THEME_PATH.$array['name'].$this->config['template_suffix'];
                $replace    =   isset($array['replace'])?$array['replace']:$this->config['layout_item'];
                // Replace the layout of the main content
                $content    =   str_replace($replace,$content,file_get_contents($layoutFile));
            }
        }else{
            $content = str_replace('{__NOLAYOUT__}','',$content);
        }
        return $content;
    }

    // Include parsing template tags
    protected function parseInclude($content) {
        // Resolving inheritance
        $content    =   $this->parseAddons($content);
        // Analysis of layout
        $content    =   $this->parseLayout($content);
        // Read template include tags
        $find       =   preg_match_all('/'.$this->config['taglib_begin'].'include\s(.+?)\s*?\/'.$this->config['taglib_end'].'/is',$content,$matches);
        if($find) {
            for($i=0;$i<$find;$i++) {
                $include    =   $matches[1][$i];
                $array      =   $this->parseXmlAttrs($include);
                $file       =   $array['file'];
                unset($array['file']);
                $content    =   str_replace($matches[0][$i],$this->parseIncludeItem($file,$array),$content);
            }
        }
        return $content;
    }

    // Addons parsing template tags
    protected function parseAddons($content) {
        $begin      =   $this->config['taglib_begin'];
        $end        =   $this->config['taglib_end'];        
        // Read inheritance in a template tags
        $find       =   preg_match('/'.$begin.'addons\s(.+?)\s*?\/'.$end.'/is',$content,$matches);
        if($find) {
            //Replace addons tag
            $content    =   str_replace($matches[0],'',$content);
            // Block tag records page
            preg_replace('/'.$begin.'block\sname=(.+?)\s*?'.$end.'(.*?)'.$begin.'\/block'.$end.'/eis',"\$this->parseBlock('\\1','\\2')",$content);
            // Read inherited template
            $array      =   $this->parseXmlAttrs($matches[1]);
            $content    =   $this->parseTemplateName($array['name']);
            // Replace block tags
            $content    =   preg_replace('/'.$begin.'block\sname=(.+?)\s*?'.$end.'(.*?)'.$begin.'\/block'.$end.'/eis',"\$this->replaceBlock('\\1','\\2')",$content);
        }else{
            $content    =   preg_replace('/'.$begin.'block\sname=(.+?)\s*?'.$end.'(.*?)'.$begin.'\/block'.$end.'/eis',"stripslashes('\\2')",$content);            
        }
        return $content;
    }

    /**
     * Analysis of XML attributes
     * @access private
     * @param string $attrs  XML attribute string
     * @return array
     */
    private function parseXmlAttrs($attrs) {
        $xml        =   '<tpl><tag '.$attrs.' /></tpl>';
        $xml        =   simplexml_load_string($xml);
        if(!$xml)
            throw_exception(L('_XML_TAG_ERROR_'));
        $xml        =   (array)($xml->tag->attributes());
        $array      =   array_change_key_case($xml['@attributes']);
        return $array;
    }

    /**
     * Replace literal tag page
     * @access private
     * @param string $content  Template content
     * @return string|false
     */
    private function parseLiteral($content) {
        if(trim($content)=='')  return '';
        $content            =   stripslashes($content);
        $i                  =   count($this->literal);
        $parseStr           =   "<!--###literal{$i}###-->";
        $this->literal[$i]  =   $content;
        return $parseStr;
    }

    /**
     * Restore has been replacing literal tags
     * @access private
     * @param string $tag  Literal tag serial number
     * @return string|false
     */
    private function restoreLiteral($tag) {
        // Restore literal tags
        $parseStr   =  $this->literal[$tag];
        // Destruction of literal records
        unset($this->literal[$tag]);
        return $parseStr;
    }

    /**
     * Block tag records in the current page
     * @access private
     * @param string $name Block name
     * @param string $content  Template content
     * @return string
     */
    private function parseBlock($name,$content) {
        $this->block[$name]  =   $content;
        return '';
    }

    /**
     * Replacing the inherited template block tag
     * @access private
     * @param string $name  Block name
     * @param string $content  Template content
     * @return string
     */
    private function replaceBlock($name,$content) {
        // Replace block tags Do not redefine the original
        $replace   =  isset($this->block[$name])?   $this->block[$name]   :   $content;
        return stripslashes($replace);
    }

    /**
     * Search template page containing the TagLib library
     * And return to the list
     * @access public
     * @param string $content  Template content
     * @return string|false
     */
    public function getIncludeTagLib(& $content) {
        //Search for TagLib tag
        $find = preg_match('/'.$this->config['taglib_begin'].'taglib\s(.+?)(\s*?)\/'.$this->config['taglib_end'].'\W/is',$content,$matches);
        if($find) {
            //Replace TagLib tag
            $content        = str_replace($matches[0],'',$content);
            //Resolving TagLib tag
            $array          =   $this->parseXmlAttrs($matches[1]);
            $this->tagLib   = explode(',',$array['name']);
        }
        return;
    }

    /**
     * TagLib library parsing
     * @access public
     * @param string $tagLib To parse the tag library
     * @param string $content To parse the template content
     * @param boolen $hide Whether to hide tag library prefix
     * @return string
     */
    public function parseTagLib($tagLib,&$content,$hide=false) {
        $begin      =   $this->config['taglib_begin'];
        $end        =   $this->config['taglib_end'];
        $className  =   'TagLib'.ucwords($tagLib);
        $tLib       =   Sen::instance($className);
        foreach ($tLib->getTags() as $name=>$val){
            $tags = array($name);
            if(isset($val['alias'])) {// Alias settings
                $tags       = explode(',',$val['alias']);
                $tags[]     =  $name;
            }
            $level      =   isset($val['level'])?$val['level']:1;
            $closeTag   =   isset($val['close'])?$val['close']:true;
            foreach ($tags as $tag){
                $parseTag = !$hide? $tagLib.':'.$tag: $tag;// Actual tag name to be resolved
                if(!method_exists($tLib,'_'.$tag)) {
                    // Analytical method may need to define an alias
                    $tag  =  $name;
                }
                $n1 = empty($val['attr'])?'(\s*?)':'\s([^'.$end.']*)';
                if (!$closeTag){
                    $patterns       = '/'.$begin.$parseTag.$n1.'\/(\s*?)'.$end.'/eis';
                    $replacement    = "\$this->parseXmlTag('$tagLib','$tag','$1','')";
                    $content        = preg_replace($patterns, $replacement,$content);
                }else{
                    $patterns       = '/'.$begin.$parseTag.$n1.$end.'(.*?)'.$begin.'\/'.$parseTag.'(\s*?)'.$end.'/eis';
                    $replacement    = "\$this->parseXmlTag('$tagLib','$tag','$1','$2')";
                    for($i=0;$i<$level;$i++) 
                        $content=preg_replace($patterns,$replacement,$content);
                }
            }
        }
    }

    /**
     * Resolution tab library
     * Need to call the corresponding tag library file parsing class
     * @access public
     * @param string $tagLib  Tag library name
     * @param string $tag  Tag names
     * @param string $attr  Tag attributes
     * @param string $content  Tag contents
     * @return string|false
     */
    public function parseXmlTag($tagLib,$tag,$attr,$content) {
        //if (MAGIC_QUOTES_GPC) {
            $attr   = stripslashes($attr);
            $content= stripslashes($content);
        //}
        if(ini_get('magic_quotes_sybase'))
            $attr   =  str_replace('\"','\'',$attr);
        $tLib       =  Sen::instance('TagLib'.ucwords(strtolower($tagLib)));
        $parse      = '_'.$tag;
        $content    = trim($content);
        return $tLib->$parse($attr,$content);
    }

    /**
     * Template Tag resolution
     * Format: {TagName:args [|content] }
     * @access public
     * @param string $tagStr Tag contents
     * @return string
     */
    public function parseTag($tagStr){
        //if (MAGIC_QUOTES_GPC) {
            $tagStr = stripslashes($tagStr);
        //}
        //Reducing non-template tag
        if(preg_match('/^[\s|\d]/is',$tagStr))
            //Filtering tags beginning with numbers and spaces
            return C('TMPL_L_DELIM') . $tagStr .C('TMPL_R_DELIM');
        $flag   =  substr($tagStr,0,1);
        $flag2  =  substr($tagStr,1,1);
        $name   = substr($tagStr,1);
        if('$' == $flag && '.' != $flag2 && '(' != $flag2){ //Parsing template variables Format {$varName}
            return $this->parseVar($name);
        }elseif('-' == $flag || '+'== $flag){ // Output calculation
            return  '<?php echo '.$flag.$name.';?>';
        }elseif(':' == $flag){ // Output the result of a function
            return  '<?php echo '.$name.';?>';
        }elseif('~' == $flag){ // Perform a function
            return  '<?php '.$name.';?>';
        }elseif(substr($tagStr,0,2)=='//' || (substr($tagStr,0,2)=='/*' && substr($tagStr,-2)=='*/')){
            //Comment tags
            return '';
        }
        // Identification tags are not returned directly
        return C('TMPL_L_DELIM') . $tagStr .C('TMPL_R_DELIM');
    }

    /**
     * Template variable resolution, Support use the function
     * Format: {$varname|function1|function2=arg1,arg2}
     * @access public
     * @param string $varStr Variable data
     * @return string
     */
    public function parseVar($varStr){
        $varStr     =   trim($varStr);
        static $_varParseList = array();
        //If the variable is a string that has been parsed, the direct return variable value
        if(isset($_varParseList[$varStr])) return $_varParseList[$varStr];
        $parseStr   =   '';
        $varExists  =   true;
        if(!empty($varStr)){
            $varArray = explode('|',$varStr);
            //Get the variable name
            $var = array_shift($varArray);
            if('Sen.' == substr($var,0,4)){
                // All to Sen. Variables beginning with a special treat Templates can be exported without assignment
                $name = $this->parseSenVar($var);
            }elseif( false !== strpos($var,'.')) {
                //Support {$var.property}
                $vars = explode('.',$var);
                $var  =  array_shift($vars);
                switch(strtolower(C('TMPL_VAR_IDENTIFY'))) {
                    case 'array': // Identified as an array
                        $name = '$'.$var;
                        foreach ($vars as $key=>$val)
                            $name .= '["'.$val.'"]';
                        break;
                    case 'obj':  // Identifying the object
                        $name = '$'.$var;
                        foreach ($vars as $key=>$val)
                            $name .= '->'.$val;
                        break;
                    default:  // Automatically determine the array or object Support only two-dimensional
                        $name = 'is_array($'.$var.')?$'.$var.'["'.$vars[0].'"]:$'.$var.'->'.$vars[0];
                }
            }elseif(false !== strpos($var,'[')) {
                //Support {$var['key']} Mode output array
                $name = "$".$var;
                preg_match('/(.+?)\[(.+?)\]/is',$var,$match);
                $var = $match[1];
            }elseif(false !==strpos($var,':') && false ===strpos($var,'::') && false ===strpos($var,'?')){
                //Support {$var:property} Output the object's properties
                $vars = explode(':',$var);
                $var  =  str_replace(':','->',$var);
                $name = "$".$var;
                $var  = $vars[0];
            }else {
                $name = "$$var";
            }
            //The variable using the function
            if(count($varArray)>0)
                $name = $this->parseVarFunction($name,$varArray);
            $parseStr = '<?php echo ('.$name.'); ?>';
        }
        $_varParseList[$varStr] = $parseStr;
        return $parseStr;
    }

    /**
     * Use the template variable function
     * Format {$varname|function1|function2=arg1,arg2}
     * @access public
     * @param string $name Variable name
     * @param array $varArray  Function List
     * @return string
     */
    public function parseVarFunction($name,$varArray){
        //The variable using the function
        $length = count($varArray);
        //Prohibit the use of a template to obtain a list of functions
        $template_deny_funs = explode(',',C('TMPL_DENY_FUNC_LIST'));
        for($i=0;$i<$length ;$i++ ){
            $args = explode('=',$varArray[$i],2);
            //Template function filter
            $fun = strtolower(trim($args[0]));
            switch($fun) {
            case 'default':  // Special template function
                $name   = '('.$name.')?('.$name.'):'.$args[1];
                break;
            default:  // Universal template function
                if(!in_array($fun,$template_deny_funs)){
                    if(isset($args[1])){
                        if(strstr($args[1],'###')){
                            $args[1] = str_replace('###',$name,$args[1]);
                            $name = "$fun($args[1])";
                        }else{
                            $name = "$fun($name,$args[1])";
                        }
                    }else if(!empty($args[0])){
                        $name = "$fun($name)";
                    }
                }
            }
        }
        return $name;
    }

    /**
     * Special template variable resolution
     * Format $Sen. Heading variable is a special template variables
     * @access public
     * @param string $varStr  Variable string
     * @return string
     */
    public function parseSenVar($varStr){
        $vars = explode('.',$varStr);
        $vars[1] = strtoupper(trim($vars[1]));
        $parseStr = '';
        if(count($vars)>=3){
            $vars[2] = trim($vars[2]);
            switch($vars[1]){
                case 'SERVER':
                    $parseStr = '$_SERVER[\''.strtoupper($vars[2]).'\']';break;
                case 'GET':
                    $parseStr = '$_GET[\''.$vars[2].'\']';break;
                case 'POST':
                    $parseStr = '$_POST[\''.$vars[2].'\']';break;
                case 'COOKIE':
                    if(isset($vars[3])) {
                        $parseStr = '$_COOKIE[\''.$vars[2].'\'][\''.$vars[3].'\']';
                    }else{
                        $parseStr = 'cookie(\''.$vars[2].'\')';
                    }
                    break;
                case 'SESSION':
                    if(isset($vars[3])) {
                        $parseStr = '$_SESSION[\''.$vars[2].'\'][\''.$vars[3].'\']';
                    }else{
                        $parseStr = 'session(\''.$vars[2].'\')';
                    }
                    break;
                case 'ENV':
                    $parseStr = '$_ENV[\''.strtoupper($vars[2]).'\']';break;
                case 'REQUEST':
                    $parseStr = '$_REQUEST[\''.$vars[2].'\']';break;
                case 'CONST':
                    $parseStr = strtoupper($vars[2]);break;
                case 'LANG':
                    $parseStr = 'L("'.$vars[2].'")';break;
                case 'CONFIG':
                    if(isset($vars[3])) {
                        $vars[2] .= '.'.$vars[3];
                    }
                    $parseStr = 'C("'.$vars[2].'")';break;
                default:break;
            }
        }else if(count($vars)==2){
            switch($vars[1]){
                case 'NOW':
                    $parseStr = "date('Y-m-d g:i a',time())";
                    break;
                case 'VERSION':
                    $parseStr = 'SEN_VERSION';
                    break;
                case 'TEMPLATE':
                    $parseStr = "'".$this->templateFile."'";//'C("TEMPLATE_NAME")';
                    break;
                case 'LDELIM':
                    $parseStr = 'C("TMPL_L_DELIM")';
                    break;
                case 'RDELIM':
                    $parseStr = 'C("TMPL_R_DELIM")';
                    break;
                default:
                    if(defined($vars[1]))
                        $parseStr = $vars[1];
            }
        }
        return $parseStr;
    }

    /**
     * Common template loaded and cached and the current template in the same path, or use a relative path
     * @access private
     * @param string $tmplPublicName  Public template file name
     * @param array $vars  The list of variables to be passed
     * @return string
     */
    private function parseIncludeItem($tmplPublicName,$vars=array()){
        // Analyze and read the contents of the template file name
        $parseStr = $this->parseTemplateName($tmplPublicName);
        // Substitution variables
        foreach ($vars as $key=>$val) {
            $parseStr = str_replace('['.$key.']',$val,$parseStr);
        }
        // Template that contains the file again analyzed
        return $this->parseInclude($parseStr);
    }

    /**
     * Analysis of the loaded template file and read the contents Support multiple template file read
     * @access private
     * @param string $tmplPublicName  Template file name
     * @return string
     */    
    private function parseTemplateName($templateName){
        if(substr($templateName,0,1)=='$')
            //Support load variable file name
            $templateName = $this->get(substr($templateName,1));
        $array  =   explode(',',$templateName);
        $parseStr   =   '';
        foreach ($array as $templateName){
            if(false === strpos($templateName,$this->config['template_suffix'])) {
                // Parsing rules Template Theme:Module:Operating Not Support Cross-project and cross-grouping called
                $path   =  explode(':',$templateName);
                $action = array_pop($path);
                $module = !empty($path)?array_pop($path):MODULE_NAME;
                if(!empty($path) && THEME_NAME) {// Set Template Theme
                    $path = dirname(THEME_PATH).'/'.array_pop($path).'/';
                }else{
                    $path = THEME_PATH;
                }
                $templateName  =  $path.$module.C('TMPL_FILE_DEPR').$action.$this->config['template_suffix'];
            }
            // Get the template file contents
            $parseStr .= file_get_contents($templateName);
        }
        return $parseStr;
    }    
}