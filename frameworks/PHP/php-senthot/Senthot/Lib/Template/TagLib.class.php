<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

/**
 * Senthot TagLib parsing a tag library base class
 * @category	Sen
 * @package		Sen
 * @subpackage  Template
 * @author		ms134n <ms134n@gmail.com>
 */
class TagLib {

    /**
     * Tag library definition XML file
     * @var string
     * @access protected
     */
    protected $xml      = '';
    protected $tags     = array();// Tag definition
    /**
     * Tag library name
     * @var string
     * @access protected
     */
    protected $tagLib   ='';

    /**
     * Tag Library Tag List
     * @var string
     * @access protected
     */
    protected $tagList  = array();

    /**
     * Analysis of the array tag library
     * @var string
     * @access protected
     */
    protected $parse    = array();

    /**
     * Tag library is valid
     * @var string
     * @access protected
     */
    protected $valid    = false;

    /**
     * Current template object
     * @var object
     * @access protected
     */
    protected $tpl;

    protected $comparison = array(' nheq '=>' !== ',' heq '=>' === ',' neq '=>' != ',' eq '=>' == ',' egt '=>' >= ',' gt '=>' > ',' elt '=>' <= ',' lt '=>' < ');

    /**
     * Architecture function
     * @access public
     */
    public function __construct() {
        $this->tagLib  = strtolower(substr(get_class($this),4));
        $this->tpl     = Sen::instance('SenTemplate');
    }

    /**
     * TagLibTag attributes analysis Back Tag attributes array
     * @access public
     * @param string $tagStr Tag contents
     * @return array
     */
    public function parseXmlAttr($attr,$tag) {
        //XML parsing security filtering
        $attr   =   str_replace('&','___', $attr);
        $xml    =   '<tpl><tag '.$attr.' /></tpl>';
        $xml    =   simplexml_load_string($xml);
        if(!$xml) {
            throw_exception(L('_XML_TAG_ERROR_').' : '.$attr);
        }
        $xml    =   (array)($xml->tag->attributes());
        $array  =   array_change_key_case($xml['@attributes']);
        if($array) {
            $attrs  = explode(',',$this->tags[strtolower($tag)]['attr']);
            if(isset($this->tags[strtolower($tag)]['must'])){
                $must   =   explode(',',$this->tags[strtolower($tag)]['must']);
            }else{
                $must   =   array();
            }
            foreach($attrs as $name) {
                if( isset($array[$name])) {
                    $array[$name] = str_replace('___','&',$array[$name]);
                }elseif(false !== array_search($name,$must)){
                    throw_exception(L('_PARAM_ERROR_').':'.$name);
                }
            }
            return $array;
        }
    }

    /**
     * Analytical conditional expression
     * @access public
     * @param string $condition Expressions Tag contents
     * @return array
     */
    public function parseCondition($condition) {
        $condition = str_ireplace(array_keys($this->comparison),array_values($this->comparison),$condition);
        $condition = preg_replace('/\$(\w+):(\w+)\s/is','$\\1->\\2 ',$condition);
        switch(strtolower(C('TMPL_VAR_IDENTIFY'))) {
            case 'array': // Identified as an array
                $condition  =   preg_replace('/\$(\w+)\.(\w+)\s/is','$\\1["\\2"] ',$condition);
                break;
            case 'obj':  // Identifying the object
                $condition  =   preg_replace('/\$(\w+)\.(\w+)\s/is','$\\1->\\2 ',$condition);
                break;
            default:  // Automatically determine the array or object Support only two-dimensional
                $condition  =   preg_replace('/\$(\w+)\.(\w+)\s/is','(is_array($\\1)?$\\1["\\2"]:$\\1->\\2) ',$condition);
        }
        if(false !== strpos($condition, '$Sen'))
            $condition      =   preg_replace('/(\$Sen.*?)\s/ies',"\$this->parseSenVar('\\1');" , $condition);        
        return $condition;
    }

    /**
     * Automatic recognition of construction variables
     * @access public
     * @param string $name Variable description
     * @return string
     */
    public function autoBuildVar($name) {
        if('Sen.' == substr($name,0,4)){
            // Special variable
            return $this->parseSenVar($name);
        }elseif(strpos($name,'.')) {
            $vars = explode('.',$name);
            $var  =  array_shift($vars);
            switch(strtolower(C('TMPL_VAR_IDENTIFY'))) {
                case 'array': // Identified as an array
                    $name = '$'.$var;
                    foreach ($vars as $key=>$val){
                        if(0===strpos($val,'$')) {
                            $name .= '["{'.$val.'}"]';
                        }else{
                            $name .= '["'.$val.'"]';
                        }
                    }
                    break;
                case 'obj':  // Identifying the object
                    $name = '$'.$var;
                    foreach ($vars as $key=>$val)
                        $name .= '->'.$val;
                    break;
                default:  // Automatically determine the array or object Support only two-dimensional
                    $name = 'is_array($'.$var.')?$'.$var.'["'.$vars[0].'"]:$'.$var.'->'.$vars[0];
            }
        }elseif(strpos($name,':')){
            // Additional object Support
            $name   =   '$'.str_replace(':','->',$name);
        }elseif(!defined($name)) {
            $name = '$'.$name;
        }
        return $name;
    }

    /**
     * For Tag attributes inside the special template variable resolution
     * Format Sen. Heading variable is a special template variables
     * @access public
     * @param string $varStr  Variable string
     * @return string
     */
    public function parseSenVar($varStr){
        $vars       = explode('.',$varStr);
        $vars[1]    = strtoupper(trim($vars[1]));
        $parseStr   = '';
        if(count($vars)>=3){
            $vars[2] = trim($vars[2]);
            switch($vars[1]){
                case 'SERVER':    $parseStr = '$_SERVER[\''.$vars[2].'\']';break;
                case 'GET':         $parseStr = '$_GET[\''.$vars[2].'\']';break;
                case 'POST':       $parseStr = '$_POST[\''.$vars[2].'\']';break;
                case 'COOKIE':
                    if(isset($vars[3])) {
                        $parseStr = '$_COOKIE[\''.$vars[2].'\'][\''.$vars[3].'\']';
                    }elseif(C('COOKIE_PREFIX')){
                        $parseStr = '$_COOKIE[\''.C('COOKIE_PREFIX').$vars[2].'\']';
                    }else{
                        $parseStr = '$_COOKIE[\''.$vars[2].'\']';
                    }
                    break;
                case 'SESSION':
                    if(isset($vars[3])) {
                        $parseStr = '$_SESSION[\''.$vars[2].'\'][\''.$vars[3].'\']';
                    }elseif(C('SESSION_PREFIX')){
                        $parseStr = '$_SESSION[\''.C('SESSION_PREFIX').'\'][\''.$vars[2].'\']';
                    }else{
                        $parseStr = '$_SESSION[\''.$vars[2].'\']';
                    }
                    break;
                case 'ENV':         $parseStr = '$_ENV[\''.$vars[2].'\']';break;
                case 'REQUEST':  $parseStr = '$_REQUEST[\''.$vars[2].'\']';break;
                case 'CONST':     $parseStr = strtoupper($vars[2]);break;
                case 'LANG':       $parseStr = 'L("'.$vars[2].'")';break;
                case 'CONFIG':    $parseStr = 'C("'.$vars[2].'")';break;
            }
        }else if(count($vars)==2){
            switch($vars[1]){
                case 'NOW':       $parseStr = "date('Y-m-d g:i a',time())";break;
                case 'VERSION':  $parseStr = 'SEN_VERSION';break;
                case 'TEMPLATE':$parseStr = 'C("TEMPLATE_NAME")';break;
                case 'LDELIM':    $parseStr = 'C("TMPL_L_DELIM")';break;
                case 'RDELIM':    $parseStr = 'C("TMPL_R_DELIM")';break;
                default:  if(defined($vars[1])) $parseStr = $vars[1];
            }
        }
        return $parseStr;
    }

    // Get Tag definition
    public function getTags(){
        return $this->tags;
    }
}