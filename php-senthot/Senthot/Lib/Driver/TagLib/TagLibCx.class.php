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
 * CX tag library analytic categories
 * @category	Sen
 * @package		Sen
 * @subpackage  Driver.Taglib
 * @author		ms134n <ms134n@gmail.com>
 */
class TagLibCx extends TagLib {

    // Tag definition
    protected $tags   =  array(
        // Tag definition: attr Property List close Is closed ( 0 Or 1  default 1) alias Tags alias level nesting level
        'php'       =>  array(),
        'volist'    =>  array('attr'=>'name,id,offset,length,key,mod','level'=>3,'alias'=>'iterate'),
        'foreach'   =>  array('attr'=>'name,item,key','level'=>3),
        'if'        =>  array('attr'=>'condition','level'=>2),
        'elseif'    =>  array('attr'=>'condition','close'=>0),
        'else'      =>  array('attr'=>'','close'=>0),
        'switch'    =>  array('attr'=>'name','level'=>2),
        'case'      =>  array('attr'=>'value,break'),
        'default'   =>  array('attr'=>'','close'=>0),
        'compare'   =>  array('attr'=>'name,value,type','level'=>3,'alias'=>'eq,equal,notequal,neq,gt,lt,egt,elt,heq,nheq'),
        'range'     =>  array('attr'=>'name,value,type','level'=>3,'alias'=>'in,notin,between,notbetween'),
        'empty'     =>  array('attr'=>'name','level'=>3),
        'notempty'  =>  array('attr'=>'name','level'=>3),
        'present'   =>  array('attr'=>'name','level'=>3),
        'notpresent'=>  array('attr'=>'name','level'=>3),
        'defined'   =>  array('attr'=>'name','level'=>3),
        'notdefined'=>  array('attr'=>'name','level'=>3),
        'import'    =>  array('attr'=>'file,href,type,value,basepath','close'=>0,'alias'=>'load,css,js'),
        'assign'    =>  array('attr'=>'name,value','close'=>0),
        'define'    =>  array('attr'=>'name,value','close'=>0),
        'for'       =>  array('attr'=>'start,end,name,comparison,step', 'level'=>3),
        );

    /**
     * php tags parsing
     * @access public
     * @param string $attr Tag attributes
     * @param string $content  Tag contents
     * @return string
     */
    public function _php($attr,$content) {
        $parseStr = '<?php '.$content.' ?>';
        return $parseStr;
    }

    /**
     * volist tag resolution Cyclic output data sets
     * Format:
     * <volist name="userList" id="user" empty="" >
     * {user.username}
     * {user.email}
     * </volist>
     * @access public
     * @param string $attr Tag attributes
     * @param string $content  Tag contents
     * @return string|void
     */
    public function _volist($attr,$content) {
        static $_iterateParseCache = array();
        //If you have already parsed, simply return the variable value
        $cacheIterateId = md5($attr.$content);
        if(isset($_iterateParseCache[$cacheIterateId]))
            return $_iterateParseCache[$cacheIterateId];
        $tag   =    $this->parseXmlAttr($attr,'volist');
        $name  =    $tag['name'];
        $id    =    $tag['id'];
        $empty =    isset($tag['empty'])?$tag['empty']:'';
        $key   =    !empty($tag['key'])?$tag['key']:'i';
        $mod   =    isset($tag['mod'])?$tag['mod']:'2';
        // Allows the use of function setting data sets <volist name=":fun('arg')" id="vo">{$vo.name}</volist>
        $parseStr   =  '<?php ';
        if(0===strpos($name,':')) {
            $parseStr   .= '$_result='.substr($name,1).';';
            $name   = '$_result';
        }else{
            $name   = $this->autoBuildVar($name);
        }
        $parseStr  .=  'if(is_array('.$name.')): $'.$key.' = 0;';
		if(isset($tag['length']) && '' !=$tag['length'] ) {
			$parseStr  .= ' $__LIST__ = array_slice('.$name.','.$tag['offset'].','.$tag['length'].',true);';
		}elseif(isset($tag['offset'])  && '' !=$tag['offset']){
            $parseStr  .= ' $__LIST__ = array_slice('.$name.','.$tag['offset'].',null,true);';
        }else{
            $parseStr .= ' $__LIST__ = '.$name.';';
        }
        $parseStr .= 'if( count($__LIST__)==0 ) : echo "'.$empty.'" ;';
        $parseStr .= 'else: ';
        $parseStr .= 'foreach($__LIST__ as $key=>$'.$id.'): ';
        $parseStr .= '$mod = ($'.$key.' % '.$mod.' );';
        $parseStr .= '++$'.$key.';?>';
        $parseStr .= $this->tpl->parse($content);
        $parseStr .= '<?php endforeach; endif; else: echo "'.$empty.'" ;endif; ?>';
        $_iterateParseCache[$cacheIterateId] = $parseStr;

        if(!empty($parseStr)) {
            return $parseStr;
        }
        return ;
    }

    /**
     * foreach tag resolution Cyclic output data sets
     * @access public
     * @param string $attr Tag attributes
     * @param string $content  Tag contents
     * @return string|void
     */
    public function _foreach($attr,$content) {
        static $_iterateParseCache = array();
        //If you have already parsed, simply return the variable value
        $cacheIterateId = md5($attr.$content);
        if(isset($_iterateParseCache[$cacheIterateId]))
            return $_iterateParseCache[$cacheIterateId];
        $tag        =   $this->parseXmlAttr($attr,'foreach');
        $name       =   $tag['name'];
        $item       =   $tag['item'];
        $key        =   !empty($tag['key'])?$tag['key']:'key';
        $name       =   $this->autoBuildVar($name);
        $parseStr   =   '<?php if(is_array('.$name.')): foreach('.$name.' as $'.$key.'=>$'.$item.'): ?>';
        $parseStr  .=   $this->tpl->parse($content);
        $parseStr  .=   '<?php endforeach; endif; ?>';
        $_iterateParseCache[$cacheIterateId] = $parseStr;
        if(!empty($parseStr)) {
            return $parseStr;
        }
        return ;
    }

    /**
     * if tag resolution
     * Format:
     * <if condition=" $a eq 1" >
     * <elseif condition="$a eq 2" />
     * <else />
     * </if>
     * Expression support eq neq gt egt lt elt == > >= < <= or and || &&
     * @access public
     * @param string $attr Tag attributes
     * @param string $content  Tag contents
     * @return string
     */
    public function _if($attr,$content) {
        $tag        =   $this->parseXmlAttr($attr,'if');
        $condition  =   $this->parseCondition($tag['condition']);
        $parseStr   =   '<?php if('.$condition.'): ?>'.$content.'<?php endif; ?>';
        return $parseStr;
    }

    /**
     * else tag resolution
     * Format:See if tag
     * @access public
     * @param string $attr Tag attributes
     * @param string $content  Tag contents
     * @return string
     */
    public function _elseif($attr,$content) {
        $tag        =   $this->parseXmlAttr($attr,'elseif');
        $condition  =   $this->parseCondition($tag['condition']);
        $parseStr   =   '<?php elseif('.$condition.'): ?>';
        return $parseStr;
    }

    /**
     * else tag resolution
     * @access public
     * @param string $attr Tag attributes
     * @return string
     */
    public function _else($attr) {
        $parseStr = '<?php else: ?>';
        return $parseStr;
    }

    /**
     * switch tag resolution
     * Format:
     * <switch name="a.name" >
     * <case value="1" break="false">1</case>
     * <case value="2" >2</case>
     * <default />other
     * </switch>
     * @access public
     * @param string $attr Tag attributes
     * @param string $content  Tag contents
     * @return string
     */
    public function _switch($attr,$content) {
        $tag        =   $this->parseXmlAttr($attr,'switch');
        $name       =   $tag['name'];
        $varArray   =   explode('|',$name);
        $name       =   array_shift($varArray);
        $name       =   $this->autoBuildVar($name);
        if(count($varArray)>0)
            $name   =   $this->tpl->parseVarFunction($name,$varArray);
        $parseStr   =   '<?php switch('.$name.'): ?>'.$content.'<?php endswitch;?>';
        return $parseStr;
    }

    /**
     * analytic case tag Need to meet the switch to be effective
     * @access public
     * @param string $attr Tag attributes
     * @param string $content  Tag contents
     * @return string
     */
    public function _case($attr,$content) {
        $tag    = $this->parseXmlAttr($attr,'case');
        $value  = $tag['value'];
        if('$' == substr($value,0,1)) {
            $varArray   =   explode('|',$value);
            $value	    =	array_shift($varArray);
            $value      =   $this->autoBuildVar(substr($value,1));
            if(count($varArray)>0)
                $value  =   $this->tpl->parseVarFunction($value,$varArray);
            $value      =   'case '.$value.': ';
        }elseif(strpos($value,'|')){
            $values     =   explode('|',$value);
            $value      =   '';
            foreach ($values as $val){
                $value   .=  'case "'.addslashes($val).'": ';
            }
        }else{
            $value	=    'case "'.$value.'": ';
        }
        $parseStr = '<?php '.$value.' ?>'.$content;
        $isBreak  = isset($tag['break']) ? $tag['break'] : '';
        if('' ==$isBreak || $isBreak) {
            $parseStr .= '<?php break;?>';
        }
        return $parseStr;
    }

    /**
     * default tag resolution Need to meet the switch to be effective
     * Use: <default />ddfdf
     * @access public
     * @param string $attr Tag attributes
     * @param string $content  Tag contents
     * @return string
     */
    public function _default($attr) {
        $parseStr = '<?php default: ?>';
        return $parseStr;
    }

    /**
     * compare tag resolution
     * For value comparison Support eq neq gt lt egt elt heq nheq default is EQ
     * Format: <compare name="" type="eq" value="" >content</compare>
     * @access public
     * @param string $attr Tag attributes
     * @param string $content  Tag contents
     * @return string
     */
    public function _compare($attr,$content,$type='eq') {
        $tag        =   $this->parseXmlAttr($attr,'compare');
        $name       =   $tag['name'];
        $value      =   $tag['value'];
        $type       =   isset($tag['type'])?$tag['type']:$type;
        $type       =   $this->parseCondition(' '.$type.' ');
        $varArray   =   explode('|',$name);
        $name       =   array_shift($varArray);
        $name       =   $this->autoBuildVar($name);
        if(count($varArray)>0)
            $name = $this->tpl->parseVarFunction($name,$varArray);
        if('$' == substr($value,0,1)) {
            $value  =  $this->autoBuildVar(substr($value,1));
        }else {
            $value  =   '"'.$value.'"';
        }
        $parseStr   =   '<?php if(('.$name.') '.$type.' '.$value.'): ?>'.$content.'<?php endif; ?>';
        return $parseStr;
    }

    public function _eq($attr,$content) {
        return $this->_compare($attr,$content,'eq');
    }

    public function _equal($attr,$content) {
        return $this->_compare($attr,$content,'eq');
    }

    public function _neq($attr,$content) {
        return $this->_compare($attr,$content,'neq');
    }

    public function _notequal($attr,$content) {
        return $this->_compare($attr,$content,'neq');
    }

    public function _gt($attr,$content) {
        return $this->_compare($attr,$content,'gt');
    }

    public function _lt($attr,$content) {
        return $this->_compare($attr,$content,'lt');
    }

    public function _egt($attr,$content) {
        return $this->_compare($attr,$content,'egt');
    }

    public function _elt($attr,$content) {
        return $this->_compare($attr,$content,'elt');
    }

    public function _heq($attr,$content) {
        return $this->_compare($attr,$content,'heq');
    }

    public function _nheq($attr,$content) {
        return $this->_compare($attr,$content,'nheq');
    }

    /**
     * range tag parsing
     * If a variable exists in a range The contents of the output type= in Indicate the range it says out of range
     * Format: <range name="var|function"  value="val" type='in|notin' >content</range>
     * example: <range name="a"  value="1,2,3" type='in' >content</range>
     * @access public
     * @param string $attr Tag attributes
     * @param string $content  Tag contents
     * @param string $type  Type of comparison
     * @return string
     */
    public function _range($attr,$content,$type='in') {
        $tag        =   $this->parseXmlAttr($attr,'range');
        $name       =   $tag['name'];
        $value      =   $tag['value'];
        $varArray   =   explode('|',$name);
        $name       =   array_shift($varArray);
        $name       =   $this->autoBuildVar($name);
        if(count($varArray)>0)
            $name   =   $this->tpl->parseVarFunction($name,$varArray);

        $type       =   isset($tag['type'])?$tag['type']:$type;

        if('$' == substr($value,0,1)) {
            $value  =   $this->autoBuildVar(substr($value,1));
            $str    =   'is_array('.$value.')?'.$value.':explode(\',\','.$value.')';
        }else{
            $value  =   '"'.$value.'"';
            $str    =   'explode(\',\','.$value.')';
        }
        if($type=='between') {
            $parseStr = '<?php $_RANGE_VAR_='.$str.';if('.$name.'>= $_RANGE_VAR_[0] && '.$name.'<= $_RANGE_VAR_[1]):?>'.$content.'<?php endif; ?>';
        }elseif($type=='notbetween'){
            $parseStr = '<?php $_RANGE_VAR_='.$str.';if('.$name.'<$_RANGE_VAR_[0] && '.$name.'>$_RANGE_VAR_[1]):?>'.$content.'<?php endif; ?>';
        }else{
            $fun        =  ($type == 'in')? 'in_array'    :   '!in_array';
            $parseStr   = '<?php if('.$fun.'(('.$name.'), '.$str.')): ?>'.$content.'<?php endif; ?>';
        }
        return $parseStr;
    }

    // range tag alias Used in judgment
    public function _in($attr,$content) {
        return $this->_range($attr,$content,'in');
    }

    // range tag alias Judge for notin
    public function _notin($attr,$content) {
        return $this->_range($attr,$content,'notin');
    }

    public function _between($attr,$content){
        return $this->_range($attr,$content,'between');
    }

    public function _notbetween($attr,$content){
        return $this->_range($attr,$content,'notbetween');
    }

    /**
     * present tag resolution
     * If a variable has been set The contents of the output
     * Format: <present name="" >content</present>
     * @access public
     * @param string $attr Tag attributes
     * @param string $content  Tag contents
     * @return string
     */
    public function _present($attr,$content) {
        $tag        =   $this->parseXmlAttr($attr,'present');
        $name       =   $tag['name'];
        $name       =   $this->autoBuildVar($name);
        $parseStr   =   '<?php if(isset('.$name.')): ?>'.$content.'<?php endif; ?>';
        return $parseStr;
    }

    /**
     * notpresent tag resolution
     * If a variable is not set, The contents of the output
     * Format: <notpresent name="" >content</notpresent>
     * @access public
     * @param string $attr Tag attributes
     * @param string $content  Tag contents
     * @return string
     */
    public function _notpresent($attr,$content) {
        $tag        =   $this->parseXmlAttr($attr,'notpresent');
        $name       =   $tag['name'];
        $name       =   $this->autoBuildVar($name);
        $parseStr   =   '<?php if(!isset('.$name.')): ?>'.$content.'<?php endif; ?>';
        return $parseStr;
    }

    /**
     * empty tag resolution
     * If a variable is empty The contents of the output
     * Format: <empty name="" >content</empty>
     * @access public
     * @param string $attr Tag attributes
     * @param string $content  Tag contents
     * @return string
     */
    public function _empty($attr,$content) {
        $tag        =   $this->parseXmlAttr($attr,'empty');
        $name       =   $tag['name'];
        $name       =   $this->autoBuildVar($name);
        $parseStr   =   '<?php if(empty('.$name.')): ?>'.$content.'<?php endif; ?>';
        return $parseStr;
    }

    public function _notempty($attr,$content) {
        $tag        =   $this->parseXmlAttr($attr,'notempty');
        $name       =   $tag['name'];
        $name       =   $this->autoBuildVar($name);
        $parseStr   =   '<?php if(!empty('.$name.')): ?>'.$content.'<?php endif; ?>';
        return $parseStr;
    }

    /**
     * Determine whether this constant has been defined
     * <defined name='TXT'>Defined</defined>
     * @param <type> $attr
     * @param <type> $content
     * @return string
     */
    public function _defined($attr,$content) {
        $tag        =   $this->parseXmlAttr($attr,'defined');
        $name       =   $tag['name'];
        $parseStr   =   '<?php if(defined("'.$name.'")): ?>'.$content.'<?php endif; ?>';
        return $parseStr;
    }

    public function _notdefined($attr,$content) {
        $tag        =   $this->parseXmlAttr($attr,'_notdefined');
        $name       =   $tag['name'];
        $parseStr   =   '<?php if(!defined("'.$name.'")): ?>'.$content.'<?php endif; ?>';
        return $parseStr;
    }

    /**
     * import Tag resolution <import file="Js.Base" /> 
     * <import file="Css.Base" type="css" />
     * @access public
     * @param string $attr Tag attributes
     * @param string $content  Tag contents
     * @param boolean $isFile  Whether papers
     * @param string $type  Type
     * @return string
     */
    public function _import($attr,$content,$isFile=false,$type='') {
        $tag        =   $this->parseXmlAttr($attr,'import');
        $file       =   isset($tag['file'])?$tag['file']:$tag['href'];
        $parseStr   =   '';
        $endStr     =   '';
        // Determine if there are load conditions Allows the use of function to determine (The default is isset)
        if (isset($tag['value'])) {
            $varArray  =    explode('|',$tag['value']);
            $name      =    array_shift($varArray);
            $name      =    $this->autoBuildVar($name);
            if (!empty($varArray))
                $name  =    $this->tpl->parseVarFunction($name,$varArray);
            else
                $name  =    'isset('.$name.')';
            $parseStr .=    '<?php if('.$name.'): ?>';
            $endStr    =    '<?php endif; ?>';
        }
        if($isFile) {
            // Automatic recognition based on the file name suffix
            $type  = $type?$type:(!empty($tag['type'])?strtolower($tag['type']):null);
            // Import papers
            $array =  explode(',',$file);
            foreach ($array as $val){
                if (!$type || isset($reset)) {
                    $type = $reset = strtolower(substr(strrchr($val, '.'),1));
                }
                switch($type) {
                case 'js':
                    $parseStr .= '<script type="text/javascript" src="'.$val.'"></script>';
                    break;
                case 'css':
                    $parseStr .= '<link rel="stylesheet" type="text/css" href="'.$val.'" />';
                    break;
                case 'php':
                    $parseStr .= '<?php require_cache("'.$val.'"); ?>';
                    break;
                }
            }
        }else{
            // Namespace import mode The default is js
            $type       =   $type?$type:(!empty($tag['type'])?strtolower($tag['type']):'js');
            $basepath   =   !empty($tag['basepath'])?$tag['basepath']:__ROOT__.'/Public';
            // Way to import an external file namespaces
            $array      =   explode(',',$file);
            foreach ($array as $val){
                list($val,$version) =   explode('?',$val);
                switch($type) {
                case 'js':
                    $parseStr .= '<script type="text/javascript" src="'.$basepath.'/'.str_replace(array('.','#'), array('/','.'),$val).'.js'.($version?'?'.$version:'').'"></script>';
                    break;
                case 'css':
                    $parseStr .= '<link rel="stylesheet" type="text/css" href="'.$basepath.'/'.str_replace(array('.','#'), array('/','.'),$val).'.css'.($version?'?'.$version:'').'" />';
                    break;
                case 'php':
                    $parseStr .= '<?php import("'.$val.'"); ?>';
                    break;
                }
            }
        }
        return $parseStr.$endStr;
    }

    // import alias Be loaded using the file (To use the namespace must import) For example, <load file="__PUBLIC__/Js/Base.js" />
    public function _load($attr,$content) {
        return $this->_import($attr,$content,true);
    }

    // import aliases Import CSS file <css file="__PUBLIC__/Css/Base.css" />
    public function _css($attr,$content) {
        return $this->_import($attr,$content,true,'css');
    }

    // import aliases Import a js file <js file="__PUBLIC__/Js/Base.js" />
    public function _js($attr,$content) {
        return $this->_import($attr,$content,true,'js');
    }

    /**
     * assignTag resolution
     * In the template to a variable assignment Support variable assignment
     * Format: <assign name="" value="" />
     * @access public
     * @param string $attr Tag attributes
     * @param string $content  Tag contents
     * @return string
     */
    public function _assign($attr,$content) {
        $tag        =   $this->parseXmlAttr($attr,'assign');
        $name       =   $this->autoBuildVar($tag['name']);
        if('$'==substr($tag['value'],0,1)) {
            $value  =   $this->autoBuildVar(substr($tag['value'],1));
        }else{
            $value  =   '\''.$tag['value']. '\'';
        }
        $parseStr   =   '<?php '.$name.' = '.$value.'; ?>';
        return $parseStr;
    }

    /**
     * defineTag resolution
     * Constants defined in the template Support variable assignment
     * Format: <define name="" value="" />
     * @access public
     * @param string $attr Tag attributes
     * @param string $content  Tag contents
     * @return string
     */
    public function _define($attr,$content) {
        $tag        =   $this->parseXmlAttr($attr,'define');
        $name       =   '\''.$tag['name']. '\'';
        if('$'==substr($tag['value'],0,1)) {
            $value  =   $this->autoBuildVar(substr($tag['value'],1));
        }else{
            $value  =   '\''.$tag['value']. '\'';
        }
        $parseStr   =   '<?php define('.$name.', '.$value.'); ?>';
        return $parseStr;
    }
    
    /**
     * forTag resolution
     * Format: <for start="" end="" comparison="" step="" name="" />
     * @access public
     * @param string $attr Tag attributes
     * @param string $content  Tag contents
     * @return string
     */
    public function _for($attr, $content){
        //Set Default
        $start 		= 0;
        $end   		= 0;
        $step 		= 1;
        $comparison = 'lt';
        $name		= 'i';
        $rand       = rand(); //Adding random number to prevent conflicts nested variable
        //Get Properties
        foreach ($this->parseXmlAttr($attr, 'for') as $key => $value){
            $value = trim($value);
            if(':'==substr($value,0,1))
                $value = substr($value,1);
            elseif('$'==substr($value,0,1))
                $value = $this->autoBuildVar(substr($value,1));
            switch ($key){
                case 'start':   
                    $start      = $value; break;
                case 'end' :    
                    $end        = $value; break;
                case 'step':    
                    $step       = $value; break;
                case 'comparison':
                    $comparison = $value; break;
                case 'name':
                    $name       = $value; break;
            }
        }
        
        $parseStr   = '<?php $__FOR_START_'.$rand.'__='.$start.';$__FOR_END_'.$rand.'__='.$end.';';
        $parseStr  .= 'for($'.$name.'=$__FOR_START_'.$rand.'__;'.$this->parseCondition('$'.$name.' '.$comparison.' $__FOR_END_'.$rand.'__').';$'.$name.'+='.$step.'){ ?>';
        $parseStr  .= $content;
        $parseStr  .= '<?php } ?>';
        return $parseStr;
    }

    }