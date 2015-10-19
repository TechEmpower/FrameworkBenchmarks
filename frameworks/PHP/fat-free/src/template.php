<?php

/*

	Copyright (c) 2009-2015 F3::Factory/Bong Cosca, All rights reserved.

	This file is part of the Fat-Free Framework (http://fatfreeframework.com).

	This is free software: you can redistribute it and/or modify it under the
	terms of the GNU General Public License as published by the Free Software
	Foundation, either version 3 of the License, or later.

	Fat-Free Framework is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	General Public License for more details.

	You should have received a copy of the GNU General Public License along
	with Fat-Free Framework.  If not, see <http://www.gnu.org/licenses/>.

*/

//! XML-style template engine
class Template extends Preview {

	//@{ Error messages
	const
		E_Method='Call to undefined method %s()';
	//@}

	protected
		//! Template tags
		$tags,
		//! Custom tag handlers
		$custom=array();

	/**
	*	Template -set- tag handler
	*	@return string
	*	@param $node array
	**/
	protected function _set(array $node) {
		$out='';
		foreach ($node['@attrib'] as $key=>$val)
			$out.='$'.$key.'='.
				(preg_match('/\{\{(.+?)\}\}/',$val)?
					$this->token($val):
					Base::instance()->stringify($val)).'; ';
		return '<?php '.$out.'?>';
	}

	/**
	*	Template -include- tag handler
	*	@return string
	*	@param $node array
	**/
	protected function _include(array $node) {
		$attrib=$node['@attrib'];
		$hive=isset($attrib['with']) &&
			($attrib['with']=$this->token($attrib['with'])) &&
			preg_match_all('/(\w+)\h*=\h*(.+?)(?=,|$)/',
				$attrib['with'],$pairs,PREG_SET_ORDER)?
					'array('.implode(',',
						array_map(function($pair) {
							return '\''.$pair[1].'\'=>'.
								(preg_match('/^\'.*\'$/',$pair[2]) ||
									preg_match('/\$/',$pair[2])?
									$pair[2]:
									\Base::instance()->stringify($pair[2]));
						},$pairs)).')+get_defined_vars()':
					'get_defined_vars()';
		return
			'<?php '.(isset($attrib['if'])?
				('if ('.$this->token($attrib['if']).') '):'').
				('echo $this->render('.
					(preg_match('/^\{\{(.+?)\}\}$/',$attrib['href'])?
						$this->token($attrib['href']):
						Base::instance()->stringify($attrib['href'])).','.
					'$this->mime,'.$hive.'); ?>');
	}

	/**
	*	Template -exclude- tag handler
	*	@return string
	**/
	protected function _exclude() {
		return '';
	}

	/**
	*	Template -ignore- tag handler
	*	@return string
	*	@param $node array
	**/
	protected function _ignore(array $node) {
		return $node[0];
	}

	/**
	*	Template -loop- tag handler
	*	@return string
	*	@param $node array
	**/
	protected function _loop(array $node) {
		$attrib=$node['@attrib'];
		unset($node['@attrib']);
		return
			'<?php for ('.
				$this->token($attrib['from']).';'.
				$this->token($attrib['to']).';'.
				$this->token($attrib['step']).'): ?>'.
				$this->build($node).
			'<?php endfor; ?>';
	}

	/**
	*	Template -repeat- tag handler
	*	@return string
	*	@param $node array
	**/
	protected function _repeat(array $node) {
		$attrib=$node['@attrib'];
		unset($node['@attrib']);
		return
			'<?php '.
				(isset($attrib['counter'])?
					(($ctr=$this->token($attrib['counter'])).'=0; '):'').
				'foreach (('.
				$this->token($attrib['group']).'?:array()) as '.
				(isset($attrib['key'])?
					($this->token($attrib['key']).'=>'):'').
				$this->token($attrib['value']).'):'.
				(isset($ctr)?(' '.$ctr.'++;'):'').' ?>'.
				$this->build($node).
			'<?php endforeach; ?>';
	}

	/**
	*	Template -check- tag handler
	*	@return string
	*	@param $node array
	**/
	protected function _check(array $node) {
		$attrib=$node['@attrib'];
		unset($node['@attrib']);
		// Grab <true> and <false> blocks
		foreach ($node as $pos=>$block)
			if (isset($block['true']))
				$true=array($pos,$block);
			elseif (isset($block['false']))
				$false=array($pos,$block);
		if (isset($true,$false) && $true[0]>$false[0])
			// Reverse <true> and <false> blocks
			list($node[$true[0]],$node[$false[0]])=array($false[1],$true[1]);
		return
			'<?php if ('.$this->token($attrib['if']).'): ?>'.
				$this->build($node).
			'<?php endif; ?>';
	}

	/**
	*	Template -true- tag handler
	*	@return string
	*	@param $node array
	**/
	protected function _true(array $node) {
		return $this->build($node);
	}

	/**
	*	Template -false- tag handler
	*	@return string
	*	@param $node array
	**/
	protected function _false(array $node) {
		return '<?php else: ?>'.$this->build($node);
	}

	/**
	*	Template -switch- tag handler
	*	@return string
	*	@param $node array
	**/
	protected function _switch(array $node) {
		$attrib=$node['@attrib'];
		unset($node['@attrib']);
		foreach ($node as $pos=>$block)
			if (is_string($block) && !preg_replace('/\s+/','',$block))
				unset($node[$pos]);
		return
			'<?php switch ('.$this->token($attrib['expr']).'): ?>'.
				$this->build($node).
			'<?php endswitch; ?>';
	}

	/**
	*	Template -case- tag handler
	*	@return string
	*	@param $node array
	**/
	protected function _case(array $node) {
		$attrib=$node['@attrib'];
		unset($node['@attrib']);
		return
			'<?php case '.(preg_match('/\{\{(.+?)\}\}/',$attrib['value'])?
				$this->token($attrib['value']):
				Base::instance()->stringify($attrib['value'])).': ?>'.
				$this->build($node).
			'<?php '.(isset($attrib['break'])?
				'if ('.$this->token($attrib['break']).') ':'').
				'break; ?>';
	}

	/**
	*	Template -default- tag handler
	*	@return string
	*	@param $node array
	**/
	protected function _default(array $node) {
		return
			'<?php default: ?>'.
				$this->build($node).
			'<?php break; ?>';
	}

	/**
	*	Assemble markup
	*	@return string
	*	@param $node array|string
	**/
	protected function build($node) {
		if (is_string($node))
			return parent::build($node);
		$out='';
		foreach ($node as $key=>$val)
			$out.=is_int($key)?$this->build($val):$this->{'_'.$key}($val);
		return $out;
	}

	/**
	*	Extend template with custom tag
	*	@return NULL
	*	@param $tag string
	*	@param $func callback
	**/
	function extend($tag,$func) {
		$this->tags.='|'.$tag;
		$this->custom['_'.$tag]=$func;
	}

	/**
	*	Call custom tag handler
	*	@return string|FALSE
	*	@param $func callback
	*	@param $args array
	**/
	function __call($func,array $args) {
		if ($func[0]=='_')
			return call_user_func_array($this->custom[$func],$args);
		if (method_exists($this,$func))
			return call_user_func_array(array($this,$func),$args);
		user_error(sprintf(self::E_Method,$func),E_USER_ERROR);
	}

	/**
	*	Parse string for template directives and tokens
	*	@return string|array
	*	@param $text string
	**/
	function parse($text) {
		// Build tree structure
		for ($ptr=0,$len=strlen($text),$tree=array(),$node=&$tree,
			$stack=array(),$depth=0,$tmp='';$ptr<$len;)
			if (preg_match('/^<(\/?)(?:F3:)?'.
				'('.$this->tags.')\b((?:\h+[\w-]+'.
				'(?:\h*=\h*(?:"(?:.+?)"|\'(?:.+?)\'))?|'.
				'\h*\{\{.+?\}\})*)\h*(\/?)>/is',
				substr($text,$ptr),$match)) {
				if (strlen($tmp))
					$node[]=$tmp;
				// Element node
				if ($match[1]) {
					// Find matching start tag
					$save=$depth;
					$found=FALSE;
					while ($depth>0) {
						$depth--;
						foreach ($stack[$depth] as $item)
							if (is_array($item) && isset($item[$match[2]])) {
								// Start tag found
								$found=TRUE;
								break 2;
							}
					}
					if (!$found)
						// Unbalanced tag
						$depth=$save;
					$node=&$stack[$depth];
				}
				else {
					// Start tag
					$stack[$depth]=&$node;
					$node=&$node[][$match[2]];
					if ($match[3]) {
						// Process attributes
						preg_match_all(
							'/(?:\b([\w-]+)\h*'.
							'(?:=\h*(?:"(.*?)"|\'(.*?)\'))?|'.
							'(\{\{.+?\}\}))/s',
							$match[3],$attr,PREG_SET_ORDER);
						foreach ($attr as $kv)
							if (isset($kv[4]))
								$node['@attrib'][]=$kv[4];
							else
								$node['@attrib'][$kv[1]]=
									(isset($kv[2]) && $kv[2]!==''?
										$kv[2]:
										(isset($kv[3]) && $kv[3]!==''?
											$kv[3]:NULL));
					}
					if ($match[4])
						// Empty tag
						$node=&$stack[$depth];
					else
						$depth++;
				}
				$tmp='';
				$ptr+=strlen($match[0]);
			}
			else {
				// Text node
				$tmp.=substr($text,$ptr,1);
				$ptr++;
			}
		if (strlen($tmp))
			// Append trailing text
			$node[]=$tmp;
		// Break references
		unset($node);
		unset($stack);
		return $tree;
	}

	/**
	*	Class constructor
	*	return object
	**/
	function __construct() {
		$ref=new ReflectionClass(__CLASS__);
		$this->tags='';
		foreach ($ref->getmethods() as $method)
			if (preg_match('/^_(?=[[:alpha:]])/',$method->name))
				$this->tags.=(strlen($this->tags)?'|':'').
					substr($method->name,1);
	}

}
