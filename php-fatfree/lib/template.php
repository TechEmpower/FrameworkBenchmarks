<?php

/*
	Copyright (c) 2009-2013 F3::Factory/Bong Cosca, All rights reserved.

	This file is part of the Fat-Free Framework (http://fatfree.sf.net).

	THE SOFTWARE AND DOCUMENTATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF
	ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
	IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR
	PURPOSE.

	Please see the license.txt file for more information.
*/

//! Template engine
class Template extends View {

	//@{ Error messages
	const
		E_Method='Call to undefined method %s()';
	//@}

	protected
		//! MIME type
		$mime,
		//! Template tags
		$tags,
		//! Custom tag handlers
		$custom=array();

	/**
	*	Convert token to variable
	*	@return string
	*	@param $str string
	**/
	function token($str) {
		$self=$this;
		$str=preg_replace_callback(
			'/(?<!\w)@(\w(?:[\w\.\[\]]|\->|::)*)/',
			function($var) use($self) {
				// Convert from JS dot notation to PHP array notation
				return '$'.preg_replace_callback(
					'/(\.\w+)|\[((?:[^\[\]]*|(?R))*)\]/',
					function($expr) use($self) {
						$fw=Base::instance();
						return
							'['.
							($expr[1]?
								$fw->stringify(substr($expr[1],1)):
								(preg_match('/^\w+/',
									$mix=$self->token($expr[2]))?
									$fw->stringify($mix):
									$mix)).
							']';
					},
					$var[1]
				);
			},
			$str
		);
		return trim(preg_replace('/{{(.+?)}}/',trim('\1'),$str));
	}

	/**
	*	Template -set- tag handler
	*	@return string
	*	@param $node array
	**/
	protected function _set(array $node) {
		$out='';
		foreach ($node['@attrib'] as $key=>$val)
			$out.='$'.$key.'='.
				(preg_match('/{{(.+?)}}/',$val)?
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
		return
			'<?php '.(isset($attrib['if'])?
				('if ('.$this->token($attrib['if']).') '):'').
				('echo $this->render('.
					(preg_match('/{{(.+?)}}/',$attrib['href'])?
						$this->token($attrib['href']):
						Base::instance()->stringify($attrib['href'])).','.
					'$this->mime,get_defined_vars()); ?>');
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
			'<?php case '.(preg_match('/{{(.+?)}}/',$attrib['value'])?
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
		if (is_string($node)) {
			$self=$this;
			return preg_replace_callback(
				'/{{(.+?)}}/s',
				function($expr) use($self) {
					$str=trim($self->token($expr[1]));
					if (preg_match('/^(.+?)\h*\|\h*(raw|esc|format)$/',
						$str,$parts))
						$str='Base::instance()->'.$parts[2].'('.$parts[1].')';
					return '<?php echo '.$str.'; ?>';
				},
				$node
			);
		}
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
		user_error(sprintf(self::E_Method,$func));
	}

	/**
	*	Render template
	*	@return string
	*	@param $file string
	*	@param $mime string
	*	@param $hive array
	**/
	function render($file,$mime='text/html',array $hive=NULL) {
		$fw=Base::instance();
		if (!is_dir($tmp=$fw->get('TEMP')))
			mkdir($tmp,Base::MODE,TRUE);
		foreach ($fw->split($fw->get('UI')) as $dir)
			if (is_file($view=$fw->fixslashes($dir.$file))) {
				if (!is_file($this->view=($tmp.
					$fw->hash($fw->get('ROOT').$fw->get('BASE')).'.'.
					$fw->hash($view).'.php')) ||
					filemtime($this->view)<filemtime($view)) {
					// Remove PHP code and comments
					$text=preg_replace('/<\?(?:php)?.+?\?>|{{\*.+?\*}}/is','',
						$fw->read($view));
					// Build tree structure
					for ($ptr=0,$len=strlen($text),$tree=array(),$node=&$tree,
						$stack=array(),$depth=0,$tmp='';$ptr<$len;)
						if (preg_match('/^<(\/?)(?:F3:)?('.$this->tags.')\b'.
							'((?:\h+\w+\h*=\h*(?:"(?:.+?)"|\'(?:.+?)\'))*)'.
							'\h*(\/?)>/is',substr($text,$ptr),$match)) {
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
										if (is_array($item) &&
											isset($item[$match[2]])) {
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
										'/\b([\w-]+)\h*=\h*'.
										'(?:"(.+?)"|\'(.+?)\')/s',
										$match[3],$attr,PREG_SET_ORDER);
									foreach ($attr as $kv)
										$node['@attrib'][$kv[1]]=
											$kv[2]?:$kv[3];
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
					$fw->write($this->view,$this->build($tree));
				}
				if (isset($_COOKIE[session_name()]))
					@session_start();
				$fw->sync('SESSION');
				if (!$hive)
					$hive=$fw->hive();
				$this->hive=$fw->get('ESCAPE')?$fw->esc($hive):$hive;
				if (PHP_SAPI!='cli')
					header('Content-Type: '.($this->mime=$mime).'; '.
						'charset='.$fw->get('ENCODING'));
				return $this->sandbox();
			}
		user_error(sprintf(Base::E_Open,$file));
	}

	function __construct() {
		$ref=new ReflectionClass(__CLASS__);
		$this->tags='';
		foreach ($ref->getmethods() as $method)
			if (preg_match('/^_(?=[[:alpha:]])/',$method->name))
				$this->tags.=(strlen($this->tags)?'|':'').
					substr($method->name,1);
	}

}
