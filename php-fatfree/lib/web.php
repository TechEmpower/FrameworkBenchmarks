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

//! Wrapper for various HTTP utilities
class Web extends Prefab {

	//@{ Error messages
	const
		E_Request='No suitable HTTP request engine found';
	//@}

	private
		//! HTTP request engine
		$wrapper;

	/**
	*	Detect MIME type using file extension
	*	@return string
	*	@param $file string
	**/
	function mime($file) {
		if (preg_match('/\w+$/',$file,$ext)) {
			$map=array(
				'au'=>'audio/basic',
				'avi'=>'video/avi',
				'bmp'=>'image/bmp',
				'bz2'=>'application/x-bzip2',
				'css'=>'text/css',
				'dtd'=>'application/xml-dtd',
				'doc'=>'application/msword',
				'gif'=>'image/gif',
				'gz'=>'application/x-gzip',
				'hqx'=>'application/mac-binhex40',
				'html?'=>'text/html',
				'jar'=>'application/java-archive',
				'jpe?g'=>'image/jpeg',
				'js'=>'application/x-javascript',
				'midi'=>'audio/x-midi',
				'mp3'=>'audio/mpeg',
				'mpe?g'=>'video/mpeg',
				'ogg'=>'audio/vorbis',
				'pdf'=>'application/pdf',
				'png'=>'image/png',
				'ppt'=>'application/vnd.ms-powerpoint',
				'ps'=>'application/postscript',
				'qt'=>'video/quicktime',
				'ram?'=>'audio/x-pn-realaudio',
				'rdf'=>'application/rdf',
				'rtf'=>'application/rtf',
				'sgml?'=>'text/sgml',
				'sit'=>'application/x-stuffit',
				'svg'=>'image/svg+xml',
				'swf'=>'application/x-shockwave-flash',
				'tgz'=>'application/x-tar',
				'tiff'=>'image/tiff',
				'txt'=>'text/plain',
				'wav'=>'audio/wav',
				'xls'=>'application/vnd.ms-excel',
				'xml'=>'application/xml',
				'zip'=>'application/zip'
			);
			foreach ($map as $key=>$val)
				if (preg_match('/'.$key.'/',strtolower($ext[0])))
					return $val;
		}
		return 'application/octet-stream';
	}

	/**
	*	Return the MIME types stated in the HTTP Accept header as an array;
	*	If a list of MIME types is specified, return the best match; or
	*	FALSE if none found
	*	@return array|string|FALSE
	*	@param $list string|array
	**/
	function acceptable($list=NULL) {
		$accept=array();
		foreach (explode(',',str_replace(' ','',$_SERVER['HTTP_ACCEPT']))
			as $mime)
			if (preg_match('/(.+?)(?:;q=([\d\.]+)|$)/',$mime,$parts))
				$accept[$parts[1]]=isset($parts[2])?$parts[2]:1;
		if (!$accept)
			$accept['*/*']=1;
		else {
			krsort($accept);
			arsort($accept);
		}
		if ($list) {
			if (is_string($list))
				$list=explode(',',$list);
			foreach ($accept as $mime=>$q)
				if ($q && $out=preg_grep('/'.
					str_replace('\*','.*',preg_quote($mime,'/')).'/',$list))
					return current($out);
			return FALSE;
		}
		return $accept;
	}

	/**
	*	Transmit file to HTTP client; Return file size if successful,
	*	FALSE otherwise
	*	@return int|FALSE
	*	@param $file string
	*	@param $mime string
	*	@param $kbps int
	*	@param $force bool
	**/
	function send($file,$mime=NULL,$kbps=0,$force=TRUE) {
		if (!is_file($file))
			return FALSE;
		if (PHP_SAPI!='cli') {
			header('Content-Type: '.($mime?:$this->mime($file)));
			if ($force)
				header('Content-Disposition: attachment; '.
					'filename='.basename($file));
			header('Accept-Ranges: bytes');
			header('Content-Length: '.$size=filesize($file));
			header('X-Powered-By: '.Base::instance()->get('PACKAGE'));
		}
		$ctr=0;
		$handle=fopen($file,'rb');
		$start=microtime(TRUE);
		while (!feof($handle) &&
			($info=stream_get_meta_data($handle)) &&
			!$info['timed_out'] && !connection_aborted()) {
			if ($kbps) {
				// Throttle output
				$ctr++;
				if ($ctr/$kbps>$elapsed=microtime(TRUE)-$start)
					usleep(1e6*($ctr/$kbps-$elapsed));
			}
			// Send 1KiB and reset timer
			echo fread($handle,1024);
		}
		fclose($handle);
		return $size;
	}

	/**
	*	Receive file(s) from HTTP client
	*	@return array|bool
	*	@param $func callback
	*	@param $overwrite bool
	*	@param $slug bool
	**/
	function receive($func=NULL,$overwrite=FALSE,$slug=TRUE) {
		$fw=Base::instance();
		$dir=$fw->get('UPLOADS');
		if (!is_dir($dir))
			mkdir($dir,Base::MODE,TRUE);
		if ($fw->get('VERB')=='PUT') {
			$fw->write($dir.basename($fw->get('URI')),$fw->get('BODY'));
			return TRUE;
		}
		$out=array();
		foreach ($_FILES as $item) {
			if (is_array($item['name'])) {
				// Transpose array
				$tmp=array();
				foreach ($item as $keyx=>$cols)
					foreach ($cols as $keyy=>$valy)
						$tmp[$keyy][$keyx]=$valy;
				$item=$tmp;
			}
			else
				$item=array($item);
			foreach ($item as $file) {
				if (empty($file['name']))
					continue;
				$base=basename($file['name']);
				$dst=$dir.
					($slug && preg_match('/(.+?)(\.\w+)?$/',$base,$parts)?
						($this->slug($parts[1]).
						(isset($parts[2])?$parts[2]:'')):$base);
				$out[$dst]=!$file['error'] &&
					$file['type']==$this->mime($file['name']) &&
					(!file_exists($dst) || $overwrite) &&
					(!$func || $fw->call($func,array($file))) &&
					move_uploaded_file($file['tmp_name'],$dst);
			}
		}
		return $out;
	}

	/**
	*	Return upload progress in bytes, FALSE on failure
	*	@return int|FALSE
	*	@param $id string
	**/
	function progress($id) {
		// ID returned by session.upload_progress.name
		return ini_get('session.upload_progress.enabled') &&
			isset($_SESSION[$id]['bytes_processed'])?
				$_SESSION[$id]['bytes_processed']:FALSE;
	}

	/**
	*	HTTP request via cURL
	*	@return array
	*	@param $url string
	*	@param $options array
	**/
	protected function _curl($url,$options) {
		$curl=curl_init($url);
		curl_setopt($curl,CURLOPT_FOLLOWLOCATION,
			$options['follow_location']);
		curl_setopt($curl,CURLOPT_MAXREDIRS,
			$options['max_redirects']);
		curl_setopt($curl,CURLOPT_CUSTOMREQUEST,$options['method']);
		if (isset($options['header']))
			curl_setopt($curl,CURLOPT_HTTPHEADER,$options['header']);
		if (isset($options['user_agent']))
			curl_setopt($curl,CURLOPT_USERAGENT,$options['user_agent']);
		if (isset($options['content']))
			curl_setopt($curl,CURLOPT_POSTFIELDS,$options['content']);
		curl_setopt($curl,CURLOPT_ENCODING,'gzip,deflate');
		$timeout=isset($options['timeout'])?
			$options['timeout']:
			ini_get('default_socket_timeout');
		curl_setopt($curl,CURLOPT_CONNECTTIMEOUT,$timeout);
		curl_setopt($curl,CURLOPT_TIMEOUT,$timeout);
		$headers=array();
		curl_setopt($curl,CURLOPT_HEADERFUNCTION,
			// Callback for response headers
			function($curl,$line) use(&$headers) {
				if ($trim=trim($line))
					$headers[]=$trim;
				return strlen($line);
			}
		);
		curl_setopt($curl,CURLOPT_SSL_VERIFYPEER,FALSE);
		ob_start();
		curl_exec($curl);
		curl_close($curl);
		$body=ob_get_clean();
		return array(
			'body'=>$body,
			'headers'=>$headers,
			'engine'=>'cURL',
			'cached'=>FALSE
		);
	}

	/**
	*	HTTP request via PHP stream wrapper
	*	@return array
	*	@param $url string
	*	@param $options array
	**/
	protected function _stream($url,$options) {
		$eol="\r\n";
		$options['header']=implode($eol,$options['header']);
		$body=@file_get_contents($url,FALSE,
			stream_context_create(array('http'=>$options)));
		$headers=isset($http_response_header)?
			$http_response_header:array();
		$match=NULL;
		foreach ($headers as $header)
			if (preg_match('/Content-Encoding: (.+)/',$header,$match))
				break;
		if ($match)
			switch ($match[1]) {
				case 'gzip':
					$body=gzdecode($body);
					break;
				case 'deflate':
					$body=gzuncompress($body);
					break;
			}
		return array(
			'body'=>$body,
			'headers'=>$headers,
			'engine'=>'stream',
			'cached'=>FALSE
		);
	}

	/**
	*	HTTP request via low-level TCP/IP socket
	*	@return array
	*	@param $url string
	*	@param $options array
	**/
	protected function _socket($url,$options) {
		$eol="\r\n";
		$headers=array();
		$body='';
		$parts=parse_url($url);
		if ($parts['scheme']=='https') {
			$parts['host']='ssl://'.$parts['host'];
			$parts['port']=443;
		}
		else
			$parts['port']=80;
		if (empty($parts['path']))
			$parts['path']='/';
		if (empty($parts['query']))
			$parts['query']='';
		$socket=@fsockopen($parts['host'],$parts['port']);
		if (!$socket)
			return FALSE;
		stream_set_blocking($socket,TRUE);
		fputs($socket,$options['method'].' '.$parts['path'].
			($parts['query']?('?'.$parts['query']):'').' HTTP/1.0'.$eol
		);
		fputs($socket,implode($eol,$options['header']).$eol.$eol);
		if (isset($options['content']))
			fputs($socket,$options['content'].$eol);
		// Get response
		$content='';
		while (!feof($socket) &&
			($info=stream_get_meta_data($socket)) &&
			!$info['timed_out'] && $str=fgets($socket,4096))
			$content.=$str;
		fclose($socket);
		$html=explode($eol.$eol,$content,2);
		$body=isset($html[1])?$html[1]:'';
		$headers=array_merge($headers,$current=explode($eol,$html[0]));
		$match=NULL;
		foreach ($current as $header)
			if (preg_match('/Content-Encoding: (.+)/',$header,$match))
				break;
		if ($match)
			switch ($match[1]) {
				case 'gzip':
					$body=gzdecode($body);
					break;
				case 'deflate':
					$body=gzuncompress($body);
					break;
			}
		if ($options['follow_location'] &&
			preg_match('/Location: (.+?)'.preg_quote($eol).'/',
			$html[0],$loc)) {
			$options['max_redirects']--;
			return $this->request($loc[1],$options);
		}
		return array(
			'body'=>$body,
			'headers'=>$headers,
			'engine'=>'socket',
			'cached'=>FALSE
		);
	}

	/**
	*	Specify the HTTP request engine to use; If not available,
	*	fall back to an applicable substitute
	*	@return string
	*	@param $arg string
	**/
	function engine($arg='socket') {
		$arg=strtolower($arg);
		$flags=array(
			'curl'=>extension_loaded('curl'),
			'stream'=>ini_get('allow_url_fopen'),
			'socket'=>function_exists('fsockopen')
		);
		if ($flags[$arg])
			return $this->wrapper=$arg;
		foreach ($flags as $key=>$val)
			if ($val)
				return $this->wrapper=$key;
		user_error(E_Request);
	}

	/**
	*	Replace old headers with new elements
	*	@return NULL
	*	@param $old array
	*	@param $new string|array
	**/
	function subst(array &$old,$new) {
		if (is_string($new))
			$new=array($new);
		foreach ($new as $hdr) {
			$old=preg_grep('/'.preg_quote(strstr($hdr,':',TRUE),'/').':.+/',
					$old,PREG_GREP_INVERT);
			array_push($old,$hdr);
		}
	}

	/**
	*	Submit HTTP request; Use HTTP context options (described in
	*	http://www.php.net/manual/en/context.http.php) if specified;
	*	Cache the page as instructed by remote server
	*	@return array|FALSE
	*	@param $url string
	*	@param $options array
	**/
	function request($url,array $options=NULL) {
		$fw=Base::instance();
		$parts=parse_url($url);
		if (empty($parts['scheme'])) {
			// Local URL
			$url=$fw->get('SCHEME').'://'.
				$fw->get('HOST').
				($url[0]!='/'?($fw->get('BASE').'/'):'').$url;
			$parts=parse_url($url);
		}
		elseif (!preg_match('/https?/',$parts['scheme']))
			return FALSE;
		if (!is_array($options))
			$options=array();
		if (empty($options['header']))
			$options['header']=array();
		elseif (is_string($options['header']))
			$options['header']=array($options['header']);
		if (!$this->wrapper)
			$this->engine();
		if ($this->wrapper!='stream') {
			// PHP streams can't cope with redirects when Host header is set
			foreach ($options['header'] as &$header)
				if (preg_match('/^Host:/',$header)) {
					$header='Host: '.$parts['host'];
					unset($header);
					break;
				}
			$this->subst($options['header'],'Host: '.$parts['host']);
		}
		$this->subst($options['header'],
			array(
				'Accept-Encoding: gzip,deflate',
				'User-Agent: Mozilla/5.0 (compatible; '.php_uname('s').')',
				'Connection: close'
			)
		);
		if (isset($options['content'])) {
			if ($options['method']=='POST')
				$this->subst($options['header'],
					'Content-Type: application/x-www-form-urlencoded');
			$this->subst($options['header'],
				'Content-Length: '.strlen($options['content']));
		}
		if (isset($parts['user'],$parts['pass']))
			$this->subst($options['header'],
				'Authorization: Basic '.
					base64_encode($parts['user'].':'.$parts['pass'])
			);
		$options+=array(
			'method'=>'GET',
			'header'=>$options['header'],
			'follow_location'=>TRUE,
			'max_redirects'=>20,
			'ignore_errors'=>FALSE
		);
		$eol="\r\n";
		if ($fw->get('CACHE') &&
			preg_match('/GET|HEAD/',$options['method'])) {
			$cache=Cache::instance();
			if ($cache->exists(
				$hash=$fw->hash($options['method'].' '.$url).'.url',$data)) {
				if (preg_match('/Last-Modified: (.+?)'.preg_quote($eol).'/',
					implode($eol,$data['headers']),$mod))
					$this->subst($options['header'],
						'If-Modified-Since: '.$mod[1]);
			}
		}
		$result=$this->{'_'.$this->wrapper}($url,$options);
		if ($result && isset($cache)) {
			if (preg_match('/HTTP\/1\.\d 304/',
				implode($eol,$result['headers']))) {
				$result=$cache->get($hash);
				$result['cached']=TRUE;
			}
			elseif (preg_match('/Cache-Control: max-age=(.+?)'.
				preg_quote($eol).'/',implode($eol,$result['headers']),$exp))
				$cache->set($hash,$result,$exp[1]);
		}
		return $result;
	}

	/**
	*	Strip Javascript/CSS files of extraneous whitespaces and comments;
	*	Return combined output as a minified string
	*	@return string
	*	@param $files string|array
	*	@param $mime string
	*	@param $header bool
	*	@param $path string
	**/
	function minify($files,$mime=NULL,$header=TRUE,$path='') {
		$fw=Base::instance();
		if (is_string($files))
			$files=$fw->split($files);
		if (!$mime)
			$mime=$this->mime($files[0]);
		preg_match('/\w+$/',$files[0],$ext);
		$cache=Cache::instance();
		$dst='';
		foreach ($fw->split($path?:$fw->get('UI')) as $dir)
			foreach ($files as $file)
				if (is_file($save=$fw->fixslashes($dir.$file))) {
					if ($fw->get('CACHE') &&
						($cached=$cache->exists(
							$hash=$fw->hash($save).'.'.$ext[0],$data)) &&
						$cached[0]>filemtime($save))
						$dst.=$data;
					else {
						$data='';
						$src=$fw->read($save);
						for ($ptr=0,$len=strlen($src);$ptr<$len;) {
							if (preg_match('/^@import\h+url'.
								'\(\h*([\'"])(.+?)\1\h*\)[^;]*;/',
								substr($src,$ptr),$parts)) {
								$path=dirname($file);
								$data.=$this->minify(
									($path?($path.'/'):'').$parts[2],
									$mime,$header
								);
								$ptr+=strlen($parts[0]);
								continue;
							}
							if ($src[$ptr]=='/') {
								if ($src[$ptr+1]=='*') {
									// Multiline comment
									$str=strstr(
										substr($src,$ptr+2),'*/',TRUE);
									$ptr+=strlen($str)+4;
								}
								elseif ($src[$ptr+1]=='/') {
									// Single-line comment
									$str=strstr(
										substr($src,$ptr+2),"\n",TRUE);
									$ptr+=strlen($str)+2;
								}
								else {
									// Presume it's a regex pattern
									$regex=TRUE;
									// Backtrack and validate
									for ($ofs=$ptr;$ofs;$ofs--) {
										// Pattern should be preceded by
										// open parenthesis, colon,
										// object property or operator
										if (preg_match(
											'/(return|[(:=!+\-*&|])$/',
											substr($src,0,$ofs))) {
											$data.='/';
											$ptr++;
											while ($ptr<$len) {
												$data.=$src[$ptr];
												$ptr++;
												if ($src[$ptr-1]=='\\') {
													$data.=$src[$ptr];
													$ptr++;
												}
												elseif ($src[$ptr-1]=='/')
													break;
											}
											break;
										}
										elseif (!ctype_space($src[$ofs-1])) {
											// Not a regex pattern
											$regex=FALSE;
											break;
										}
									}
									if (!$regex) {
										// Division operator
										$data.=$src[$ptr];
										$ptr++;
									}
								}
								continue;
							}
							if (in_array($src[$ptr],array('\'','"'))) {
								$match=$src[$ptr];
								$data.=$match;
								$ptr++;
								// String literal
								while ($ptr<$len) {
									$data.=$src[$ptr];
									$ptr++;
									if ($src[$ptr-1]=='\\') {
										$data.=$src[$ptr];
										$ptr++;
									}
									elseif ($src[$ptr-1]==$match)
										break;
								}
								continue;
							}
							if (ctype_space($src[$ptr])) {
								if ($ptr+1<strlen($src) &&
									preg_match('/[\w'.($ext[0]=='css'?
										'#\.+\-*()\[\]':'\$').']{2}|'.
										'[+\-]{2}/',
										substr($data,-1).$src[$ptr+1]))
									$data.=' ';
								$ptr++;
								continue;
							}
							$data.=$src[$ptr];
							$ptr++;
						}
						if ($fw->get('CACHE'))
							$cache->set($hash,$data);
						$dst.=$data;
					}
				}
		if (PHP_SAPI!='cli' && $header)
			header('Content-Type: '.$mime.'; charset='.$fw->get('ENCODING'));
		return $dst;
	}

	/**
	*	Retrieve RSS feed and return as an array
	*	@return array|FALSE
	*	@param $url string
	*	@param $max int
	*	@param $tags string
	**/
	function rss($url,$max=10,$tags=NULL) {
		if (!$data=$this->request($url))
			return FALSE;
		// Suppress errors caused by invalid XML structures
		libxml_use_internal_errors(TRUE);
		$xml=simplexml_load_string($data['body'],
			NULL,LIBXML_NOBLANKS|LIBXML_NOERROR);
		if (!is_object($xml))
			return FALSE;
		$out=array();
		if (isset($xml->channel)) {
			$out['source']=(string)$xml->channel->title;
			$max=min($max,count($xml->channel->item));
			for ($i=0;$i<$max;$i++) {
				$item=$xml->channel->item[$i];
				$list=array(''=>NULL)+$item->getnamespaces(TRUE);
				$fields=array();
				foreach ($list as $ns=>$uri)
					foreach ($item->children($uri) as $key=>$val)
						$fields[$ns.($ns?':':'').$key]=(string)$val;
				$out['feed'][]=$fields;
			}
		}
		else
			return FALSE;
		Base::instance()->scrub($out,$tags);
		return $out;
	}

	/**
	*	Retrieve information from whois server
	*	@return string|FALSE
	*	@param $addr string
	*	@param $server string
	**/
	function whois($addr,$server='whois.internic.net') {
		$socket=@fsockopen($server,43,$errno,$errstr);
		if (!$socket)
			// Can't establish connection
			return FALSE;
		// Set connection timeout parameters
		stream_set_blocking($socket,TRUE);
		stream_set_timeout($socket,ini_get('default_socket_timeout'));
		// Send request
		fputs($socket,$addr."\r\n");
		$info=stream_get_meta_data($socket);
		// Get response
		$response='';
		while (!feof($socket) && !$info['timed_out']) {
			$response.=fgets($socket,4096); // MDFK97
			$info=stream_get_meta_data($socket);
		}
		fclose($socket);
		return $info['timed_out']?FALSE:trim($response);
	}

	/**
	*	Return a URL/filesystem-friendly version of string
	*	@return string
	*	@param $text string
	**/
	function slug($text) {
		return trim(strtolower(preg_replace('/([^\pL\pN])+/u','-',
			trim(strtr(str_replace('\'','',$text),
			Base::instance()->get('DIACRITICS')+
			array(
				'Ǎ'=>'A','А'=>'A','Ā'=>'A','Ă'=>'A','Ą'=>'A','Å'=>'A',
				'Ǻ'=>'A','Ä'=>'Ae','Á'=>'A','À'=>'A','Ã'=>'A','Â'=>'A',
				'Æ'=>'AE','Ǽ'=>'AE','Б'=>'B','Ç'=>'C','Ć'=>'C','Ĉ'=>'C',
				'Č'=>'C','Ċ'=>'C','Ц'=>'C','Ч'=>'Ch','Ð'=>'Dj','Đ'=>'Dj',
				'Ď'=>'Dj','Д'=>'Dj','É'=>'E','Ę'=>'E','Ё'=>'E','Ė'=>'E',
				'Ê'=>'E','Ě'=>'E','Ē'=>'E','È'=>'E','Е'=>'E','Э'=>'E',
				'Ë'=>'E','Ĕ'=>'E','Ф'=>'F','Г'=>'G','Ģ'=>'G','Ġ'=>'G',
				'Ĝ'=>'G','Ğ'=>'G','Х'=>'H','Ĥ'=>'H','Ħ'=>'H','Ï'=>'I',
				'Ĭ'=>'I','İ'=>'I','Į'=>'I','Ī'=>'I','Í'=>'I','Ì'=>'I',
				'И'=>'I','Ǐ'=>'I','Ĩ'=>'I','Î'=>'I','Ĳ'=>'IJ','Ĵ'=>'J',
				'Й'=>'J','Я'=>'Ja','Ю'=>'Ju','К'=>'K','Ķ'=>'K','Ĺ'=>'L',
				'Л'=>'L','Ł'=>'L','Ŀ'=>'L','Ļ'=>'L','Ľ'=>'L','М'=>'M',
				'Н'=>'N','Ń'=>'N','Ñ'=>'N','Ņ'=>'N','Ň'=>'N','Ō'=>'O',
				'О'=>'O','Ǿ'=>'O','Ǒ'=>'O','Ơ'=>'O','Ŏ'=>'O','Ő'=>'O',
				'Ø'=>'O','Ö'=>'Oe','Õ'=>'O','Ó'=>'O','Ò'=>'O','Ô'=>'O',
				'Œ'=>'OE','П'=>'P','Ŗ'=>'R','Р'=>'R','Ř'=>'R','Ŕ'=>'R',
				'Ŝ'=>'S','Ş'=>'S','Š'=>'S','Ș'=>'S','Ś'=>'S','С'=>'S',
				'Ш'=>'Sh','Щ'=>'Shch','Ť'=>'T','Ŧ'=>'T','Ţ'=>'T','Ț'=>'T',
				'Т'=>'T','Ů'=>'U','Ű'=>'U','Ŭ'=>'U','Ũ'=>'U','Ų'=>'U',
				'Ū'=>'U','Ǜ'=>'U','Ǚ'=>'U','Ù'=>'U','Ú'=>'U','Ü'=>'Ue',
				'Ǘ'=>'U','Ǖ'=>'U','У'=>'U','Ư'=>'U','Ǔ'=>'U','Û'=>'U',
				'В'=>'V','Ŵ'=>'W','Ы'=>'Y','Ŷ'=>'Y','Ý'=>'Y','Ÿ'=>'Y',
				'Ź'=>'Z','З'=>'Z','Ż'=>'Z','Ž'=>'Z','Ж'=>'Zh','á'=>'a',
				'ă'=>'a','â'=>'a','à'=>'a','ā'=>'a','ǻ'=>'a','å'=>'a',
				'ä'=>'ae','ą'=>'a','ǎ'=>'a','ã'=>'a','а'=>'a','ª'=>'a',
				'æ'=>'ae','ǽ'=>'ae','б'=>'b','č'=>'c','ç'=>'c','ц'=>'c',
				'ċ'=>'c','ĉ'=>'c','ć'=>'c','ч'=>'ch','ð'=>'dj','ď'=>'dj',
				'д'=>'dj','đ'=>'dj','э'=>'e','é'=>'e','ё'=>'e','ë'=>'e',
				'ê'=>'e','е'=>'e','ĕ'=>'e','è'=>'e','ę'=>'e','ě'=>'e',
				'ė'=>'e','ē'=>'e','ƒ'=>'f','ф'=>'f','ġ'=>'g','ĝ'=>'g',
				'ğ'=>'g','г'=>'g','ģ'=>'g','х'=>'h','ĥ'=>'h','ħ'=>'h',
				'ǐ'=>'i','ĭ'=>'i','и'=>'i','ī'=>'i','ĩ'=>'i','į'=>'i',
				'ı'=>'i','ì'=>'i','î'=>'i','í'=>'i','ï'=>'i','ĳ'=>'ij',
				'ĵ'=>'j','й'=>'j','я'=>'ja','ю'=>'ju','ķ'=>'k','к'=>'k',
				'ľ'=>'l','ł'=>'l','ŀ'=>'l','ĺ'=>'l','ļ'=>'l','л'=>'l',
				'м'=>'m','ņ'=>'n','ñ'=>'n','ń'=>'n','н'=>'n','ň'=>'n',
				'ŉ'=>'n','ó'=>'o','ò'=>'o','ǒ'=>'o','ő'=>'o','о'=>'o',
				'ō'=>'o','º'=>'o','ơ'=>'o','ŏ'=>'o','ô'=>'o','ö'=>'oe',
				'õ'=>'o','ø'=>'o','ǿ'=>'o','œ'=>'oe','п'=>'p','р'=>'r',
				'ř'=>'r','ŕ'=>'r','ŗ'=>'r','ſ'=>'s','ŝ'=>'s','ș'=>'s',
				'š'=>'s','ś'=>'s','с'=>'s','ş'=>'s','ш'=>'sh','щ'=>'shch',
				'ß'=>'ss','ţ'=>'t','т'=>'t','ŧ'=>'t','ť'=>'t','ț'=>'t',
				'у'=>'u','ǘ'=>'u','ŭ'=>'u','û'=>'u','ú'=>'u','ų'=>'u',
				'ù'=>'u','ű'=>'u','ů'=>'u','ư'=>'u','ū'=>'u','ǚ'=>'u',
				'ǜ'=>'u','ǔ'=>'u','ǖ'=>'u','ũ'=>'u','ü'=>'ue','в'=>'v',
				'ŵ'=>'w','ы'=>'y','ÿ'=>'y','ý'=>'y','ŷ'=>'y','ź'=>'z',
				'ž'=>'z','з'=>'z','ż'=>'z','ж'=>'zh'
			))))),'-');
	}

	/**
	*	Return chunk of text from standard Lorem Ipsum passage
	*	@return string
	*	@param $count int
	*	@param $max int
	*	@param $std bool
	**/
	function filler($count=1,$max=20,$std=TRUE) {
		$out='';
		if ($std)
			$out='Lorem ipsum dolor sit amet, consectetur adipisicing elit, '.
				'sed do eiusmod tempor incididunt ut labore et dolore magna '.
				'aliqua.';
		$rnd=explode(' ',
			'a ab ad accusamus adipisci alias aliquam amet animi aperiam '.
			'architecto asperiores aspernatur assumenda at atque aut beatae '.
			'blanditiis cillum commodi consequatur corporis corrupti culpa '.
			'cum cupiditate debitis delectus deleniti deserunt dicta '.
			'dignissimos distinctio dolor ducimus duis ea eaque earum eius '.
			'eligendi enim eos error esse est eum eveniet ex excepteur '.
			'exercitationem expedita explicabo facere facilis fugiat harum '.
			'hic id illum impedit in incidunt ipsa iste itaque iure iusto '.
			'laborum laudantium libero magnam maiores maxime minim minus '.
			'modi molestiae mollitia nam natus necessitatibus nemo neque '.
			'nesciunt nihil nisi nobis non nostrum nulla numquam occaecati '.
			'odio officia omnis optio pariatur perferendis perspiciatis '.
			'placeat porro possimus praesentium proident quae quia quibus '.
			'quo ratione recusandae reiciendis rem repellat reprehenderit '.
			'repudiandae rerum saepe sapiente sequi similique sint soluta '.
			'suscipit tempora tenetur totam ut ullam unde vel veniam vero '.
			'vitae voluptas');
		for ($i=0,$add=$count-(int)$std;$i<$add;$i++) {
			shuffle($rnd);
			$words=array_slice($rnd,0,mt_rand(3,$max));
			$out.=' '.ucfirst(implode(' ',$words)).'.';
		}
		return $out;
	}

}

if (!function_exists('gzdecode')) {

	/**
	*	Decode gzip-compressed string
	*	@param $data string
	**/
	function gzdecode($str) {
		$fw=Base::instance();
		if (!is_dir($tmp=$fw->get('TEMP')))
			mkdir($tmp,Base::MODE,TRUE);
		file_put_contents($file=$tmp.'/'.
			$fw->hash($fw->get('ROOT').$fw->get('BASE')).'.'.
			$fw->hash(uniqid()).'.gz',$str,LOCK_EX);
		ob_start();
		readgzfile($file);
		$out=ob_get_clean();
		@unlink($file);
		return $out;
	}

}
