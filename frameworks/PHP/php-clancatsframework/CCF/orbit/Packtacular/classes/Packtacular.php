<?php
/**
 * ClanCats Packtacular
 **
 * @toDo This class has just been portet for CCF2. Means the code is 
 * still messy and not pretty good have to change that soon.
 *
 * @package		Packtacular
 * @author		Mario Döring <mario@clancats.com>
 * @version		0.7
 * @copyright 	2010 - 2012 ClanCats GmbH
 *
 */
class Packtacular 
{	
	/*
	 * the cache path
	 */
	private static $path = "";
		
	/**
	 * static init
	 *
	 * @return void
	 */
	public static function _init() 
	{	
		if ( !$path = CCConfig::create( "Packtacular::packtacular" )->path ) {
			throw new CCException( "CCPacktacular - please set your packtacular path!" );
		}
		
		// is there a cache dir? 
		if ( !is_dir( PUBLICPATH.$path ) ) {
			if ( !mkdir( PUBLICPATH.$path, 0755, true ) ) {
				throw new CCException( "CCPacktacular - could not create Packtacular folder at: {$path}" );
			}
		}
		
		static::$path = '/'.$path;
	}
	
	/**
	 * handle some files
	 *
	 * @param array		$files
	 * @param string	$dir
	 * @param string	$file
	 */
	public static function handle( $files, $dir, $file ) {
		
		if ( empty( $files ) ) 
		{
			return null;
		}
		
		$last_change = static::last_change( $files );
		$file = str_replace( "{time}", $last_change, $file);
		
		// the main needed paths
		$file = static::$path.$dir.$file;
		$dir = static::$path.$dir;
		
		// does the file already exist?
		if ( file_exists( PUBLICPATH.$file ) ) {
			return $file;
		}
		
		// is there a cache dir? 
		if ( !is_dir( PUBLICPATH.$dir ) ) {
			if ( !mkdir( PUBLICPATH.$dir, 0755, true ) ) {
				throw new CCException( "CCPacktacular - could not create Packtacular folder at: {$dir}" );
			}
		}
		
		// get the pack file extention
		$ext = CCStr::extension( $file );
		
		// get all the content
		$content = "";
		foreach( $files as $tmp_file ) {
			$content .= "\n\n/*\n * file: ".str_replace( PUBLICPATH, '', $tmp_file )."\n */\n\n";
			$content .= file_get_contents( $tmp_file );

		}
		
		// call the modifier
		if ( method_exists( get_class(), "handle_".$ext ) ) {
			$content = call_user_func_array( "static::handle_".$ext, array( $content ) );
		}
		
		// save the file
		if ( !file_put_contents( PUBLICPATH.$file, $content ) ) {
			throw new CCException( "CCPacktacular - could not create packed file {$file}!" );
		}

		// return the path
		return $file;
	}
	
	/**
	 * get the last changed date of an array of files
	 *
	 * @param array		$files
	 * @return int
	 */
	protected static function last_change( $files ) {
		foreach( $files as $key => $file ) {
			$files[$key] = filemtime( $file );
		}
		
		sort( $files );  $files = array_reverse( $files );
		
		return $files[key($files)];
	}
	
	/**
	 * compile css files
	 *
	 * @param string 	$css
	 * @return string
	 */
	protected static function handle_css( $css ) 
	{
		$less = new \lessc();
		$less->setFormatter("compressed");
		//$less->setFormatter( 'compressed' );
		$less = (string) $less->parse( $css );
		// remove this random crappy ascii 160 char
		$less = utf8_encode( str_replace( chr(160), ' ', $less ) );
		$less = str_replace( 'Â', "", $less );
		return $less;
		// remove double whitespaces
		return preg_replace('/\s+/', ' ', $less );
	}
	
	/**
	 * compile css files
	 *
	 * @param string 	$css
	 * @return string
	 */
	protected static function handle_js( $js ) {
		return $js;
		$url = 'http://closure-compiler.appspot.com/compile';
		$data = array(
			'compilation_level'	=> 'SIMPLE_OPTIMIZATIONS',
			'output_format' 		=> 'text', 
			'output_info' 		=> 'compiled_code',
			'language'			=> 'ECMASCRIPT5',
			'js_code'			=> $js,
		);
		$data = http_build_query($data);
		
		$options = array(
			'http' => array(
				'method'  => 'POST',
				'header' => "Connection: close\r\n".
							"Content-type: application/x-www-form-urlencoded\r\n".
							"Content-Length: ".strlen($data)."\r\n",
				'content' => $data,
			)
		);
		$context  = stream_context_create($options);
		$result = file_get_contents($url, false, $context);
		
		
		return $result;
	}
}