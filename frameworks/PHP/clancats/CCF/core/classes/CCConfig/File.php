<?php namespace Core;
/**
 * Configuration Driver
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCConfig_File implements CCConfig_Driver 
{
	/**
	 * The file extesion used for the
	 */
	const EXT = ".config.php";
	
	/**
	 * Read the configuration data 
	 * Get the configuration data and return them as array
	 *
	 * @param string		$name
	 * @return array
	 */
	public function read( $name )
	{
		// check for absolut path
		if ( substr( $name, 0, 1 ) == '/' && file_exists( $name ) ) 
		{
			return $this->read_file( $name );
		}
		
		$in_namespace = false;
		
		// split the namespace
		if ( strpos( $name, '::' ) !== false ) 
		{
			$conf = explode( '::', $name );
			$in_namespace = $conf[0];
			$name = $conf[1];
			unset( $conf );
		}
		
		// the two holders
		$main = $default = array();
		
		/*
		 * App config
		 * if app config try to load the equal core config as defaults
		 */
		if ( !$in_namespace ) 
		{
			$core_conf = $this->path( CCCORE_NAMESPACE.'::'.$name, true );
			$app_conf  = $this->path( $name, true );
			
			if ( file_exists( $core_conf ) ) 
			{
				$default = $this->read_file( $core_conf );
			}
			
			if ( file_exists( $app_conf ) ) 
			{
				$main = $this->read_file( $app_conf );
			}
		}
		/*
		 * Namespaced config
		 * means use the namespaced config as default also try to load the app implementation
		 */
		elseif ( $in_namespace ) 
		{
		
			$main_conf = $this->path( $in_namespace.'::'.$name, true );
			$app_conf = $this->path( $name, true );
		
			if ( file_exists( $main_conf ) ) 
			{
				$default = $this->read_file( $main_conf );
			}
			
			if ( file_exists( $app_conf ) ) 
			{
				$main = $this->read_file( $app_conf );
			}
		}
		
		// finally return the data
		return CCArr::merge( $default, $main );
	}
	
	/**
	 * Write the configuration data
	 *
	 * @param string		$file
	 * @return void
	 */
	public function write( $file, $data )
	{
		// check for absolut path
		if ( substr( $file, 0, 1 ) == '/' ) 
		{
			return $this->write_file( $file, $data );
		}
		
		$this->write_file( $this->path( $file ), $data );
	}
	
	/**
	 * delete the configuration data
	 *
	 * @param string		$file
	 * @return void
	 */
	public function delete( $file )
	{
		// check for absolut path
		if ( substr( $file, 0, 1 ) == '/' ) 
		{
			return CCFile::delete( $file );
		}
		
		return CCFile::delete( $this->path( $file ) );
	}
	
	/**
	 * Read the file and return the data
	 *
	 * @return array
	 */
	protected function read_file( $file )
	{
		return require $file;
	}
	
	/**
	 * Write the file down to disk
	 *
	 * @return array
	 */
	protected function write_file( $file, $data )
	{
		return CCFile::write( $file, '<?php return '.var_export( $data, true ).';' );
	}
	
	/**
	 * generates a path for a config name
	 *
	 * @param string		$name
	 * @param bool		$env
	 */
	protected function path( $name, $env = false )
	{
		$conf = CCPath::get( $name, \CCDIR_CONFIG, static::EXT );
		
		if ( !$env )
		{
			return $conf;
		}
		
		$env_conf = CCPath::get( $name, \CCDIR_CONFIG.ClanCats::environment().'/', static::EXT );
		
		if ( file_exists( $env_conf ) ) 
		{
			return $env_conf;
		}
		
		return $conf;
	}
}