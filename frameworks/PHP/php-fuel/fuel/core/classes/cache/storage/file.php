<?php
/**
 * Part of the Fuel framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

namespace Fuel\Core;



class Cache_Storage_File extends \Cache_Storage_Driver
{

	/**
	 * @const  string  Tag used for opening & closing cache properties
	 */
	const PROPS_TAG = 'Fuel_Cache_Properties';

	/**
	 * @var  string  File caching basepath
	 */
	protected static $path = '';

	/**
	 * @var  array  driver specific configuration
	 */
	protected $config = array();

	public function __construct($identifier, $config)
	{
		parent::__construct($identifier, $config);

		$this->config = isset($config['file']) ? $config['file'] : array();

		// check for an expiration override
		$this->expiration = $this->_validate_config('expiration', isset($this->config['expiration'])
			? $this->config['expiration'] : $this->expiration);

		// determine the file cache path
		static::$path = !empty($this->config['path'])
			? $this->config['path'] : \Config::get('cache_dir', APPPATH.'cache'.DS);

		if ( ! is_dir(static::$path) || ! is_writable(static::$path))
		{
			throw new \FuelException('Cache directory does not exist or is not writable.');
		}
	}

	/**
	 * Translates a given identifier to a valid path
	 *
	 * @param   string
	 * @return  string
	 */
	protected function identifier_to_path($identifier)
	{
		// replace dots with dashes
		$identifier = str_replace('.', DS, $identifier);

		return $identifier;
	}

	/**
	 * Prepend the cache properties
	 *
	 * @return  string
	 */
	protected function prep_contents()
	{
		$properties = array(
			'created'          => $this->created,
			'expiration'       => $this->expiration,
			'dependencies'     => $this->dependencies,
			'content_handler'  => $this->content_handler
		);
		$properties = '{{'.self::PROPS_TAG.'}}'.json_encode($properties).'{{/'.self::PROPS_TAG.'}}';

		return $properties.$this->contents;
	}

	/**
	 * Remove the prepended cache properties and save them in class properties
	 *
	 * @param   string
	 * @throws  UnexpectedValueException
	 */
	protected function unprep_contents($payload)
	{
		$properties_end = strpos($payload, '{{/'.self::PROPS_TAG.'}}');
		if ($properties_end === false)
		{
			throw new \UnexpectedValueException('Cache has bad formatting');
		}

		$this->contents = substr($payload, $properties_end + strlen('{{/'.self::PROPS_TAG.'}}'));
		$props = substr(substr($payload, 0, $properties_end), strlen('{{'.self::PROPS_TAG.'}}'));
		$props = json_decode($props, true);
		if ($props === null)
		{
			throw new \UnexpectedValueException('Cache properties retrieval failed');
		}

		$this->created          = $props['created'];
		$this->expiration       = is_null($props['expiration']) ? null : (int) ($props['expiration'] - time());
		$this->dependencies     = $props['dependencies'];
		$this->content_handler  = $props['content_handler'];
	}

	/**
	 * Check if other caches or files have been changed since cache creation
	 *
	 * @param   array
	 * @return  bool
	 */
	public function check_dependencies(array $dependencies)
	{
		foreach($dependencies as $dep)
		{
			if (file_exists($file = static::$path.str_replace('.', DS, $dep).'.cache'))
			{
				$filemtime = filemtime($file);
				if ($filemtime === false || $filemtime > $this->created)
				{
					return false;
				}
			}
			elseif (file_exists($dep))
			{
				$filemtime = filemtime($file);
				if ($filemtime === false || $filemtime > $this->created)
				{
					return false;
				}
			}
			else
			{
				return false;
			}
		}
		return true;
	}

	/**
	 * Delete Cache
	 */
	public function delete()
	{
		if (file_exists($file = static::$path.$this->identifier_to_path($this->identifier).'.cache'))
		{
			unlink($file);
			$this->reset();
		}
	}

	// ---------------------------------------------------------------------

	/**
	 * Purge all caches
	 *
	 * @param   limit purge to subsection
	 * @return  bool
	 */
	public function delete_all($section)
	{
		$path = rtrim(static::$path, '\\/').DS;
		$section = static::identifier_to_path($section);

		$files = \File::read_dir($path.$section, -1, array('\.cache$' => 'file'));

		$delete = function($path, $files) use(&$delete, &$section)
		{
			$path = rtrim($path, '\\/').DS;

			foreach ($files as $dir => $file)
			{
				if (is_numeric($dir))
				{
					if ( ! $result = \File::delete($path.$file))
					{
						return $result;
					}
				}
				else
				{
					if ( ! $result = ($delete($path.$dir, $file) and rmdir($path.$dir)))
					{
						return $result;
					}
				}
			}

			$section !== '' and rmdir($path);

			return true;
		};

		return $delete($path.$section, $files);
	}

	/**
	 * Save a cache, this does the generic pre-processing
	 *
	 * @return  bool  success
	 */
	protected function _set()
	{
		$payload = $this->prep_contents();
		$id_path = $this->identifier_to_path($this->identifier);

		// create directory if necessary
		$subdirs = explode(DS, $id_path);
		if (count($subdirs) > 1)
		{
			array_pop($subdirs);
			$test_path = static::$path.implode(DS, $subdirs);

			// check if specified subdir exists
			if ( ! @is_dir($test_path))
			{
				// create non existing dir
				if ( ! @mkdir($test_path, 0755, true))
				{
					return false;
				}
			}
		}

		// write the cache
		$file = static::$path.$id_path.'.cache';
		$handle = fopen($file, 'c');

		if ( ! $handle)
		{
			return false;
		}

		// wait for a lock
		while ( ! flock($handle, LOCK_EX));

		// truncate the file
		ftruncate($handle, 0);

		// write the session data
		fwrite($handle, $payload);

		//release the lock
		flock($handle, LOCK_UN);

		// close the file
		fclose($handle);

		return true;
	}

	/**
	 * Load a cache, this does the generic post-processing
	 *
	 * @return  bool  success
	 */
	protected function _get()
	{
		$id_path = $this->identifier_to_path( $this->identifier );
		$file = static::$path.$id_path.'.cache';
		if ( ! file_exists($file))
		{
			return false;
		}

		$handle = fopen($file, 'r');
		if ( ! $handle)
		{
			return false;
		}

		// wait for a lock
		while( ! flock($handle, LOCK_SH));

		// read the session data
		$payload = fread($handle, filesize($file));

		//release the lock
		flock($handle, LOCK_UN);

		// close the file
		fclose($handle);

		try
		{
			$this->unprep_contents($payload);
		}
		catch (\UnexpectedValueException $e)
		{
			return false;
		}

		return true;
	}

	/**
	 * validate a driver config value
	 *
	 * @param   string  name of the config variable to validate
	 * @param   mixed   value
	 * @return  mixed
	 */
	private function _validate_config($name, $value)
	{
		switch ($name)
		{
			case 'cache_id':
				if (empty($value) or ! is_string($value))
				{
					$value = 'fuel';
				}
			break;

			case 'expiration':
				if (empty($value) or ! is_numeric($value))
				{
					$value = null;
				}
			break;
		}

		return $value;
	}
}
