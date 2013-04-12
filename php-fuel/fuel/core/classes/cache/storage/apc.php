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



class Cache_Storage_Apc extends \Cache_Storage_Driver
{

	/**
	 * @const  string  Tag used for opening & closing cache properties
	 */
	const PROPS_TAG = 'Fuel_Cache_Properties';

	/**
	 * @var  array  driver specific configuration
	 */
	protected $config = array();

	public function __construct($identifier, $config)
	{
		parent::__construct($identifier, $config);

		$this->config = isset($config['apc']) ? $config['apc'] : array();

		// make sure we have an id
		$this->config['cache_id'] = $this->_validate_config('cache_id', isset($this->config['cache_id'])
			? $this->config['cache_id'] : 'fuel');

		// check for an expiration override
		$this->expiration = $this->_validate_config('expiration', isset($this->config['expiration'])
			? $this->config['expiration'] : $this->expiration);

		// do we have the PHP APC extension available
		if ( ! function_exists('apc_store') )
		{
			throw new \FuelException('Your PHP installation doesn\'t have APC loaded.');
		}
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
		$properties = '{{'.static::PROPS_TAG.'}}'.json_encode($properties).'{{/'.static::PROPS_TAG.'}}';

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
		$properties_end = strpos($payload, '{{/'.static::PROPS_TAG.'}}');
		if ($properties_end === FALSE)
		{
			throw new \UnexpectedValueException('Cache has bad formatting');
		}

		$this->contents = substr($payload, $properties_end + strlen('{{/'.static::PROPS_TAG.'}}'));
		$props = substr(substr($payload, 0, $properties_end), strlen('{{'.static::PROPS_TAG.'}}'));
		$props = json_decode($props, true);
		if ($props === NULL)
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
			// get the section name and identifier
			$sections = explode('.', $dep);
			if (count($sections) > 1)
			{
				$identifier = array_pop($sections);
				$sections = '.'.implode('.', $sections);

			}
			else
			{
				$identifier = $dep;
				$sections = '';
			}

			// get the cache index
			$index = apc_fetch($this->config['cache_id'].$sections);

			// get the key from the index
			$key = isset($index[$identifier][0]) ? $index[$identifier] : false;

			// key found and newer?
			if ($key === false or $key[1] > $this->created)
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
		// get the APC key for the cache identifier
		$key = $this->_get_key(true);

		// delete the key from the apc store
		$key and apc_delete($key);

		$this->reset();
	}

	/**
	 * Purge all caches
	 *
	 * @param   limit purge to subsection
	 * @return  bool
	 */
	public function delete_all($section)
	{
		// determine the section index name
		$section = $this->config['cache_id'].(empty($section)?'':'.'.$section);

		// get the directory index
		$index = apc_fetch($this->config['cache_id'].'__DIR__');

		if (is_array($index))
		{
			// limit the delete if we have a valid section
			if ( ! empty($section))
			{
				$dirs = in_array($section, $index) ? array($section) : array();
			}
			else
			{
				$dirs = $index;
			}

			// loop through the indexes, delete all stored keys, then delete the indexes
			foreach ($dirs as $dir)
			{
				$list = apc_fetch($dir);
				foreach ($list as $item)
				{
					apc_delete($item[0]);
				}
				apc_delete($dir);
			}

			// update the directory index
			$index = array_diff($index, $dirs);
			apc_store($this->config['cache_id'].'__DIR__', $index);
		}
	}

	/**
	 * Save a cache, this does the generic pre-processing
	 *
	 * @return  bool  success
	 */
	protected function _set()
	{
		// get the apc key for the cache identifier
		$key = $this->_get_key();

		$payload = $this->prep_contents();

		// adjust the expiration, apc uses a TTL instead of a timestamp
		$expiration = is_null($this->expiration) ? 0 : (int) ($this->expiration - $this->created);

		// write it to the apc store
		if (apc_store($key, $payload, $expiration) === false)
		{
			throw new \RuntimeException('APC returned failed to write. Check your configuration.');
		}

		return true;
	}

	/**
	 * Load a cache, this does the generic post-processing
	 *
	 * @return  bool  success
	 */
	protected function _get()
	{
		// get the apc key for the cache identifier
		$key = $this->_get_key();

		// fetch the cached data from the apc store
		$payload = apc_fetch($key);

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

			default:
			break;
		}

		return $value;
	}

	/**
	 * Get's the apc key belonging to the cache identifier
	 *
	 * @param   bool  if true, remove the key retrieved from the index
	 * @return  string
	 */
	private function _get_key($remove = false)
	{
		// get the section name and identifier
		$sections = explode('.', $this->identifier);
		if (count($sections) > 1)
		{
			$identifier = array_pop($sections);
			$sections = '.'.implode('.', $sections);

		}
		else
		{
			$identifier = $this->identifier;
			$sections = '';
		}

		// get the cache index
		$index = apc_fetch($this->config['cache_id'].$sections);

		// get the key from the index
		$key = isset($index[$identifier][0]) ? $index[$identifier][0] : false;

		if ($remove === true)
		{
			if ( $key !== false )
			{
				unset($index[$identifier]);
				apc_store($this->config['cache_id'].$sections, $index);
			}
		}
		else
		{
			if ( $key === false )
			{
				// create a new key
				$key = $this->_new_key();

				// create a new index and store the key
				is_array($index) || $index = array();
				apc_store($this->config['cache_id'].$sections, array_merge($index, array($identifier => array($key,$this->created))), 0);

				// get the directory index
				$index = apc_fetch($this->config['cache_id'].'__DIR__');

				if (is_array($index))
				{
					if (!in_array($this->config['cache_id'].$sections, $index))
					{
						$index[] = $this->config['cache_id'].$sections;
					}
				}
				else
				{
					$index = array($this->config['cache_id'].$sections);
				}

				// update the directory index
				apc_store($this->config['cache_id'].'__DIR__', $index, 0);
			}
		}
		return $key;
	}

	/**
	 * Generate a new unique key for the current identifier
	 *
	 * @return  string
	 */
	private function _new_key()
	{
		$key = '';
		while (strlen($key) < 32)
		{
			$key .= mt_rand(0, mt_getrandmax());
		}
		return md5($this->config['cache_id'].'_'.uniqid($key, TRUE));
	}
}
