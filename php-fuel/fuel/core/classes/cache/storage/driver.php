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


abstract class Cache_Storage_Driver
{

	/**
	 * @var  array  defines which class properties are gettable with get_... in the __call() method
	 */
	protected static $_gettable = array('created', 'expiration', 'dependencies', 'identifier');

	/**
	 * @var  array  defines which class properties are settable with set_... in the __call() method
	 */
	protected static $_settable = array('expiration', 'dependencies', 'identifier');

	/**
	 * @var  string  name of the content handler driver
	 */
	protected $content_handler = null;

	/**
	 * @var  Cache_Handler_Driver  handles and formats the cache's contents
	 */
	protected $handler_object = null;

	/**
	 * @var  string  the cache's name, either string or md5'd serialization of something else
	 */
	protected $identifier = null;

	/**
	 * @var  int  timestamp of creation of the cache
	 */
	protected $created = null;

	/**
	 * @var  int  timestamp when this cache will expire
	 */
	protected $expiration = null;

	/**
	 * @var  array  contains identifiers of other caches this one depends on
	 */
	protected $dependencies = array();

	/**
	 * @var  mixed  the contents of this
	 */
	protected $contents = null;

	/**
	 * @var  string  loaded driver
	 */
	protected $driver = null;

	/**
	 * Abstract method that should take care of the storage engine specific reading. Needs to set the object properties:
	 * - created
	 * - expiration
	 * - dependencies
	 * - contents
	 * - content_handler
	 *
	 * @return  bool  success of the operation
	 */
	abstract protected function _get();

	/**
	 * Abstract method that should take care of the storage engine specific writing. Needs to write the object properties:
	 * - created
	 * - expiration
	 * - dependencies
	 * - contents
	 * - content_handler
	 */
	abstract protected function _set();

	/**
	 * Should delete this cache instance, should also run reset() afterwards
	 */
	abstract public function delete();

	/**
	 * Flushes the whole cache for a specific storage type or just a part of it when $section is set
	 * (might not work with all storage drivers), defaults to the default storage type
	 *
	 * @param  string
	 */
	abstract public function delete_all($section);

	/**
	 * Should check all dependencies against the creation timestamp.
	 * This is static to make it possible in the future to check dependencies from other storages then the current one,
	 * though I don't have a clue yet how to make that possible.
	 *
	 * @return  bool either true or false on any failure
	 */
	abstract public function check_dependencies(array $dependencies);

	/**
	 * Default constructor, any extension should either load this first or act similar
	 *
	 * @param  string  the identifier for this cache
	 * @param  array   additional config values
	 */
	public function __construct($identifier, $config)
	{
		$this->identifier = $identifier;

		// fetch options from config and set them
		$this->expiration       = array_key_exists('expiration', $config) ? $config['expiration'] : \Config::get('cache.expiration', null);
		$this->dependencies     = array_key_exists('dependencies', $config) ? $config['dependencies'] : array();
		$this->content_handler  = array_key_exists('content_handler', $config) ? new $config['content_handler']() : null;
		$this->driver           = array_key_exists('driver', $config) ? $config['driver'] : 'file';
	}

	/**
	 * Allows for default getting and setting
	 *
	 * @param   string
	 * @param   array
	 * @return  void|mixed
	 */
	public function __call($method, $args = array())
	{
		// Allow getting any properties set in static::$_gettable
		if (substr($method, 0, 3) == 'get')
		{
			$name = substr($method, 4);
			if (in_array($name, static::$_gettable))
			{
				return $this->{$name};
			}
			else
			{
				throw new \BadMethodCallException('This property doesn\'t exist or can\'t be read.');
			}
		}
		// Allow setting any properties set in static::$_settable
		elseif (substr($method, 0, 3) == 'set')
		{
			$name = substr($method, 4);
			if (in_array($name, static::$_settable))
			{
				$this->{$name} = @$args[0];
			}
			else
			{
				throw new \BadMethodCallException('This property doesn\'t exist or can\'t be set.');
			}
			return $this;
		}
		else
		{
			throw new \BadMethodCallException('Illegal method call: ' . $method);
		}
	}

	/**
	 * Converts the identifier to a string when necessary:
	 * A int is just converted to a string, all others are serialized and then md5'd
	 *
	 * @param   mixed
	 * @return  string
	 */
	public static function stringify_identifier($identifier)
	{
		// Identifier may not be empty, but can be false or 0
		if ($identifier === '' || $identifier === null)
		{
			throw new \FuelException('The identifier cannot be empty, must contain a value of any kind other than null or an empty string.');
		}

		// In case of string or int just return it as a string
		if (is_string($identifier) || is_int($identifier))
		{
			// cleanup to only allow alphanum chars, dashes, dots & underscores
			if (preg_match('/^([a-z0-9_\.\-]*)$/iuD', $identifier) === 0)
			{
				throw new \FuelException('Cache identifier can only contain alphanumeric characters, underscores, dashes & dots.');
			}

			return (string) $identifier;
		}
		// In case of array, bool or object return the md5 of the $identifier's serialization
		else
		{
			return '_hashes.'.md5(serialize($identifier));
		}
	}

	/**
	 * Resets all properties except for the identifier, should be run by default when a delete() is triggered
	 */
	public function reset()
	{
		$this->contents			= null;
		$this->created			= null;
		$this->expiration		= null;
		$this->dependencies		= array();
		$this->content_handler	= null;
		$this->handler_object	= null;
	}

	/**
	 * Front for writing the cache, ensures interchangeability of storage engines. Actual writing
	 * is being done by the _set() method which needs to be extended.
	 *
	 * @param   mixed                 The content to be cached
	 * @param   int                   The time in seconds until the cache will expire, =< 0 or null means no expiration
	 * @param   array                 array of names on which this cache depends for
	 * @return  Cache_Storage_Driver  The new request
	 */
	final public function set($contents = null, $expiration = false, $dependencies = array())
	{
		$contents = \Fuel::value($contents);
		// save the current expiration
		$current_expiration = $this->expiration;

		// Use either the given value or the class property
		if ( ! is_null($contents)) $this->set_contents($contents);
		$this->expiration	= ($expiration !== false) ? $expiration : $this->expiration;
		$this->dependencies	= ( ! empty($dependencies)) ? $dependencies : $this->dependencies;

		$this->created = time();

		// Create expiration timestamp when other then null
		if ( ! is_null($this->expiration))
		{
			if ( ! is_numeric($this->expiration))
			{
				throw new \InvalidArgumentException('Expiration must be a valid number.');
			}
			$this->expiration = $this->created + intval($this->expiration);
		}

		// Convert dependency identifiers to string when set
		$this->dependencies = ( ! is_array($this->dependencies)) ? array($this->dependencies) : $this->dependencies;
		if ( ! empty( $this->dependencies ) )
		{
			foreach($this->dependencies as $key => $id)
			{
				$this->dependencies[$key] = $this->stringify_identifier($id);
			}
		}

		// Turn everything over to the storage specific method
		$this->_set();

		// restore the expiration
		$this->expiration = $current_expiration;
	}

	/**
	 * Front for reading the cache, ensures interchangeability of storage engines. Actual reading
	 * is being done by the _get() method which needs to be extended.
	 *
	 * @param   bool
	 * @return  Cache_Storage_Driver
	 */
	final public function get($use_expiration = true)
	{
		if ( ! $this->_get())
		{
			throw new \CacheNotFoundException('not found');
		}

		if ($use_expiration)
		{
			if ( ! is_null($this->expiration) and $this->expiration < 0)
			{
				$this->delete();
				throw new \CacheExpiredException('expired');
			}

			// Check dependencies and handle as expired on failure
			if ( ! $this->check_dependencies($this->dependencies))
			{
				$this->delete();
				throw new \CacheExpiredException('expired');
			}
		}

		return $this->get_contents();
	}

	/**
	 * Does get() & set() in one call that takes a callback and it's arguments to generate the contents
	 *
	 * @param   string|array  Valid PHP callback
	 * @param   array         Arguments for the above function/method
	 * @param   int|null      Cache expiration in seconds
	 * @param   array         Contains the identifiers of caches this one will depend on
	 * @return  mixed
	 */
	final public function call($callback, $args = array(), $expiration = null, $dependencies = array())
	{
		try
		{
			$this->get();
		}
		catch (\CacheNotFoundException $e)
		{
			// Create the contents
			$contents = call_user_func_array($callback, $args);

			$this->set($contents, $expiration, $dependencies);
		}

		return $this->get_contents();
	}

	/**
	 * Set the contents with optional handler instead of the default
	 *
	 * @param   mixed
	 * @param   string
	 * @return  Cache_Storage_Driver
	 */
	public function set_contents($contents, $handler = NULL)
	{
		$this->contents = $contents;
		$this->set_content_handler($handler);
		$this->contents = $this->handle_writing($contents);
		return $this;
	}

	/**
	 * Fetches contents
	 *
	 * @return  mixed
	 */
	public function get_contents()
	{
		return $this->handle_reading($this->contents);
	}

	/**
	 * Decides a content handler that makes it possible to write non-strings to a file
	 *
	 * @param   string
	 * @return  Cache_Storage_Driver
	 */
	protected function set_content_handler($handler)
	{
		$this->handler_object = null;
		$this->content_handler = (string) $handler;
		return $this;
	}

	/**
	 * Gets a specific content handler
	 *
	 * @param   string
	 * @return  Cache_Handler_Driver
	 */
	public function get_content_handler($handler = null)
	{
		if ( ! empty($this->handler_object))
		{
			return $this->handler_object;
		}

		// When not yet set, use $handler or detect the preferred handler (string = string, otherwise serialize)
		if (empty($this->content_handler) && empty($handler))
		{
			if ( ! empty($handler))
			{
				$this->content_handler = $handler;
			}
			if (is_string($this->contents))
			{
				$this->content_handler = \Config::get('cache.string_handler', 'string');
			}
			else
			{
				$type = is_object($this->contents) ? get_class($this->contents) : gettype($this->contents);
				$this->content_handler = \Config::get('cache.'.$type.'_handler', 'serialized');
			}
		}

		$class = '\\Cache_Handler_'.ucfirst($this->content_handler);
		$this->handler_object = new $class();

		return $this->handler_object;
	}

	/**
	 * Converts the contents the cachable format
	 *
	 * @return  string
	 */
	protected function handle_writing($contents)
	{
		return $this->get_content_handler()->writable($contents);
	}

	/**
	 * Converts the cachable format to the original value
	 *
	 * @return  mixed
	 */
	protected function handle_reading($contents)
	{
		return $this->get_content_handler()->readable($contents);
	}
}
