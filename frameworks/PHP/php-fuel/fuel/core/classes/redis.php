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

/**
 * This code is based on Redisent, a Redis interface for the modest.
 *
 * It has been modified to work with Fuel and to improve the code slightly.
 *
 * @author Justin Poliey <justin@getglue.com>
 * @copyright 2009-2012 Justin Poliey <justin@getglue.com>
 * @license http://www.opensource.org/licenses/ISC The ISC License
 */

namespace Fuel\Core;


class RedisException extends \FuelException {}


/**
 * Redisent, a Redis interface for the modest among us
 */
class Redis
{
	/**
	 * Multiton pattern, keep track of all created instances
	 */
	protected static $instances = array();

	/**
	 * Get an instance of the Redis class
	 */
	public static function instance($name = 'default')
	{
		if ( ! array_key_exists($name, static::$instances))
		{
			// @deprecated since 1.4
			// call forge() if a new instance needs to be created, this should throw an error
			return static::forge($name);
		}

		return static::$instances[$name];
	}

	/**
	 * create an instance of the Redis class
	 */
	public static function forge($name = 'default', $config = array())
	{
		empty(static::$instances) and \Config::load('db', true);

		if ( ! ($conf = \Config::get('db.redis.'.$name)))
		{
			throw new \RedisException('Invalid instance name given.');
		}
		$config = \Arr::merge($conf, $config);

		static::$instances[$name] = new static($config);

		return static::$instances[$name];
	}

	/**
	 * @var	resource
	 */
	protected $connection = false;

	/**
	 * Flag indicating whether or not commands are being pipelined
	 *
	 * @var	boolean
	 */
	protected $pipelined = false;

	/**
	 * The queue of commands to be sent to the Redis server
	 *
	 * @var	array
	 */
	protected $queue = array();

	/**
	 * Create a new Redis instance using the configuration values supplied
	 */
	public function  __construct(array $config = array())
	{
		empty($config['timeout']) and $config['timeout'] = ini_get("default_socket_timeout");

		$this->connection = @fsockopen($config['hostname'], $config['port'], $errno, $errstr, $config['timeout']);

		if ( ! $this->connection)
		{
			throw new \RedisException($errstr, $errno);
		}
		else
		{
			// execute the auth command if a password is present in config
			empty($config['password']) or $this->auth($config['password']);
		}
	}

	/**
	 * Close the open connection on class destruction
	 */
	public function  __destruct()
	{
		$this->connection and fclose($this->connection);
	}

	/**
	 * Returns the Redisent instance ready for pipelining.
	 *
	 * Redis commands can now be chained, and the array of the responses will be
	 * returned when {@link execute} is called.
	 * @see execute
	 *
	 */
	public function pipeline()
	{
		$this->pipelined = true;

		return $this;
	}

	/**
	 * Flushes the commands in the pipeline queue to Redis and returns the responses.
	 * @see pipeline
	 */
	public function execute()
	{
		// open a Redis connection and execute the queued commands
		foreach ($this->queue as $command)
		{
			for ($written = 0; $written < strlen($command); $written += $fwrite)
			{
				$fwrite = fwrite($this->connection, substr($command, $written));
				if ($fwrite === false)
				{
					throw new \RedisException('Failed to write entire command to stream');
				}
			}
		}

		// Read in the results from the pipelined commands
		$responses = array();
		for ($i = 0; $i < count($this->queue); $i++)
		{
			$responses[] = $this->readResponse();
		}

		// Clear the queue and return the response
		$this->queue = array();

		if ($this->pipelined)
		{
			$this->pipelined = false;
			return $responses;
		}
		else
		{
			return $responses[0];
		}
	}

	/**
	 */
	public function __call($name, $args)
	{
		// build the Redis unified protocol command
		array_unshift($args, strtoupper($name));

		$command = sprintf('*%d%s%s%s', count($args), CRLF, implode(array_map(function($arg) {
			return sprintf('$%d%s%s', strlen($arg), CRLF, $arg);
		}, $args), CRLF), CRLF);

		// add it to the pipeline queue
		$this->queue[] = $command;

		if ($this->pipelined)
		{
			return $this;
		}
		else
		{
			return $this->execute();
		}
	}

	protected function readResponse()
	{
		//  parse the response based on the reply identifier
		$reply = trim(fgets($this->connection, 512));

		switch (substr($reply, 0, 1))
		{
			// error reply
			case '-':
				throw new \RedisException(trim(substr($reply, 4)));
				break;

			// inline reply
			case '+':
				$response = substr(trim($reply), 1);
				if ($response === 'OK')
				{
					$response = true;
				}
				break;

			// bulk reply
			case '$':
				$response = null;
				if ($reply == '$-1')
				{
					break;
				}
				$read = 0;
				$size = intval(substr($reply, 1));
				if ($size > 0)
				{
					do
					{
						$block_size = ($size - $read) > 1024 ? 1024 : ($size - $read);
						$r = fread($this->connection, $block_size);
						if ($r === false)
						{
							throw new \RedisException('Failed to read response from stream');
						}
						else
						{
							$read += strlen($r);
							$response .= $r;
						}
					}
					while ($read < $size);
				}

				 // discard the crlf
				fread($this->connection, 2);
				break;

			// multi-bulk reply
			case '*':
				$count = intval(substr($reply, 1));
				if ($count == '-1')
				{
					return null;
				}
				$response = array();
				for ($i = 0; $i < $count; $i++)
				{
					$response[] = $this->readResponse();
				}
				break;

			// integer reply
			case ':':
				$response = intval(substr(trim($reply), 1));
				break;

			default:
				throw new \RedisException("Unknown response: {$reply}");
				break;
		}

		// party on...
		return $response;
	}

}
