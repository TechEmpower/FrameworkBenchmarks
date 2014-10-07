<?php namespace Laravel;

class Messages {

	/**
	 * All of the registered messages.
	 *
	 * @var array
	 */
	public $messages;

	/**
	 * Default format for message output.
	 *
	 * @var string
	 */	
	public $format = ':message';

	/**
	 * Create a new Messages instance.
	 *
	 * @param  array  $messages
	 * @return void
	 */
	public function __construct($messages = array())
	{
		$this->messages = (array) $messages;
	}

	/**
	 * Add a message to the collector.
	 *
	 * <code>
	 *		// Add a message for the e-mail attribute
	 *		$messages->add('email', 'The e-mail address is invalid.');
	 * </code>
	 *
	 * @param  string  $key
	 * @param  string  $message
	 * @return void
	 */
	public function add($key, $message)
	{
		if ($this->unique($key, $message)) $this->messages[$key][] = $message;
	}

	/**
	 * Determine if a key and message combination already exists.
	 *
	 * @param  string  $key
	 * @param  string  $message
	 * @return bool
	 */
	protected function unique($key, $message)
	{
		return ! isset($this->messages[$key]) or ! in_array($message, $this->messages[$key]);
	}

	/**
	 * Determine if messages exist for a given key.
	 *
	 * <code>
	 *		// Is there a message for the e-mail attribute
	 *		return $messages->has('email');
	 *
	 *		// Is there a message for the any attribute
	 *		echo $messages->has();
	 * </code>
	 *
	 * @param  string  $key
	 * @return bool
	 */
	public function has($key = null)
	{
		return $this->first($key) !== '';
	}

	/**
	 * Set the default message format for output.
	 *
	 * <code>
	 *		// Apply a new default format.
	 *		$messages->format('email', '<p>this is my :message</p>');
	 * </code>
	 *
	 * @param  string  $format
	 */
	public function format($format = ':message')
	{
		$this->format = $format;
	}

	/**
	 * Get the first message from the container for a given key.
	 *
	 * <code>
	 *		// Echo the first message out of all messages.
	 *		echo $messages->first();
	 *
	 *		// Echo the first message for the e-mail attribute
	 *		echo $messages->first('email');
	 *
	 *		// Format the first message for the e-mail attribute
	 *		echo $messages->first('email', '<p>:message</p>');
	 * </code>
	 *
	 * @param  string  $key
	 * @param  string  $format
	 * @return string
	 */
	public function first($key = null, $format = null)
	{
		$format = ($format === null) ? $this->format : $format;

		$messages = is_null($key) ? $this->all($format) : $this->get($key, $format);

		return (count($messages) > 0) ? $messages[0] : '';
	}

	/**
	 * Get all of the messages from the container for a given key.
	 *
	 * <code>
	 *		// Echo all of the messages for the e-mail attribute
	 *		echo $messages->get('email');
	 *
	 *		// Format all of the messages for the e-mail attribute
	 *		echo $messages->get('email', '<p>:message</p>');
	 * </code>
	 *
	 * @param  string  $key
	 * @param  string  $format
	 * @return array
	 */
	public function get($key, $format = null)
	{
		$format = ($format === null) ? $this->format : $format;

		if (array_key_exists($key, $this->messages))
		{
			return $this->transform($this->messages[$key], $format);
		}

		return array();
	}

	/**
	 * Get all of the messages for every key in the container.
	 *
	 * <code>
	 *		// Get all of the messages in the collector
	 *		$all = $messages->all();
	 *
	 *		// Format all of the messages in the collector
	 *		$all = $messages->all('<p>:message</p>');
	 * </code>
	 *
	 * @param  string  $format
	 * @return array
	 */
	public function all($format = null)
	{
		$format = ($format === null) ? $this->format : $format;

		$all = array();

		foreach ($this->messages as $messages)
		{
			$all = array_merge($all, $this->transform($messages, $format));
		}

		return $all;
	}

	/**
	 * Format an array of messages.
	 *
	 * @param  array   $messages
	 * @param  string  $format
	 * @return array
	 */
	protected function transform($messages, $format)
	{
		$messages = (array) $messages;

		foreach ($messages as $key => &$message)
		{
			$message = str_replace(':message', $message, $format);
		}

		return $messages;
	}

}