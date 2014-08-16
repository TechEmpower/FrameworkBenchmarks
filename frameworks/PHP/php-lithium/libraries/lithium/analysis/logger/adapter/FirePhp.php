<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2012, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\analysis\logger\adapter;

/**
 * The `FirePhp` log adapter allows you to log messages to [ FirePHP](http://www.firephp.org/).
 *
 * This allows you to inspect native PHP values and objects inside the FireBug console.
 *
 * Because this adapter interacts directly with the `Response` object, some additional code is
 * required to use it. The simplest way to achieve this is to add a filter to the `Dispatcher`. For
 * example, the following can be placed in a bootstrap file:
 *
 * {{{
 * use lithium\action\Dispatcher;
 * use lithium\analysis\Logger;
 *
 * Logger::config(array(
 * 	'default' => array('adapter' => 'FirePhp')
 * ));
 *
 * Dispatcher::applyFilter('_call', function($self, $params, $chain) {
 * 	if (isset($params['callable']->response)) {
 * 		Logger::adapter('default')->bind($params['callable']->response);
 * 	}
 * 	return $chain->next($self, $params, $chain);
 * });
 * }}}
 *
 * This will cause the message and other debug settings added to the header of the
 * response, where FirePHP is able to locate and print it accordingly. As this adapter
 * implements the protocol specification directly, you don't need another vendor library to
 * use it.
 *
 * Now, in you can use the logger in your application code (like controllers, views and models).
 *
 * {{{
 * class PagesController extends \lithium\action\Controller {
 * 	public function view() {
 * 		//...
 * 		Logger::error("Something bad happened!");
 * 		//...
 * 	}
 * }
 * }}}
 *
 * Because this adapter also has a queue implemented, it is possible to log messages even when the
 * `Response` object is not yet generated. When it gets generated (and bound), all queued messages
 * get flushed instantly.
 *
 * Because FirePHP is not a conventional logging destination like a file or a database, you can
 * pass everything (except resources) to the logger and inspect it further in FirePHP. In fact,
 * every message that is passed will be encoded via `json_encode()`, so check out this built-in
 * method for more information on how your message will be encoded.
 *
 * {{{
 * Logger::debug(array('debug' => 'me'));
 * Logger::debug(new \lithium\action\Response());
 * }}}
 *
 * @see lithium\action\Response
 * @see lithium\net\http\Message::headers()
 * @link http://www.firephp.org/ FirePHP
 * @link http://www.firephp.org/Wiki/Reference/Protocol FirePHP Protocol Reference
 * @link http://php.net/manual/en/function.json-encode.php PHP Manual: `json_encode()`
 */
class FirePhp extends \lithium\core\Object {

	/**
	 * These headers are specified by FirePHP and get added as headers to the response.
	 *
	 * @var array
	 */
	protected $_headers = array(
		'X-Wf-Protocol-1' => 'http://meta.wildfirehq.org/Protocol/JsonStream/0.2',
		'X-Wf-1-Plugin-1' =>
			'http://meta.firephp.org/Wildfire/Plugin/FirePHP/Library-FirePHPCore/0.3',
		'X-Wf-1-Structure-1' =>
			'http://meta.firephp.org/Wildfire/Structure/FirePHP/FirebugConsole/0.1'
	);

	/**
	 * This is a mapping table that maps Lithium log levels to FirePHP log levels as they
	 * do not correlate directly and FirePHP only accepts a distinct set.
	 *
	 * @var array
	 */
	protected $_levels = array(
		'emergency' => 'ERROR',
		'alert'     => 'ERROR',
		'critical'  => 'ERROR',
		'error'     => 'ERROR',
		'warning'   => 'WARN',
		'notice'    => 'INFO',
		'info'      => 'INFO',
		'debug'     => 'LOG'
	);

	/**
	 * This self-incrementing counter allows the user to log more than one message per request.
	 *
	 * @var integer
	 */
	protected $_counter = 1;

	/**
	 * Holds the response object where the headers will be inserted.
	 */
	protected $_response = null;

	/**
	 * Contains messages that have been written to the log before the bind() call.
	 */
	protected $_queue = array();

	/**
	 * Binds the response object to the logger and sets the required Wildfire
	 * protocol headers.
	 *
	 * @param object $response An instance of a response object (usually `lithium\action\Response`)
	 *               with HTTP request information.
	 * @return void
	 */
	public function bind($response) {
		$this->_response = $response;
		$this->_response->headers += $this->_headers;

		foreach ($this->_queue as $message) {
			$this->_write($message);
		}
	}

	/**
	 * Appends a log message to the response header for FirePHP.
	 *
	 * @param string $priority Represents the message priority.
	 * @param string $message Contains the actual message to store.
	 * @return boolean Always returns `true`. Note that in order for message-writing to take effect,
	 *                 the adapter must be bound to the `Response` object instance associated with
	 *                 the current request. See the `bind()` method.
	 */
	public function write($priority, $message) {
		$_self =& $this;

		return function($self, $params) use (&$_self) {
			$priority = $params['priority'];
			$message = $params['message'];
			$message = $_self->invokeMethod('_format', array($priority, $message));
			$_self->invokeMethod('_write', array($message));
			return true;
		};
	}

	/**
	 * Heper method that writes the message to the header of a bound `Response` object. If no
	 * `Response` object is bound when this method is called, it is stored in a message queue.
	 *
	 * @see lithium\analysis\logger\adapter\FirePhp::_format()
	 * @param array $message A message containing the key and the content to store.
	 * @return void
	 */
	protected function _write($message) {
		if (!$this->_response) {
			return $this->_queue[] = $message;
		}
		$this->_response->headers[$message['key']] = $message['content'];
	}

	/**
	 * Generates a string representation of the type and message, suitable for FirePHP.
	 *
	 * @param string $type Represents the message priority.
	 * @param string $message Contains the actual message to store.
	 * @return array Returns the encoded string representations of the priority and message, in the
	 *               `'key'` and `'content'` keys, respectively.
	 */
	protected function _format($type, $message) {
		$key = 'X-Wf-1-1-1-' . $this->_counter++;

		$content = array(array('Type' => $this->_levels[$type]), $message);
		$content = json_encode($content);
		$content = strlen($content) . '|' . $content . '|';

		return compact('key', 'content');
	}
}

?>