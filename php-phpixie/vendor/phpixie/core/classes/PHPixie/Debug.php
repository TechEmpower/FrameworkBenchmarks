<?php

namespace PHPixie;

/**
 * Handles error reporting and debugging.
 * @package Core
 */
class Debug
{

	/**
	 * Pixie Dependancy Container
	 * @var \PHPixie\Pixie
	 */
	protected $pixie;
	
	/**
	 * An array of logged items
	 * @var array
	 */
	public $logged = array();
	
	/**
	 * Flag that determines if the errors are displayed
	 * @var boolean
	 */
	public $display_errors = true;
	
	/**
	 * Constructs a debugger
	 *
	 * @param \PHPixie\Pixie $pixie Pixie dependency container
	 */
	public function __construct($pixie) {
		$this->pixie = $pixie;
	}
	
	/**
	 * Displays the error page. If you have 'silent_errors' enabled in
	 * core.php config file, a small message will be shown instead.
	 *
	 * @param \Exception $exception Exception to display
	 * @return void
	 */
	public function render_error($exception)
	{
		if (ob_get_length() > 0)
		{
			ob_end_clean();
		}

		if ($exception->getCode() == 404)
		{
			$status = '404 Not Found';
		}
		else
		{
			$status = '503 Service Temporarily Unavailable';
		}

		header($_SERVER["SERVER_PROTOCOL"].' '.$status);
		header("Status: {$status}");

		if (!$this->display_errors)
		{
			echo $status;
			return;
		}

		$view = $this->pixie->view('debug');
		$view->exception = $exception;
		$view->log = $this->logged;
		echo $view->render();
	}

	/**
	 * Catches errors and exceptions and sends them
	 * to the configured handler if one is present,
	 * otherwise render_error() will be called.
	 *
	 * @param \Exception $exception Caught exception
	 * @return void
	 */
	public function onError($exception)
	{
		set_exception_handler(array($this, 'internalException'));
		set_error_handler(array($this, 'internalError'), E_ALL);
		$this->render_error($exception);
	}

	/**
	 * Converts PHP Errors to Exceptions
	 *
	 * @param string        $errno   Error number
	 * @param string        $errstr  Error message
	 * @param string        $errfile File in which the error occurred
	 * @param string        $errline Line at which the error occurred
	 * @return void
	 * @throws \ErrorException Throws converted exception to be immediately caught
	 */
	public function errorHandler($errno, $errstr, $errfile, $errline)
	{
		throw new \ErrorException($errstr, $errno, 0, $errfile, $errline);
	}

	/**
	 * Handles exceptions that occurred while inside the error handler. Prevents recursion.
	 *
	 * @param \Exception  $exception Caught exception
	 * @return void
	 */
	public function internalException($exception)
	{
		echo $exception->getMessage().' in '.$exception->getFile().' on line '.$exception->getLine();
	}

	/**
	 * Handles errors that occurred while inside the error handler. Prevents recursion.
	 *
	 * @param string        $errno   Error number
	 * @param string        $errstr  Error message
	 * @param string        $errfile File in which the error occurred
	 * @param string        $errline Line at which the error occurred
	 * @return void
	 */
	public function internalError($errno, $errstr, $errfile, $errline)
	{
		echo $errstr.' in '.$errfile.' on line '.$errline;
	}

	/**
	 * Initializes the error handler
	 *
	 * @return void
	 */
	public function init()
	{
		set_exception_handler(array($this, 'onError'));
		set_error_handler(array($this, 'errorHandler'), E_ALL);
	}

	/**
	 * Adds an item to the log.
	 *
	 * @param mixed $val Item to be logged
	 * @return void
	 */
	public function log($val)
	{
		array_unshift($this->logged, $val);
	}

}
