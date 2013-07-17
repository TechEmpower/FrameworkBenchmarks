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
	 * Displays the error page. If you set $display_errors to false
	 * only a small error message will be displayed.
	 *
	 * @param \Exception $exception Exception to display
	 * @return void
	 */
	public function render_exception_page($exception)
	{
		if (ob_get_length() > 0)
			ob_end_clean();

		$status = '503 Service Temporarily Unavailable';
		
		if ($exception instanceof \PHPixie\Exception\PageNotFound)
			$status = '404 Not Found';
			
		header($_SERVER["SERVER_PROTOCOL"].' '.$status);
		header("Status: {$status}");

		if (!$this->display_errors) {
			echo $status;
		}else{
			$view = $this->pixie->view('debug');
			$view->exception = $exception;
			$view->log = $this->logged;
			echo $view->render();
		}
		
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
	public function error_handler($errno, $errstr, $errfile, $errline)
	{
		throw new \ErrorException($errstr, $errno, 0, $errfile, $errline);
	}

	/**
	 * Initializes the error handler
	 *
	 * @return void
	 */
	public function init()
	{
		set_error_handler(array($this, 'error_handler'), E_ALL);
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
