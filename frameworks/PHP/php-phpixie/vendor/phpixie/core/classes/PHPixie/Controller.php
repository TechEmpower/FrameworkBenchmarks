<?php

namespace PHPixie;

/**
 * Base Controller class. Controllers contain the  logic of your website,
 * each action representing a reply to a particular request, e.g. a single page.
 * @package Core
 */
class Controller
{
	
	/**
	 * Pixie Dependancy Container
	 * @var \PHPixie\Pixie
	 */
	protected $pixie;
	
	/**
	 * Request for this controller. Holds all input data.
	 * @var \PHPixie\Request
	 */
	public $request;

	/**
	 * Response for this controller. It will be updated with headers and
	 * response body during controller execution
	 * @var \PHPixie\Response
	 */
	public $response;

	/**
	 * If set to False stops controller execution
	 * @var boolean
	 */
	public $execute = true;

	/**
	 * This method is called before the action.
	 * You can override it if you need to,
	 * it doesn't do anything by default.
	 *
	 * @return void
	 */
	public function before()
	{

	}

	/**
	 * This method is called after the action.
	 * You can override it if you need to,
	 * it doesn't do anything by default.
	 *
	 * @return void
	 */
	public function after()
	{

	}

	/**
	 * Creates new Controller
	 *
	 */
	public function __construct($pixie)
	{
		$this->pixie = $pixie;
		$this->response = new Response;
	}

	/**
	 * Shortcut for redirecting the user.
	 * Use like this:
	 * <code>
	 *     return $this->redirect($url);
	 * </code>
	 *
	 * @param string $url URL to redirect to.
	 * @return void
	 */
	public function redirect($url) {
		$this->response-> redirect($url);
		$this->execute = false;
	}
	
	/**
	 * Runs the appropriate action.
	 * It will execute the before() method before the action
	 * and after() method after the action finishes.
	 *
	 * @param string    $action Name of the action to execute.
	 * @return void
	 * @throws \PHPixie\Exception\PageNotFound If the specified action doesn't exist
	 */
	public function run($action)
	{
		$action = 'action_'.$action;
		
		if (!method_exists($this, $action))
			throw new \PHPixie\Exception\PageNotFound("Method {$action} doesn't exist in ".get_class($this));
			
		$this->execute = true;
		$this->before();
		if ($this->execute)
			$this->$action();
		if ($this->execute)
			$this->after();
	}

}
