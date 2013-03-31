<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
require_once("verysimple/HTTP/RequestUtil.php");
require_once("verysimple/Util/ExceptionThrower.php");

/**
 * Dispatcher direct a web request to the correct controller & method
 *
 * @package    verysimple::Phreeze
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.4
 */
class Dispatcher
{
	/**
	 * Set to true and Phreeze will not try to handle deprecated function warnings
	 * @var boolean default = true
	 */
	static $IGNORE_DEPRECATED = true;

	/**
	 * Processes user input and executes the specified controller method, ensuring
	 * that the controller dependencies are all injected properly
	 *
	 * @param Phreezer $phreezer Object persistance engine
	 * @param IRenderEngine $renderEngine rendering engine
	 * @param string (optional) $action the user requested action (if not provided will use router->GetRoute())
	 * @param Context (optional) a context object for persisting state
	 * @param IRouter (optional) router object for reading/writing URLs (if not provided, GenericRouter will be used)
	 */
	static function Dispatch($phreezer,$renderEngine,$action='',$context=null,$router=null)
	{
		if ($router == null)
		{
			require_once('GenericRouter.php');
			$router = new GenericRouter();
		}

		list($controller_param,$method_param) = $router->GetRoute( $action );

		// normalize the input
		$controller_class = $controller_param."Controller";
		$controller_file = "Controller/" . $controller_param . "Controller.php";

		// look for the file in the expected places, hault if not found
		if ( !(file_exists($controller_file) || file_exists("libs/".$controller_file)) )
		{
			// go to plan be, search the include path for the controller
			$paths = explode(PATH_SEPARATOR,get_include_path());
			$found = false;
			foreach ($paths as $path)
			{
				if (file_exists($path ."/".$controller_file))
				{
					$found = true;
					break;
				}
			}

			if (!$found) throw new Exception("File ~/libs/".$controller_file." was not found in include path");
		}

		// convert any php errors into an exception
		if (self::$IGNORE_DEPRECATED)
		{
			ExceptionThrower::Start();
		}
		else
		{
			ExceptionThrower::Start(E_ALL);
			ExceptionThrower::$IGNORE_DEPRECATED = false;
		}

		// we should be fairly certain the file exists at this point
		include_once($controller_file);

		// we found the file but the expected class doesn't appear to be defined
		if (!class_exists($controller_class))
		{
			throw new Exception("Controller file was found, but class '".$controller_class."' is not defined");
		}


		// create an instance of the controller class
		$controller = new $controller_class($phreezer,$renderEngine,$context,$router);
		
		// we have a valid instance, just verify there is a matching method
		if (!is_callable(array($controller, $method_param)))
		{
			throw new Exception("'".$controller_class.".".$method_param."' is not a valid action");
		}

		// do not call the requested method/route if the controller request has been cancelled
		if (!$controller->IsTerminated())
		{
			// file, class and method all are ok, go ahead and call it
			call_user_func(array(&$controller, $method_param));
		}

		// reset error handling back to whatever it was
		//restore_exception_handler();
		ExceptionThrower::Stop();

		return true;
	}

	/**
	 * Fired by the PHP error handler function.  Calling this function will
	 * always throw an exception unless error_reporting == 0.  If the
	 * PHP command is called with @ preceeding it, then it will be ignored
	 * here as well.
	 *
	 * @deprecated use ExceptionThrower::HandleError instead
	 * @param string $code
	 * @param string $string
	 * @param string $file
	 * @param string $line
	 * @param string $context
	 */
	static function HandleException($code, $string, $file, $line, $context)
	{
		ExceptionThrower::HandleError($code, $string, $file, $line, $context);
	}
}

?>