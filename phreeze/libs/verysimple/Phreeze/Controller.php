<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
require_once("verysimple/HTTP/RequestUtil.php");
require_once("verysimple/HTTP/Context.php");
require_once("Phreezer.php");
require_once("Criteria.php");
require_once("IRouter.php");
require_once("GenericRouter.php");
require_once("verysimple/Authentication/IAuthenticatable.php");

/**
 * Controller is a base controller object used for an MVC pattern
 * This controller uses Phreeze ORM and RenderEngine Template Engine
 * This controller could be extended to use a differente ORM and
 * Rendering engine as long as they implement compatible functions.
 *
 * @package    verysimple::Phreeze
 * @author     VerySimple Inc.
 * @copyright  1997-2011 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    3.1
 */
abstract class Controller
{
	protected $Phreezer;
	protected $RenderEngine;

	/**
	 * @var string ModelName is used by the base Controller class for certain functions in which
	 * require knowledge of what Model is being used.  For example, when validating user input.
	 * This may be defined in Init() if any of thes base Controller features will be used.
	 */
	protected $ModelName;

	protected $Context;

	/** @deprecated use RenderEngine */
	protected $Smarty;

	private $_router;
	private $_cu;
	public $GUID;
	public $DebugOutput = "";
	public $UnitTestMode = false;
	public $CaptureOutputMode = false;
	
	private $_terminate = false;

	/** Prefix this to the name of the view templates */
	static $SmartyViewPrefix = "View";
	
	/** search string to look for to determine if this is an API request or not */
	static $ApiIdentifier = "api/";

	/** the default mode used when calling 'Redirect' */
	static $DefaultRedirectMode = "client";

	/**
	 * Constructor initializes the controller.  This method cannot be overriden.  If you need
	 * to do something during construction, add it to Init
	 *
	 * @param Phreezer $phreezer Object persistance engine
	 * @param IRenderEngine $renderEngine rendering engine
	 * @param Context (optional) a context object for persisting the state of the current page
	 * @param Router (optional) a custom writer for URL formatting
	 */
	final function __construct(Phreezer $phreezer, $renderEngine, $context = null, IRouter $router = null)
	{
		$this->Phreezer =& $phreezer;
		$this->RenderEngine =& $renderEngine;

		// for backwards compatibility
		$this->Smarty =& $renderEngine;

		$ra = RequestUtil::GetRemoteHost();
		$this->GUID = $this->Phreezer->DataAdapter->GetDBName() . "_" . str_replace(".","_", $ra);

		$this->_router = $router ? $router : new GenericRouter();

		if ($context)
		{
			$this->Context =& $context;
		}
		else
		{
			$this->Context = new Context();
			$this->Context->GUID = "CTX_" . $this->GUID;
		}

		if ($this->RenderEngine)
		{
			// assign some variables globally for the views
			$this->Assign("CURRENT_USER",$this->GetCurrentUser());
			$this->Assign("URL",$this->GetRouter());
			$this->Assign("BROWSER_DEVICE",$this->GetDevice());
	
			// if feedback was persisted, set it
			$feedback = $this->Context->Get("feedback");
			
			// print_r($feedback); die('feedback');
			
			if (is_array($feedback)) {
				foreach ($feedback as $key => $val) {
					$this->Assign($key,$val);
				}
			}
			else {
				$this->Assign("feedback",$feedback);
			}
			$this->Context->Set("feedback",null);
		}

		$this->Init();
	}
	
	/**
	 * Calling Terminate in Init will tell the dispatcher to halt execution
	 * without calling the requested method/route
	 */
	protected function Terminate()
	{
		$this->_terminate = true;
	}
	
	/**
	 * Returns true if Terminate() has been fired
	 * @return bool
	 */
	public function IsTerminated()
	{
		return $this->_terminate;
	}

	/**
	 * Returns the router object used to convert url/uri to controller method
	 * @return IRouter
	 */
	protected function GetRouter()
	{
		return $this->_router;
	}

	/**
	 * Init is called by the base constructor immediately after construction.
	 * This method must be implemented and provided an oportunity to
	 * set any class-wide variables such as ModelName, implement
	 * authentication for this Controller or any other class-wide initialization
	 */
	abstract protected function Init();

	/**
	 * Requires 401 Authentication.  If authentication fails, this function
	 * terminates with a 401 header.  If success, sets CurrentUser and returns null.
	 * @param IAuthenticatable any IAuthenticatable object
	 * @param string http realm (basically the login message shown to the user)
	 * @param string username querystring field (optional) if provided, the username can be passed via querystring instead of through the auth headers
	 * @param string password querystring field (optional) if provided, the password can be passed via querystring instead of through the auth headers
	 */
	protected function Require401Authentication(IAuthenticatable $authenticatable, $realm = "Login Required", $qs_username_field = "", $qs_password_field = "")
	{
		require_once("verysimple/Authentication/Auth401.php");
		
		$user = $this->Get401Authentication($authenticatable,$qs_username_field, $qs_password_field);

		// we only want to output 401 headers if the user is not already authenticated
		if (!$user)
		{
			if( $this->Get401AuthUsername($qs_username_field) )
			{
				// a username was provided, which means login failed
				Auth401::OutputHeaders("Invalid Login");
			}
			else
			{
				// no username provided, which means prompt for username
				Auth401::OutputHeaders($realm);
			}
		}
	}
	
	/**
	 * Assign the current CSRFToken to the view layer
	 * @param string $varname the view varname to use for assignment
	 */
	protected function AssignCSRFToken($varname = 'CSRFToken')
	{
		$this->Assign($varname, $this->GetCSRFToken());
	}
	
	/**
	 * Returns a stored CSRF Token from the session.  If no token exists, then generate
	 * one and save it to the session.
	 *
	 * @return string
	 */
	protected function GetCSRFToken()
	{
		$token = $this->Context->Get('X-CSRFToken');
	
		if (!$token)
		{
			$token = md5(rand(1111111111,9999999999).microtime());
			$this->Context->Set('X-CSRFToken',$token);
		}
	
		return $token;
	}
	
	/**
	 * Verify that X-CSRFToken was sent in the request headers and matches the session token
	 * If not an Exception with be thrown.  If no exception is thrown then the token
	 * is verified.
	 * @param string the name of the header variable that contains the token
	 * @throws Exception if token is not provided or does not match
	 */
	protected function VerifyCSRFToken($headerName = 'X-CSRFToken')
	{
		// check that a CSRF token is present in the request
		$headers = RequestUtil::GetHeaders();
		
		// make this case-insensitive (IE changes all headers to lower-case)
		$headers = array_change_key_case($headers, CASE_LOWER);
		$headerName = strtolower($headerName);
	
		if (array_key_exists($headerName, $headers))
		{
			if ($this->GetCSRFToken() != $headers[$headerName])
			{
				throw new Exception('Invalid CSRFToken');
			}
		}
		else
		{
			throw new Exception('Missing CSRFToken');
		}
	}
	
	/**
	 * Start observing messages from Phreeze.  If no observer is provided, then
	 * an ObserveToBrowser will be used and debug messages will be output to the browser
	 * @param IObserver $observer
	 * @param bool $with_styles if true then basic styles will be output to the browser
	 */
	protected function StartObserving($observer = null, $with_styles = true)
	{
		if ($observer == null)
		{
			require_once "ObserveToBrowser.php";
			$observer = new ObserveToBrowser();
		}
		
		if ($with_styles)
		{
			$this->PrintOut("<style>.debug, .query, .info {font-family: courier new; border-bottom: solid 1px #999;} .debug {color: blue;} .query {color: green;}</style>");
		}
		
		$this->Phreezer->AttachObserver($observer);
	}

	/**
	 * accept username passed in either headers or querystring.  if a querystring parameter name is
	 * provided, that will be checked first before the 401 auth headers
	 * @param string $qs_username_field the querystring parameter to check for username (optional)
	 */
	protected function Get401AuthUsername($qs_username_field = "")
	{
		$qsv = $qs_username_field ? RequestUtil::Get($qs_username_field) : '';
		return $qsv ? $qsv : Auth401::GetUsername();
	}

	/**
	* accept password passed in either headers or querystring.  if a querystring parameter name is
	* provided, that will be checked first before the 401 auth headers
	* @param string $qs_password_field the querystring parameter to check for password (optional)
	*/
	protected function Get401AuthPassword($qs_password_field = "")
	{
		$qsv = $qs_password_field ? RequestUtil::Get($qs_password_field) : '';
		return $qsv ? $qsv : Auth401::GetPassword();
	}

	/**
	* Gets the user from 401 auth headers (or optionally querystring).  There are three scenarios
	*   - The user is already logged authenticated = IAuthenticatable is returned
	*   - The user was not logged in and valid login credentials were provided = SetCurrentUser is called and IAuthenticatable is returned
	*   - The user was not logged in and invalid (or no) credentials were provided = NULL is returned
	* @param IAuthenticatable any IAuthenticatable object
	* @param string username querystring field (optional) if provided, the username can be passed via querystring instead of through the auth headers
	* @param string password querystring field (optional) if provided, the password can be passed via querystring instead of through the auth headers
	* @return IAuthenticatable or NULL
	*/
	protected function Get401Authentication(IAuthenticatable $authenticatable, $qs_username_field = "", $qs_password_field = "")
	{
		$user = null;
		$username = $this->Get401AuthUsername($qs_username_field);

		if( $username )
		{
			// username was provided so let's attempt a login
			$password = $this->Get401AuthPassword($qs_password_field);

			if ( $authenticatable->Login($username,$password) )
			{
				$user = $authenticatable;
				$this->SetCurrentUser($authenticatable);
			}
		}
		else
		{
			// no login info was provided so return whatever is in the session
			// (which will be null if the user is not authenticated)
			$user = $this->GetCurrentUser();
		}

		return $user;

	}

	/**
	 * LoadFromForm should load the object specified by primary key = $pk, or
	 * create a new instance of the object.  Then should overwrite any applicable
	 * properties with user input.
	 *
	 * This method is used by ValidateInput for automation AJAX server-side validation.
	 *
	 * @param variant $pk the primary key (optional)
	 * @return Phreezable a phreezable object
	 */
	protected function LoadFromForm($pk = null)
	{
		return null;
	}

	/**
	 * Use as an alterative to print in order to capture debug output
	 * @param string text to print
	 * @param mime content type (example text/plain)
	 */
	protected function PrintOut($text,$contentType = null)
	{
		if ($this->CaptureOutputMode)
		{
			$this->DebugOutput .= $text;
		}
		else
		{
			if ($contentType) header("Content-type: " . $contentType);
			print $text;
		}
	}

	/**
	 * Returns a BrowserDevice object with information about the browser
	 * that is being used to view/execute this code.
	 * @return BrowserDevice
	 */
	public function GetDevice()
	{
		require_once("verysimple/HTTP/BrowserDevice.php");
		return BrowserDevice::GetInstance();
	}

	/**
	 * Displays the ListAll view for the primary model object.  Because the
	 * datagrid is populated via ajax, no model data is populated here
	 */
	public function ListAll()
	{
		if (!$this->ModelName)
		{
			throw new Exception("ModelName must be defined in " . get_class($this) . "::ListAll");
		}

		// capture output instead of rendering if specified
		if ($this->CaptureOutputMode)
		{
			$this->DebugOutput = $this->RenderEngine->fetch("View" . $this->ModelName .  "ListAll.tpl");
		}
		else
		{
			$this->RenderEngine->display("View" . $this->ModelName .  "ListAll.tpl");
		}
		//$this->_ListAll(null, Request::Get("page",1), Request::Get("limit",20));
	}

	/**
	 * Displays the ListAll view for the primary model object in the event that
	 * ajax will not be used.  The model data is populated
	 *
	 * @param Criteria $criteria
	 * @param int $current_page number of the current page (for pagination)
	 * @param int $limit size of the page (for pagination)
	 */
	protected function _ListAll(Criteria $criteria, $current_page, $limit)
	{
		if (!$this->ModelName)
		{
			throw new Exception("ModelName must be defined in " . get_class($this) . "::_ListAll.");
		}

		$page = $this->Phreezer->Query($this->ModelName,$criteria)->GetDataPage($current_page,$limit);
		$this->RenderEngine->assign($this->ModelName . "DataPage", $page);
		$this->RenderEngine->display("View" . $this->ModelName .  "ListAll.tpl");
	}

	/**
	 * Renders a datapage as XML for use with a datagrid.  The optional $additionalProps allows
	 * retrieval of properties from foreign relationships
	 *
	 * @param DataPage $page
	 * @param Array $additionalProps (In the format Array("GetObjName1"=>"PropName","GetObjName2"=>"PropName1,PropName2")
	 * @param Array $supressProps (In the format Array("PropName1","PropName2")
	 * @param bool noMap set to true to render this DataPage regardless of whether there is a FieldMap
	 */
	protected function RenderXML($page,$additionalProps = null, $supressProps = null, $noMap = false)
	{
		require_once("verysimple/String/VerySimpleStringUtil.php");
		
		if (!is_array($supressProps)) $supressProps = array();

		// never include these props
		$suppressProps[] = "NoCache";
		$suppressProps[] = "CacheLevel";
		$suppressProps[] = "IsLoaded";
		$suppressProps[] = "IsPartiallyLoaded";


		$xml = "";
		$xml .= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n";

		$xml .= "<DataPage>\r\n";
		$xml .= "<ObjectName>".htmlspecialchars($page->ObjectName)."</ObjectName>\r\n";
		$xml .= "<ObjectKey>".htmlspecialchars($page->ObjectKey)."</ObjectKey>\r\n";
		$xml .= "<TotalRecords>".htmlspecialchars($page->TotalResults)."</TotalRecords>\r\n";
		$xml .= "<TotalPages>".htmlspecialchars($page->TotalPages)."</TotalPages>\r\n";
		$xml .= "<CurrentPage>".htmlspecialchars($page->CurrentPage)."</CurrentPage>\r\n";
		$xml .= "<PageSize>".htmlspecialchars($page->PageSize)."</PageSize>\r\n";

		$xml .= "<Records>\r\n";

		// get the fieldmap for this object type unless not specified
		if ($noMap)
		{
			$fms = array();
		}
		else
		{
			try
			{
				$fms = $this->Phreezer->GetFieldMaps($page->ObjectName);
			}
			catch (exception $ex)
			{
				throw new Exception("The objects contained in this DataPage do not have a FieldMap.  Set noMap argument to true to supress this error: " . $ex->getMessage());
			}
		}

		foreach ($page->Rows as $obj)
		{
			$xml .= "<" . htmlspecialchars($page->ObjectName) . ">\r\n";
			foreach (get_object_vars($obj) as $var => $val)
			{
				if (!in_array($var,$supressProps))
				{
					// depending on what type of field this is, do some special formatting
					$fm = isset($fms[$var]) ? $fms[$var]->FieldType : FM_TYPE_UNKNOWN;

					if ($fm == FM_TYPE_DATETIME)
					{
						$val = strtotime($val) ? date("m/d/Y h:i A",strtotime($val)) : $val;
					}
					elseif ($fm == FM_TYPE_DATE)
					{
						$val = strtotime($val) ? date("m/d/Y",strtotime($val)) : $val;
					}

					// if the developer has added a property that is not a simple type
					// we need to serialize it
					if (is_array($val) || is_object($val))
					{
						$val = serialize($val);
					}

					$val = VerySimpleStringUtil::EncodeSpecialCharacters($val, true, true);

					$xml .= "<" . htmlspecialchars($var) . ">" . $val . "</" . htmlspecialchars($var) . ">\r\n";
				}
			}


			// Add any properties that we want from child objects
			if ($additionalProps)
			{
				foreach ($additionalProps as $meth => $propPair)
				{
					$props = explode(",",$propPair);
					foreach ($props as $prop)
					{
						$xml .= "<" . htmlspecialchars($meth . $prop) . ">" . htmlspecialchars($obj->$meth()->$prop) . "</" . htmlspecialchars($meth . $prop) . ">\r\n";
					}
				}
		}

			$xml .= "</" . htmlspecialchars($page->ObjectName) . ">\r\n";
		}
		$xml .= "</Records>\r\n";

		$xml .= "</DataPage>\r\n";

		// capture output instead of rendering if specified
		if ($this->CaptureOutputMode)
		{
			$this->DebugOutput = $xml;
		}
		else
		{
			header('Content-type: text/xml');
			print $xml;
		}

	}

	/**
	 * Render an array of IRSSFeedItem objects as an RSS feed
	 * @param array $feedItems array of IRSSFeedItem objects
	 * @param string $feedTitle
	 * @param string $feedDescription
	 */
	protected function RenderRSS(Array $feedItems, $feedTitle = "RSS Feed", $feedDescription = "RSS Feed")
	{
		require_once('verysimple/RSS/Writer.php');
		require_once('verysimple/RSS/IRSSFeedItem.php');

		$baseUrl = RequestUtil::GetBaseURL();
		$rssWriter = new RSS_Writer($feedTitle,$baseUrl,$feedDescription);
		$rssWriter->setLanguage('us-en');
		$rssWriter->addCategory("Items");

		if (count($feedItems))
		{
			$count = 0;
			foreach ($feedItems as $item)
			{
				$count++;

				if ($item instanceof IRSSFeedItem)
				{
					$rssWriter->addItem(
						$item->GetRSSTitle(), 						// title
						$item->GetRSSLink($baseUrl), 				// link
						$item->GetRSSDescription(), 				// description
						$item->GetRSSAuthor(), 						// author
						date(DATE_RSS, $item->GetRSSPublishDate()), // date
						null, 										// source
						$item->GetRSSGUID() 						// guid
					);
				}
				else
				{
					$rssWriter->addItem("Item $count doesn't implment IRSSFeedItem","about:blank",'','Error',date(DATE_RSS) );
				}
			}
		}
		else
		{
			$rssWriter->addItem("No Items","about:blank",'','No Author',date(DATE_RSS) );
		}

		$rssWriter->writeOut();
	}

	/**
	 * @deprecated use Controller->Context->Set instead
	 */
	protected function Set($var,$val)
	{
		return $this->Context->Set($var,$val);
	}

	/**
	 * @deprecated use Controller->Context->Get instead
	 */
	protected function Get($var,$default=null)
	{
		return $this->Context->Get($var,$default);
	}

	/**
	 * This method calls LoadFromForm to retrieve a model object populated with user
	 * input.  The input is validated and a ValidationResponse is rendered in JSON format
	 *
	 * if Request::Get("SaveInline") is set then validate will call Save instead of
	 * rendering JSON.  In which case, your Save method should render the ValidationResponse
	 */
	function ValidateInput()
	{
		require_once("ValidationResponse.php");
		$vr = new ValidationResponse();

		$save = RequestUtil::Get("SaveInline");

		$obj = $this->LoadFromForm();

		if (!is_object($obj))
		{
			$vr->Success = false;
			$vr->Errors = array("Unknown"=>"LoadFromForm does not appear to be implemented.  Unable to validate");
			$vr->Message = "LoadFromForm does not appear to be implemented.  Unable to validate";
		}
		elseif ($obj->Validate())
		{
			$vr->Success = true;
		}
		else
		{
			$vr->Success = false;
			$vr->Errors = $obj->GetValidationErrors();
			$vr->Message = "Validation Errors Occured";
		}

		// if the user requested to save inline, their Save method will take over from here
		if ($vr->Success && $save)
		{
			$this->Save();
		}
		else
		{
			$this->RenderJSON($vr);
		}
	}


	/**
	 * Stub method
	 */
	function Save()
	{
		if ( !RequestUtil::Get("SaveInline") )
		{
			throw new Exception("Save is not implemented by this controller");
		}

		require_once("ValidationResponse.php");
		$vr = new ValidationResponse();
		$vr->Success = false;
		$vr->Errors = array();
		$vr->Message = "SaveInline is not implemented by this controller";
		$this->RenderJSON($vr);
	}

	/**
	 * Returns an array of all property names in the primary model
	 *
	 * @return array
	 */
	protected function GetColumns()
	{
		if (!$this->ModelName)
		{
			throw new Exception("ModelName must be defined in " . get_class($this) . "::GetColumns");
		}

		$counter = 0;
		$props = array();
		foreach (get_class_vars($this->ModelName)as $var => $val)
		{
			$props[$counter++] = $var;
		}
		return $props;
	}

	/**
	 * Returns a unique ID for this session based on connection string and remote IP
	 * This is a reasonable variable to use as a session variable because it ensures
	 * that if other applications on the same server are running phreeze, there won't
	 * be cross-application authentication issues.  Additionally, the remote ip
	 * helps to make session hijacking more difficult
	 *
	 * @deprecated use $controller->GUID instead
	 * @return string
	 */
	private function GetGUID()
	{
		return $this->GUID;
	}

	/**
	 * Clears the current authenticated user from the session
	 */
	public function ClearCurrentUser()
	{
		require_once("verysimple/Authentication/Authenticator.php");
		
		$this->_cu = null;
		Authenticator::ClearAuthentication($this->GUID);
	}

	/**
	 * Sets the given user as the authenticatable user for this session.
	 *
	 * @param IAuthenticatable The user object that has authenticated
	 */
	protected function SetCurrentUser(IAuthenticatable $user)
	{
		$this->_cu = $user;
		Authenticator::SetCurrentUser($user,$this->GUID);

		// assign some global variables to the view
		$this->Assign("CURRENT_USER",$this->GetCurrentUser());
	}

	/**
	 * Returns the currently authenticated user, or null if a user has not been authenticated.
	 *
	 * @return IAuthenticatable || null
	 */
	protected function GetCurrentUser()
	{
		if (!$this->_cu)
		{
			require_once("verysimple/Authentication/Authenticator.php");
			
			$this->Phreezer->Observe("Loading CurrentUser from Session");
			$this->_cu = Authenticator::GetCurrentUser($this->GUID);

			if ($this->_cu )
			{
				if (get_class($this->_cu) == "__PHP_Incomplete_Class")
				{
					// this happens if the class used for authentication was not included before the session was started
					$tmp = print_r($this->_cu,1);
					$parts1 = explode("__PHP_Incomplete_Class_Name] => ",$tmp);
					$parts2 = explode("[",$parts1[1]);
					$name = trim($parts2[0]);

					Authenticator::ClearAuthentication($this->GUID);
					throw new Exception("The class definition used for authentication '$name' must be defined (included) before the session is started, for example in _app_config.php.");
				}
				else
				{
					// refresh the current user if the object supports it
					if (method_exists($this->_cu, 'Refresh')) $this->_cu->Refresh($this->Phreezer);
				}
			}
		}
		else
		{
			$this->Phreezer->Observe("Using previously loaded CurrentUser");
		}

		return $this->_cu;
	}
	
	/**
	 * Returns true if this request is an API request.  This examines the URL to 
	 * see if the string Controller::$ApiIdentifier is in the URL
	 * @return bool
	 */
	public function IsApiRequest()
	{
		$url = RequestUtil::GetCurrentURL();
		return (strpos($url, self::$ApiIdentifier ) !== false);
	}

	/**
	 * Check the current user to see if they have the requested permission.
	 * If so then the function does nothing.  If not, then the user is redirected
	 * to $on_fail_action (if provided) or an AuthenticationException is thrown.
	 * if Controller->IsApiRequest() returns true then an AuthenticationException will
	 * be thrown regardless of the fail_action.
	 *
	 * @param int $permission Permission ID requested
	 * @param string $on_fail_action (optional) The action to redirect if require fails
	 * @param string $not_authenticated_feedback (optional) Feedback to forward to the on_fail_action if user is not logged in
	 * @param string $permission_denied_feedback (optional) Feedback to forward to the on_fail_action if user is logged in but does not have permission
	 * @throws AuthenticationException
	 */
	protected function RequirePermission($permission, $on_fail_action = "", $not_authenticated_feedback = "Please login to access this page", $permission_denied_feedback = "You are not authorized to view this page and/or your session has expired")
	{
		$this->Phreezer->Observe("Checking For Permission '$permission'");
		$cu = $this->GetCurrentUser();

		if (!$cu || !$cu->IsAuthorized($permission))
		{
			$message = !$cu || $cu->IsAnonymous()
				? $not_authenticated_feedback
				: $permission_denied_feedback;
			
			if ($on_fail_action && $this->IsApiRequest() == false)
			{
				$this->Redirect($on_fail_action,array('feedback'=>$message,'warning'=>$message));
			}
			else
			{
				$ex = new AuthenticationException($message,500);
				$this->Crash("Permission Denied",500,$ex);
			}
		}
	}

	/**
	 * Assigns a variable to the view
	 *
	 * @param string $varname
	 * @param variant $varval
	 */
	protected function Assign($varname,$varval)
	{
		$this->RenderEngine->assign($varname,$varval);
	}

	/**
	 * Renders the specified view
	 *
	 * @param string $view (optional) if not provided, the view is automatically bound using the class and method name
	 * @param string $format (optional) defaults to $self::SmartyViewPrefix
	 */
	protected function Render($view="",$format = null)
	{
		$isSmarty = (strpos(get_class($this->RenderEngine),"Smarty") > -1);

		if ($isSmarty && $format == null) $format = self::$SmartyViewPrefix;

		if ($format == null) $format = '';

		if ($view == "")
		{
			// automatic binding
			$backtrace = debug_backtrace();
			$view = str_replace("Controller","", $backtrace[1]['class']) . $backtrace[1]['function'];
		}

		// if the render engine is Smarty then add the '.tpl' suffix
		$viewPath = $isSmarty ? $format.$view.".tpl" : $format.$view;

		// capture output instead of rendering if specified
		if ($this->CaptureOutputMode)
		{
			$this->DebugOutput = $this->RenderEngine->fetch($viewPath);
		}
		else
		{
			$this->RenderEngine->display($viewPath);
		}
	}

	/**
	 * Renders the given value as JSON
	 *
	 * @param variant the variable, array, object, etc to be rendered as JSON
	 * @param string if a callback is provided, this will be rendered as JSONP
	 * @param bool if true then objects will be returned ->GetObject() (only supports ObjectArray or individual Phreezable or Reporter object)
	 * @param array  (only relvant if useSimpleObject is true) options array passed through to Phreezable->ToString()
	 * @param bool set to 0 to leave data untouched.  set to 1 to always force value to UTF8. set to 2 to only force UTF8 if an encoding error occurs (WARNING: options 1 or 2 will likely result in unreadable characters.  The recommended fix is to set your database charset to utf8)
	 */
	protected function RenderJSON($var, $callback = "",$useSimpleObject = false, $options = null, $forceUTF8 = 0)
	{
		$obj = null;

		if (is_a($var,'DataSet') || is_a($var,'DataPage'))
		{
			// if a dataset or datapage can be converted directly into an array without enumerating twice
			$obj = $var->ToObjectArray($useSimpleObject,$options);
		}
		else if ($useSimpleObject)
		{
			// we need to figure out what type
			if (is_array($var) || is_a($var,'SplFixedArray')  )
			{
				$obj = array();
				foreach ($var as $item)
				{
					$obj[] = $item->ToObject($options);
				}
			}
			elseif (is_a($var,'Phreezable') || is_a($var,'Reporter'))
			{
				$obj = $var->ToObject($options);
			}
			else
			{
				throw new Exception('RenderJSON could not determine the type of object to render');
			}
		}
		else
		{
			$obj = $var;
		}

		if ($forceUTF8 == 1) $this->UTF8Encode($obj);

		try
		{
			$output = json_encode($obj);
		}
		catch (Exception $ex)
		{
			if (strpos($ex->getMessage(),'Invalid UTF-8') !== false)
			{
				// a UTF encoding problem has been encountered
				if ($forceUTF8 == 2) 
				{
					$this->UTF8Encode($obj);
					$output = json_encode($obj);
				}
				else
				{
					throw new Exception('The object to be encoded contains invalid UTF-8 data.  Please verify your database character encoding or alternatively set the Controller::RenderJSON $forceUTF8 parameter to 1 or 2.');
				}
			}
			else
			{
				// we don't know what this is so don't try to handle it here
				throw $ex;
			}
		}

		if ($callback) $output = "$callback(" . $output . ")";

		// capture output instead of rendering if specified
		if ($this->CaptureOutputMode)
		{
			$this->DebugOutput = $output;
		}
		else
		{
			@header($callback ? 'Content-type: text/plain' : 'Content-type: application/json');
			print $output;
		}
	}


	/**
	 * Send a crash message to the browser and terminate
	 *
	 * @param string $errmsg text message to display
	 * @param int $code used for support for this error
	 * @param Exception $exception if exception was thrown, can be provided for more info
	 */
	protected function Crash($errmsg = "Unknown Error", $code = 0, $exception = null)
	{
		$ex = $exception ? $exception : new Exception($errmsg, $code);
		throw $ex;
	}

	/**
	 * Redirect to the appropriate page based on the action.  This function will
	 * call "exit" so do not put any code that you wish to execute after Redirect
	 *
	 * @param string $action in the format Controller.Method
	 * @param mixed $feedback string which will be assigne to the template as "feedback" or an array of values to assign
	 * @param array $params
	 * @param string $mode (client | header) default = Controller::$DefaultRedirectMode
	 */
	protected function Redirect($action, $feedback =  null, $params = "", $mode = "")
	{
		if (!$mode) $mode = self::$DefaultRedirectMode;

		$params = is_array($params) ? $params : array();

		if ($feedback != null)
		{
			$this->Context->Set("feedback",$feedback);
		}

		// support for deprecated Controller/Method format
		list($controller,$method) = explode(".", str_replace("/",".",$action));

		$url = $this->GetRouter()->GetUrl($controller,$method,$params);

		// capture output instead of rendering if specified
		if ($this->CaptureOutputMode)
		{
			if ($mode == 'client')
			{
				$this->RenderEngine->assign("url",$url);
				$this->DebugOutput = $this->RenderEngine->fetch("_redirect.tpl");
			}
			else
			{
				$this->DebugOutput = 'Location: ' . $url;
			}
		}
		else
		{

			if ($mode == 'client')
			{
				$this->RenderEngine->assign("url",$url);
				$this->RenderEngine->display("_redirect.tpl");
			}
			else
			{
				header('Location: ' . $url) ;
			}

		}

		// don't exit if we are unit testing because it will stop all further tests
		if (!$this->UnitTestMode) exit;

	}

	/**
	 * Does a recursive UTF8 encoding on a string/array/object.  This is used
	 * when json encoding fails due to non UTF8 in the database that cannot
	 * be repaired with any other means.
	 * 
	 * NOTE: this does not have a return value.  value is passed by reference and updated
	 * 
	 * @param variant $input
	 */
	private function UTF8Encode(&$input) 
	{
		if (is_string($input)) 
		{
			// pop recursion here
			$input = utf8_encode($input);
		} 
		else if (is_array($input)) 
		{
			foreach ($input as &$value) 
			{
				$this->UTF8Encode($value);
			}
			unset($value);
		} 
		else if (is_object($input)) 
		{
			$vars = array_keys(get_object_vars($input));
			foreach ($vars as $var) 
			{
				$this->UTF8Encode($input->$var);
			}
		}
	}

    /**
    * Throw an exception if an undeclared method is accessed
    *
	* @access     public
	* @param      string $name
	* @param      variant $vars
	* @throws     Exception
	*/
	function __call($name,$vars = null)
	{
		throw new Exception(get_class($this) . "::" . $name . " is not implemented");
	}
}

?>