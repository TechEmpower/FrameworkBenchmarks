<?php
/** @package	verysimple::HTTP */

/** import supporting libraries */
require_once("FileUpload.php");
require_once("verysimple/String/VerySimpleStringUtil.php");

/**
 * Static utility class for processing form post/request data
 *
 * Contains various methods for retrieving user input from forms
 *
 * @package	verysimple::HTTP
 * @author	 VerySimple Inc.
 * @copyright  1997-2011 VerySimple, Inc. http://www.verysimple.com
 * @license	http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version	1.3
 */
class RequestUtil
{


	/** @var bool set to true and all non-ascii characters in request variables will be html encoded */
	static $ENCODE_NON_ASCII = false;

	/** @var bool set to false to skip is_uploaded_file.  This allows for simulated file uploads during unit testing */
	static $VALIDATE_FILE_UPLOAD = true;

	/** @var body contents, only read once in case GetBody is called multiple times */
	private static $bodyCache = '';

	/** @var true if the request body has already been read */
	private static $bodyCacheIsReady = false;

	/**
	 * @var bool
	 * @deprecated use $VALIDATE_FILE_UPLOAD instead
	 */
	static $TestMode = false;

	/**
	 * Returns the remote host IP address, attempting to locate originating
	 * IP of the requester in the case of proxy/load balanced requests.
	 *
	 * @see http://en.wikipedia.org/wiki/X-Forwarded-For
	 * @return string
	 */
	static function GetRemoteHost()
	{
		if (array_key_exists('HTTP_X_CLUSTER_CLIENT_IP',$_SERVER)) return $_SERVER['HTTP_X_CLUSTER_CLIENT_IP'];
		if (array_key_exists('HTTP_X_FORWARDED_FOR',$_SERVER)) return $_SERVER['HTTP_X_FORWARDED_FOR'];
		if (array_key_exists('X_FORWARDED_FOR',$_SERVER)) return $_SERVER['X_FORWARDED_FOR'];
		if (array_key_exists('REMOTE_ADDR',$_SERVER)) return $_SERVER['REMOTE_ADDR'];
		return "0.0.0.0";
	}

	/**
	 * Returns true if the current session is running in SSL
	 */
	static function IsSSL()
	{
		return isset($_SERVER['HTTPS']) && $_SERVER['HTTPS'] != "" && $_SERVER['HTTPS'] != "off";
	}

	/** In the case of URL re-writing, sometimes querystrings appended to a URL can get
	 * lost.  This function examines the original request URI and updates $_REQUEST
	 * superglobal to ensure that it contains all of values in the qeurtystring
	 *
	 */
	public static function NormalizeUrlRewrite()
	{
		$uri = array();
		if (isset($_SERVER["REQUEST_URI"]))
		{
			$uri = parse_url($_SERVER["REQUEST_URI"]);
		}
		elseif (isset($_SERVER["QUERY_STRING"]))
		{
			$uri['query'] = $_SERVER["QUERY_STRING"];
		}

		if (isset($uri['query']))
		{
			$parts = explode("&",$uri['query']);
			foreach ($parts as $part)
			{
				$keyval = explode("=",$part,2);
				$_REQUEST[$keyval[0]] = isset($keyval[1]) ? urldecode($keyval[1]) : "";
			}
		}
	}

	/**
	* Returns the root url of the server without any subdirectories
	* @return string URL path with trailing slash
	*/
	public static function GetServerRootUrl()
	{
		$url = self::GetCurrentURL(false);
		$parts = explode('/', $url);
		if (count($parts) < 2) throw new Exception('RequestUtil is unable to determine the server root');
		return $parts[0] . '//' . $parts[2] . '/';
	}

	/**
	 * Returns the base url of the currently executing script.  For example
	 * the script http://localhost/myapp/index.php would return http://localhost/myapp/
	 * @return string URL path with trailing slash
	 */
	public static function GetBaseURL()
	{
		$url = self::GetCurrentURL(false);
		$slash = strripos($url,"/");
		return substr($url,0,$slash+1);
	}

	/**
	 * Returns the parts of the url as deliminated by forward slashes for example /this/that/other
	 * will be returned as an array [this,that,other]
	 * @param string root folder for the app (ex. 'myapp' or 'myapp/subdir1')
	 * @return array
	 */
	public static function GetUrlParts($appRoot = '')
	{
		$urlqs = explode("?", self::GetCurrentURL(),2);
		$url = $urlqs[0];

		// if a root folder was provided, then we need to strip that out as well
		if ($appRoot) $url = str_replace($appRoot.'/','',$url);

		$parts = explode("/", $url);
		// we only want the parts starting with #3 (after http://server/)

		array_shift($parts);
		array_shift($parts);
		array_shift($parts);

		// if there is no action specified then we don't want to return an array with an empty string
		while (count($parts) && $parts[0] == '')
		{
			array_shift($parts);
		}

		return $parts;
	}

	/**
	 * Returns the request method (GET, POST, PUT, DELETE).  This is detected based
	 * on the HTTP request method, a special URL parameter, or a request header
	 * with the name 'X-HTTP-Method-Override'
	 *
	 * For clients or servers that don't support PUT/DELETE requests, the emulated
	 * param can be used or the override header
	 *
	 * @param string name of the querystring parameter that has the overridden request method
	 */
	public static function GetMethod($emulateHttpParamName = '_method')
	{

		if (array_key_exists($emulateHttpParamName, $_REQUEST)) return $_REQUEST[$emulateHttpParamName];

		$headers = self::GetRequestHeaders();

		// this is used by backbone
		if (array_key_exists('X-HTTP-Method-Override', $headers))
		{
			return $headers['X-HTTP-Method-Override'];
		}

		return array_key_exists('REQUEST_METHOD', $_SERVER) ? $_SERVER['REQUEST_METHOD'] : '';
	}

	/**
	 * Return all request headers using the best method available for the server environment
	 * @return array
	 */
	public static function GetRequestHeaders()
	{

		if (function_exists('getallheaders')) return getallheaders();

		$headers = array();
		foreach ($_SERVER as $k => $v)
		{
			if (substr($k, 0, 5) == "HTTP_")
			{
				$k = str_replace('_', ' ', substr($k, 5));
				$k = str_replace(' ', '-', ucwords(strtolower($k)));
				$headers[$k] = $v;
			}
		}
		return $headers;
	}

	/**
	 * Returns the body/payload of a request.  this is cached so that this method
	 * may be called multiple times.
	 *
	 * Note: if this is a PUT request and the body is not returning data, then
	 * you must look in other code an libraries that may read from php://input,
	 * which can only be read one time for PUT requests
	 *
	 * @return string
	 */
	public static function GetBody()
	{
		if (!self::$bodyCacheIsReady)
		{
			self::$bodyCache = @file_get_contents('php://input');
			self::$bodyCacheIsReady = true;
		}

 		return self::$bodyCache;
	}

	/**
	 * Used primarily for unit testing.  Set the contents of the request body
	 * @param string $contents
	 */
	public static function SetBody($contents)
	{
		self::$bodyCache = $contents;
		self::$bodyCacheIsReady = true;
	}

	/**
	 * Return the HTTP headers sent along with the request.  This will attempt
	 * to use apache_request_headers if available in the environment, otherwise
	 * will manually build the headers using $_SERVER superglobal
	 * @return array
	 */
	public static function GetHeaders()
	{
		$headers = false;

		if (function_exists('apache_request_headers')) $headers = apache_request_headers();

		if ($headers === false)
		{
			// apache_request_headers is not supported in this environment

			$headers = array();
			foreach ($_SERVER as $key => $value)
			{
				if (substr($key, 0, 5) <> 'HTTP_')
				{
					continue;
				}
				$header = str_replace(' ', '-', ucwords(str_replace('_', ' ', strtolower(substr($key, 5)))));
				$headers[$header] = $value;
			}
		}

		return $headers;
	}

	/** Returns the full URL of the PHP page that is currently executing
	 *
	 * @param bool $include_querystring (optional) Specify true/false to include querystring. Default is true.
	 * @param bool $append_post_vars true to append post variables to the querystring as GET parameters Default is false
	 * @return string URL
	 */
	public static function GetCurrentURL($include_querystring = true, $append_post_vars = false)
	{
		$server_protocol = isset($_SERVER["SERVER_PROTOCOL"]) ? $_SERVER["SERVER_PROTOCOL"] : "";
		$http_host = isset($_SERVER["HTTP_HOST"]) ? $_SERVER["HTTP_HOST"] : "";
		$server_port = isset($_SERVER["SERVER_PORT"]) ? $_SERVER["SERVER_PORT"] : "";

		$protocol = substr($server_protocol, 0, strpos($server_protocol, "/"))
			. (isset($_SERVER["HTTPS"]) && $_SERVER["HTTPS"] == "on" ? "S" : "");
		$port = "";

		$domainport = explode(":",$http_host);
		$domain = $domainport[0];

		$port = (isset($domainport[1])) ? $domainport[1] : $server_port;

		// ports 80 and 443 are generally not included in the url
		$port = ($port == "" || $port == "80" || $port == "443") ? "" : (":" . $port);


		if (isset($_SERVER['REQUEST_URI']))
		{
			// REQUEST_URI is more accurate but isn't always defined on windows
			// in particular for the format http://www.domain.com/?var=val
			$pq = explode("?",$_SERVER['REQUEST_URI'],2);
			$path = $pq[0];
			$qs = isset($pq[1]) ? "?" . $pq[1] : "";
		}
		else
		{
			// otherwise use SCRIPT_NAME & QUERY_STRING
			$path = isset($_SERVER['SCRIPT_NAME']) ? $_SERVER['SCRIPT_NAME'] : "";
			$qs = isset($_SERVER['QUERY_STRING']) ? "?" . $_SERVER['QUERY_STRING'] : "";
		}

		// if we also want the post variables appended we can get them as a querystring from php://input
		if ($append_post_vars && isset($_POST))
		{
			$post = self::GetBody();
			$qs .= $qs ? "&$post" : "?$post";
		}

		$url = strtolower($protocol) . "://" . $domain . $port . $path . ($include_querystring ? $qs : "");

		return $url;
	}



	/**
	* Returns a form upload as a FileUpload object.  This function throws an exeption on fail
	* with details, so it is recommended to use try/catch when calling this function
	*
	* @param	string $fieldname name of the html form field
	* @param	bool $b64encode true to base64encode file data (default false)
	* @param	bool $ignore_empty true to not throw exception if form fields doesn't contain a file (default false)
	* @param	int $max_kb maximum size allowed for upload (default unlimited)
	* @param	array $ok_types if array is provided, only files with those Extensions will be allowed (default all)
	* @return   FileUpload object (or null if $ignore_empty = true and there is no file data)
	*/
	public static function GetFileUpload($fieldname, $ignore_empty = false, $max_kb = 0, $ok_types = null)
	{
		// make sure there is actually a file upload
		if (!isset($_FILES[$fieldname]))
		{
			// this means the form field wasn't present which is generally an error
			// however if ignore is specified, then return empty string
			if ($ignore_empty)
			{
				return null;
			}
			throw new Exception("\$_FILES['".$fieldname."'] is empty.  Did you forget to add enctype='multipart/form-data' to your form code?");
		}

		// make sure a file was actually uploaded, otherwise return null
		if($_FILES[$fieldname]['error'] == 4)
		{
			return;
		}

		// get the upload ref
		$upload = $_FILES[$fieldname];

		// make sure there were no errors during upload, but ignore case where
		if ($upload['error'])
		{
			$error_codes[0] = "The file uploaded with success.";
			$error_codes[1] = "The uploaded file exceeds the upload_max_filesize directive in php.ini.";
			$error_codes[2] = "The uploaded file exceeds the MAX_FILE_SIZE directive that was specified in the html form.";
			$error_codes[3] = "The uploaded file was only partially uploaded.";
			$error_codes[4] = "No file was uploaded.";
			throw new Exception("Error uploading file: " . $error_codes[$upload['error']]);
		}

		// backwards compatibility
		if (self::$TestMode) self::$VALIDATE_FILE_UPLOAD = false;

		// make sure this is a legit file request
		if ( self::$VALIDATE_FILE_UPLOAD && is_uploaded_file($upload['tmp_name']) == false )
		{
			throw new Exception("Unable to access this upload: " . $fieldname);
		}

		// get the filename and Extension
		$tmp_path = $upload['tmp_name'];
		$info = pathinfo($upload['name']);

		$fupload = new FileUpload();
		$fupload->Name = $info['basename'];
		$fupload->Size = $upload['size'];
		$fupload->Type = $upload['type'];
		$fupload->Extension = strtolower($info['extension']);


		if ($ok_types && !in_array($fupload->Extension, $ok_types) )
		{
			throw new Exception("The file '".htmlentities($fupload->Name)."' is not a type that is allowed.  Allowed file types are: " . (implode(", ",$ok_types)) . ".");
		}

		if ($max_kb && ($fupload->Size/1024) > $max_kb)
		{
			throw new Exception("The file '".htmlentities($fupload->Name)."' is to large.  Maximum allowed size is " . number_format($max_kb/1024,2) . "Mb");
		}

		// open the file and read the entire contents
		$fh = fopen($tmp_path,"r");
		$fupload->Data = fread($fh, filesize($tmp_path));
		fclose($fh);

		return $fupload;
	}

	/**
	 * Returns a form upload as an xml document with the file data base64 encoded.
	 * suitable for storing in a clob or blob
	 *
	* @param	string $fieldname name of the html form field
	* @param	bool $b64encode true to base64encode file data (default true)
	* @param	bool $ignore_empty true to not throw exception if form fields doesn't contain a file (default false)
	* @param	int $max_kb maximum size allowed for upload (default unlimited)
	* @param	array $ok_types if array is provided, only files with those Extensions will be allowed (default all)
	* @return   string or null
	 */
	public static function GetFile($fieldname, $b64encode = true, $ignore_empty = false, $max_kb = 0, $ok_types = null)
	{
		$fupload = self::GetFileUpload($fieldname, $ignore_empty, $max_kb, $ok_types);
		return ($fupload) ? $fupload->ToXML($b64encode) : null;
	}

	/**
	* Sets a value as if it was sent from the browser - primarily used for unit testing
	*
	* @param	string $key
	* @param	variant $val
	*/
	public static function Set($key, $val)
	{
		$_REQUEST[$key] = $val;
	}

	/**
	* Clears all browser input - primarily used for unit testing
	*
	*/
	public static function ClearAll()
	{
		$_REQUEST = array();
		$_FILES = array();

		self::$bodyCache = "";
		self::$bodyCacheIsReady = false;
	}

	/**
	* Returns a form parameter as a string, handles null values.  Note that if
	* $ENCODE_NON_ASCII = true then the value will be passed through VerySimpleStringUtil::EncodeToHTML
	* before being returned.
	*
	* If the form field is a multi-value type (checkbox, etc) then an array may be returned
	*
	* @param	string $fieldname
	* @param	string $default value returned if $_REQUEST[$fieldname] is blank or null (default = empty string)
	* @param	bool $escape if true htmlspecialchars($val) is returned (default = false)
	* @return   string | array
	*/
	public static function Get($fieldname, $default = "", $escape = false)
	{
		$val = (isset($_REQUEST[$fieldname]) && $_REQUEST[$fieldname] != "") ? $_REQUEST[$fieldname] : $default;

		if ($escape)
		{
			$val = htmlspecialchars($val, ENT_COMPAT, null, false);
		}

		if (self::$ENCODE_NON_ASCII)
		{
			if (is_array($val))
			{
				foreach ($val as $k=>$v)
				{
					$val[$k] = VerySimpleStringUtil::EncodeToHTML($v);
				}
			}
			else
			{
				$val = VerySimpleStringUtil::EncodeToHTML($val);
			}
		}

		return $val;
	}

	/**
	 * Returns true if the given form field has non-ascii characters
	 * @param string $fieldname
	 * @return bool
	 */
	public static function HasNonAsciiChars($fieldname)
	{
		$val = isset($_REQUEST[$fieldname]) ? $_REQUEST[$fieldname] : '';
		return VerySimpleStringUtil::EncodeToHTML($val) != $val;
	}

	/**
	* Returns a form parameter and persists it in the session.  If the form parameter was not passed
	* again, then it returns the session value.  if the session value doesn't exist, then it returns
	* the default setting
	*
	* @param	string $fieldname
	* @param	string $default
	* @return   string
	*/
	public static function GetPersisted($fieldname, $default = "",$escape = false)
	{
		if ( isset($_REQUEST[$fieldname]) )
		{
			$_SESSION["_PERSISTED_".$fieldname] = self::Get($fieldname, $default, $escape);
		}

		if ( !isset($_SESSION["_PERSISTED_".$fieldname]) )
		{
			$_SESSION["_PERSISTED_".$fieldname] = $default;
		}

		return $_SESSION["_PERSISTED_".$fieldname];
	}

	/**
	* Returns a form parameter as a date formatted for mysql YYYY-MM-DD,
	* expects some type of date format.  if default value is not provided,
	* will return today.  if default value is empty string "" will return
	* empty string.
	*
	* @param	string $fieldname
	* @param	string $default default value = today
	* @param	bool $includetime whether to include the time in addition to date
	* @return   string
	*/
	public static function GetAsDate($fieldname, $default = "date('Y-m-d')", $includetime = false)
	{
		$returnVal = self::Get($fieldname,$default);

		if ($returnVal == "date('Y-m-d')")
		{
			return date('Y-m-d');
		}
		elseif ($returnVal == "date('Y-m-d H:i:s')")
		{
			return date('Y-m-d H:i:s');
		}
		elseif ($returnVal == "")
		{
			return "";
		}
		else
		{
			if ($includetime)
			{
				if (self::Get($fieldname."Hour"))
				{
					$hour = self::Get($fieldname."Hour",date("H"));
					$minute = self::Get($fieldname."Minute",date("i"));
					$ampm = self::Get($fieldname."AMPM","AM");

					if ($ampm == "PM")
					{
						$hour = ($hour*1)+12;
					}
					$returnVal .= " " . $hour . ":" . $minute . ":" . "00";
				}

				return date("Y-m-d H:i:s",strtotime($returnVal));
			}
			else
			{
				return date("Y-m-d",strtotime($returnVal));
			}
		}
	}

	/**
	* Returns a form parameter as a date formatted for mysql YYYY-MM-DD HH:MM:SS,
	* expects some type of date format.  if default value is not provided,
	* will return now.  if default value is empty string "" will return
	* empty string.
	*
	* @param	string $fieldname
	* @param	string $default default value = today
	* @return   string
	*/
	public static function GetAsDateTime($fieldname, $default = "date('Y-m-d H:i:s')")
	{
		return self::GetAsDate($fieldname,$default,true);
	}

	/**
	 * Returns a form parameter minus currency symbols
	 *
	 * @param	string	$fieldname
	 * @return	string
	 */
	public static function GetCurrency($fieldname)
	{
		return str_replace(array(',','$'),'',self::Get($fieldname));
	}


}

?>