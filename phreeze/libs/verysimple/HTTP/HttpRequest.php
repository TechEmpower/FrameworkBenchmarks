<?php
/** @package    verysimple::HTTP */

/**
 * HttpRequest is a utility method for makine HTTP requests
 *
 * @package    verysimple::HTTP
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.0
 */
class HttpRequest
{

	static $METHOD_GET = "GET";
	static $METHOD_POST = "POST";
	static $METHOD_PUT = "PUT";
	static $METHOD_DELETE = "DELETE";

	static $USER_AGENT = "verysimple::HttpRequest";
	static $VERIFY_CERT = false;

	/**
	 *
	 * @param $method
	 * @param $params
	 */
	static function RestRequest($endpoint, $method, $params = Array())
	{
		$qs = HttpRequest::ArrayToQueryString($params);
		$ch = null;

		switch ($method)
		{
			case HttpRequest::$METHOD_GET:
				$ch = curl_init($endpoint . ($qs ? "?" . $qs : ""));
				break;
			case HttpRequest::$METHOD_POST:
				$ch = curl_init($endpoint);
				curl_setopt($ch, CURLOPT_POST, 1);
				curl_setopt($ch, CURLOPT_POSTFIELDS, $qs);
				break;
			case HttpRequest::$METHOD_PUT:
				$ch = curl_init($endpoint);
				// curl_setopt($ch, CURLOPT_PUT, 1); // <- this method requires CURLOPT_INFILE
				curl_setopt($ch, CURLOPT_CUSTOMREQUEST, 'PUT');
				curl_setopt($ch, CURLOPT_POSTFIELDS, $qs);
				break;
			case HttpRequest::$METHOD_DELETE:
				$ch = curl_init($endpoint);
				curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "DELETE");
				curl_setopt($ch, CURLOPT_POSTFIELDS, $qs);
				break;
		}
		
		curl_setopt($ch,		CURLOPT_HTTPHEADER, array("Expect:  ") );  //Fixes the HTTP/1.1 417 Expectation Failed Bug

		curl_setopt($ch, CURLOPT_FOLLOWLOCATION, 1);
		curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
		curl_setopt($ch, CURLOPT_VERBOSE, 0); // <- ENABLE DEBUGGING
		curl_setopt($ch, CURLOPT_USERAGENT, HttpRequest::$USER_AGENT);
		curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, HttpRequest::$VERIFY_CERT);
		curl_setopt($ch, CURLOPT_NOPROGRESS, 1);

		// make the request
		$response = curl_exec ($ch);

		// if error is not empty, then a network error occured
		$error = curl_error($ch);
		if ($error) {$response .= $error;}

		curl_close ($ch);

		return $response;
	}

	/**
	* Make an HTTP POST request using the best method available on the server
	*
	* @param string $url
	* @param array $data (array of field/value pairs)
	* @param bool true to require verification of SSL cert
	* @return string
	*/
	static function Post($url, $data, $verify_cert = false, $timeout = 30)
	{
		if (function_exists("curl_init"))
		{
			return HttpRequest::CurlPost($url, $data, $verify_cert, $timeout);
		}
		else
		{
			return HttpRequest::FilePost($url, $data, $verify_cert, $timeout);
		}
	}

	/**
	 * Make an HTTP GET request using the best method available on the server
	 *
	 * @param string $url
	 * @param array $data (array of field/value pairs)
	 * @param bool true to require verification of SSL cert
	 * @return string
	 */
	static function Get($url, $data = "", $verify_cert = false, $timeout = 30)
	{
		if (function_exists("curl_init"))
		{
			return HttpRequest::CurlPost($url, $data, $verify_cert, $timeout);
		}
		else
		{
			return HttpRequest::FilePost($url, $data, $verify_cert, $timeout);
		}
	}

	/**
	* Make an HTTP PUT reequest using the best method available on the server
	*
	* @param string $url
	* @param array $data (array of field/value pairs)
	* @param bool true to require verification of SSL cert
	* @param int timeout (in seconds)
	* @return string
	*/
	static function Put($url, $data = "", $verify_cert = false, $timeout = 30)
	{
		if (function_exists("curl_init"))
		{
			return HttpRequest::CurlPut($url, $data, $verify_cert, $timeout);
		}
		else
		{
			throw new Exception('PUT request is not supported on systems without curl installed');
		}
	}

	/**
	 * Make an HTTP GET request using file_get_contents
	 *
	 * @param string $url
	 * @param array $data (array of field/value pairs)
	 * @param bool true to require verification of SSL cert
	 * @return string
	 */
	static function FileGet($url, $data = "", $verify_cert = false, $timeout = 30)
	{
		$qs = HttpRequest::ArrayToQueryString($data);
		$full_url = $url . ($qs ? "?" . $qs : "");
		return file_get_contents( $full_url );
	}

	/**
	 * Make an HTTP POST request using file_get_contents
	 *
	 * @param string $url
	 * @param array $data (array of field/value pairs)
	 * @param bool true to require verification of SSL cert
	 * @return string
	 */
	static function FilePost($url, $data = "", $verify_cert = false, $timeout = 30)
	{
		$qs = HttpRequest::ArrayToQueryString($data);
		$url = $url . ($qs ? "?" . $qs : "");

		$show_headers = false;
		$url = parse_url($url);

		if (!isset($url['port'])) {
			if ($url['scheme'] == 'http') { $url['port']=80; }
			elseif ($url['scheme'] == 'https') { $url['port']=443; }
		}
		$url['query']=isset($url['query'])?$url['query']:'';

		$url['protocol']=$url['scheme'].'://';
		$eol="\r\n";

		$headers =  "POST ".$url['protocol'].$url['host'].$url['path']." HTTP/1.0".$eol.
			"Host: ".$url['host'].$eol.
			"Referer: ".$url['protocol'].$url['host'].$url['path'].$eol.
			"Content-Type: application/x-www-form-urlencoded".$eol.
			"Content-Length: ".strlen($url['query']).$eol.
			$eol.$url['query'];
		$fp = fsockopen($url['host'], $url['port'], $errno, $errstr, 30);
		if($fp)
		{
			fputs($fp, $headers);
			$result = '';
			while(!feof($fp)) { $result .= fgets($fp, 128); }
			fclose($fp);
			if (!$show_headers)
			{
				//removes headers
				$match = preg_split("/\r\n\r\n/s",$result,2);
				$result = $match[1];
			}

			return $result;
		}
	}

	/**
	 * Make an HTTP GET request using CURL
	 *
	 * @param string $url
	 * @param variant $data querystring or array of field/value pairs
	 * @param bool true to require verification of SSL cert
	 * @return string
	 */
	static function CurlGet($url, $data = "", $verify_cert = false, $timeout = 30)
	{
		return HttpRequest::CurlRequest("GET",$url, $data, $verify_cert, $timeout);
	}

	/**
	 * Make an HTTP POST request using CURL
	 *
	 * @param string $url
	 * @param variant $data querystring or array of field/value pairs
	 * @param bool true to require verification of SSL cert
	 * @return string
	 */
	static function CurlPost($url, $data, $verify_cert = false, $timeout = 30)
	{
		return HttpRequest::CurlRequest("POST",$url, $data, $verify_cert, $timeout);
	}

	/**
	 * Make an HTTP PUT request using CURL
	 *
	 * @param string $url
	 * @param variant $data querystring or array of field/value pairs
	 * @param bool true to require verification of SSL cert
	 * @return string
	 */
	static function CurlPut($url, $data, $verify_cert = false, $timeout = 30)
	{
		return HttpRequest::CurlRequest("PUT",$url, $data, $verify_cert, $timeout);
	}

	/**
	 * Make an HTTP request using CURL
	 *
	 * @param string "POST" or "GET"
	 * @param string $url
	 * @param variant $data querystring or array of field/value pairs
	 * @param bool true to require verification of SSL cert
	 * @return string
	 */
	static function CurlRequest($method, $url, $data, $verify_cert = false, $timeout = 30)
	{
		// if the data provided is in array format, convert it to a querystring
		$qs = HttpRequest::ArrayToQueryString($data);

		$agent = "verysimple::HttpRequest";

		// $header[] = "Accept: text/vnd.wap.wml,*.*";

		$fp = null;

		if ($method == "POST")
		{
			$ch = curl_init($url);
			curl_setopt($ch,		CURLOPT_POST, 1);
			curl_setopt($ch,		CURLOPT_POSTFIELDS, $qs);
		}
		elseif ($method == 'PUT')
		{
			$ch = curl_init($url);
			curl_setopt($ch, CURLOPT_PUT, true);

			if ($data)
			{
				// with a PUT request the body must be written to a file stream
				$fp = fopen('php://temp/maxmemory:256000', 'w');
				if (!$fp) {
					throw new Exception('Unable to write to php://temp for PUT request');
				}
				fwrite($fp, $data);
				fseek($fp, 0);

				// if the PUT request contains JSON data then add the content type header
				if (json_encode($data)) curl_setopt($ch, CURLOPT_HTTPHEADER, array("Content-Type: application/json") );

				curl_setopt($ch, CURLOPT_INFILE, $fp);
				curl_setopt($ch, CURLOPT_INFILESIZE, strlen($data));
				curl_setopt($ch, CURLOPT_BINARYTRANSFER, true);
			}

		}
		else
		{
			$full_url = $url . ($qs ? "?" . $qs : "");
			$ch = curl_init($full_url);
		}

		curl_setopt($ch,		CURLOPT_HTTPHEADER, array("Expect:  ") );  //Fixes the HTTP/1.1 417 Expectation Failed Bug
		
		curl_setopt($ch,		CURLOPT_FOLLOWLOCATION, 1);
		curl_setopt($ch,		CURLOPT_RETURNTRANSFER, 1);
		curl_setopt($ch,		CURLOPT_VERBOSE, 0); ########### debug
		curl_setopt($ch,	    CURLOPT_USERAGENT, $agent);
		curl_setopt($ch,		CURLOPT_SSL_VERIFYPEER, $verify_cert);
		curl_setopt($ch,		CURLOPT_NOPROGRESS, 1);
		// curl_setopt($ch,	    CURLOPT_HTTPHEADER, $header);
		// curl_setopt($ch,		CURLOPT_COOKIEJAR, "curl_cookie");
		// curl_setopt($ch,		CURLOPT_COOKIEFILE, "curl_cookie");
		curl_setopt($ch,		CURLOPT_TIMEOUT,$timeout);

		$tmp = curl_exec ($ch);
		$error = curl_error($ch);

		if ($fp) @fclose($fp); // if a PUT request had body data, close the file stream

		if ($error != "") {$tmp .= $error;}
		curl_close ($ch);

		return $tmp;
	}

	/**
	 * Converts an array into a URL querystring
	 * @param array key/value pairs
	 * @return string
	 */
	static function ArrayToQueryString($arr)
	{
		$qs = $arr;

		if (is_array($arr))
		{
			// convert the data array into a url querystring
			$qs = "";
			$delim = "";
			foreach (array_keys($arr) as $key)
			{
				$qs .= $delim . $key ."=" . $arr[$key];
				$delim = "&";
			}
		}

		return $qs;
	}
}
?>