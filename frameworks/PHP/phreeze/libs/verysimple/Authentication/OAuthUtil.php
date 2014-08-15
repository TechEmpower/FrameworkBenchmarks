<?php
/** @package	verysimple::Authentication */

require_once "oauth/OAuthStore.php";
require_once "oauth/OAuthRequester.php";
require_once "verysimple/String/VerySimpleStringUtil.php";

/**
 * A set of utility functions for working with OAuth
 *
 * @package	verysimple::String
 * @author Jason Hinkle
 * @copyright  1997-2012 VerySimple, Inc.
 * @license	http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version 1.0
 */
class OAuthUtil
{

	/**
	 * Given a URL return an OAuth signed URL.  This will handle creating a timestamp and nonce
	 *
	 * @param string $url the unsigned url
	 * @param string $method request method GET, POST, PUT, DELETE
	 * @param string $key oauth key
	 * @param string $secret oauth secret
	 * @param array $params querystring or post parameters
	 * @param string $body the body contents of the request
	 * @param string $signature_method method used for signature (default = 'HMAC_SHA1')
	 */
	public static function SignUrl($url,$method,$key,$secret,$params=null,$body=null,$signature_method='HMAC_SHA1')
	{
		$options = array('consumer_key' => $key, 'consumer_secret' => $secret);
		$params = $params ? $params : array();

		OAuthStore::instance("2Leg", $options);

		// Obtain a request object for the request we want to make
		$request = new OAuthRequester($url, $method, $params, $body);

		$sig = $request->sign($key,null,'');

		$data = $request->signatureBaseString();

		$url = substr( urldecode($data . '&oauth_signature=' . $request->calculateDataSignature($data,$secret,'',$signature_method) ), strlen($method) + 1);

		$url = VerySimpleStringUtil::ReplaceFirst('&', '?', $url);

		return $url;
	}
}

?>