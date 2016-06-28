<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */
namespace Pimf;

/**
 * Server and execution environment information.
 *
 * @package Pimf
 * @author  Gjero Krsteski <gjero@krsteski.de>
 *
 * @property string X_REQUESTED_WITH       It is sent by the Ajax functions of most major Frameworks
 * @property string HTTP                   Is the application running under HTTP protocol?
 * @property string HTTPS                  Is the application running under HTTPS protocol?
 * @property string SERVER_PROTOCOL        Name and revision of the information protocol via which the page was requested; i.e. 'HTTP/1.0';
 * @property string CONTENT_LENGTH         The Content-Length
 * @property string HOST                   The name of the server host under which the current script is executing.
 * @property string SERVER_NAME            The name of the server host under which the current script is executing.
 * @property string SERVER_PORT            Get the port
 * @property string PHP_SELF               Filename of the currently executing script.
 * @property string SCRIPT_NAME            Get Script Name (physical path)
 * @property string PATH_INFO              Get Path Info (virtual path)
 * @property string X_FORWARDED_FOR        Do on your machine is behind the proxy than us it instead of REMOTE_ADDR
 * @property string CLIENT_IP              Get the client ip address
 * @property string REMOTE_ADDR            The IP address from which the user is viewing the current page.
 * @property string HTTP_REFERER           Get Referer - it cannot really be trusted.
 * @property string USER_AGENT             Contents of the User-Agent from the current request, if there is one.
 * @property string HTTP_USER_AGENT        Contents of the User-Agent: header from the current request, if there is one.
 * @property string REQUEST_URI            The URI which was given in order to access this page; for instance, '/index.html'.
 * @property string REQUEST_METHOD         Which request method was used to access the page; i.e. 'GET', 'HEAD', 'POST', 'PUT'.
 * @property string HTTP_IF_MODIFIED_SINCE Get request header from Apache even on PHP running as a CGI
 * @property string HTTP_IF_NONE_MATCH     Get request header from Apache even on PHP running as a CGI
 */
class Environment
{
  /**
   * @var Param
   */
  private $envData;

  /**
   * @param array $envData Like $_SERVER
   */
  public function __construct(array $envData)
  {
    $this->envData = new Param($envData);
  }

  /**
   * @return Param
   */
  public function getData()
  {
    return $this->envData;
  }

  /**
   * @param $key
   *
   * @return string
   */
  public function __get($key)
  {
    return $this->envData->get($key);
  }

  /**
   * Is this an AJAX request?
   *
   * @return bool
   */
  public function isAjax()
  {
    return $this->X_REQUESTED_WITH === 'XMLHttpRequest';
  }

  /**
   * Is the application running under HTTP protocol?
   *
   * @return bool
   */
  public function isHttp()
  {
    return (bool)$this->HTTP;
  }

  /**
   * Is the application running under HTTPS protocol?
   *
   * @return bool
   */
  public function isHttps()
  {
    return $this->HTTPS === 'on';
  }

  /**
   * Get Host
   *
   * @return string
   */
  public function getHost()
  {
    if ($this->HOST) {

      if (strpos($this->HOST, ':') !== false) {
        $hostParts = explode(':', $this->HOST);

        return $hostParts[0];
      }

      return $this->HOST;
    }

    return $this->SERVER_NAME;
  }

  /**
   * Get Host with Port
   *
   * @return string
   */
  public function getHostWithPort()
  {
    return '' . $this->getHost() . ':' . $this->SERVER_PORT;
  }

  /**
   * Physical path + virtual path
   *
   * @return string
   */
  public function getPath()
  {
    return $this->SCRIPT_NAME . $this->PATH_INFO;
  }

  /**
   * Get remote IP
   *
   * @return string
   */
  public function getIp()
  {
    if ($this->X_FORWARDED_FOR) {
      return $this->X_FORWARDED_FOR;
    }

    if ($this->CLIENT_IP) {
      return $this->CLIENT_IP;
    }

    if ($this->SERVER_NAME) {
      return gethostbyname($this->SERVER_NAME);
    }

    return $this->REMOTE_ADDR;
  }

  /**
   * Get User Agent
   *
   * @return string|null
   */
  public function getUserAgent()
  {
    if ($this->USER_AGENT) {
      return $this->USER_AGENT;
    }

    if ($this->HTTP_USER_AGENT) {
      return $this->HTTP_USER_AGENT;
    }

    return null;
  }

  /**
   * Gives you the current page URL
   *
   * @return string
   */
  public function getUrl()
  {
    $protocol = strpos(strtolower($this->PATH_INFO), 'https') === false ? 'http' : 'https';

    return $protocol . '://' . $this->getHost();
  }

  /**
   * Try to get a request header.
   *
   * @param string $header
   *
   * @return array
   */
  public function getRequestHeader($header)
  {
    $header = str_replace('-', '_', strtoupper($header));
    $value  = $this->{'HTTP_' . $header};

    if (!$value) {
      $headers = $this->getRequestHeaders();
      $value   = !empty($headers[$header]) ? $headers[$header] : null;
    }

    return $value;
  }

  /**
   * Try to determine all request headers
   *
   * @return array
   */
  public function getRequestHeaders()
  {
    $headers = array();

    foreach ($this->envData->getAll() as $key => $value) {
      if ('HTTP_' === substr($key, 0, 5)) {
        $headers[substr($key, 5)] = $value;
      }
    }

    return $headers;
  }
}
