<?php
/**
* Request
* Request store all the server request data.
*
* @package : Request
* @category : System Middleware
* @author : Unic Framework
* @link : https://github.com/unicframework/unic
*/

class Request {
  //Request header Information
  public $scheme;
  public $method;
  public $time;
  public $time_float;
  public $protocol;
  public $accept;
  public $language;
  public $encoding;
  public $connection;
  public $content_type;
  public $content_length;
  public $user_agent;
  public $referrer;

  //Request body information
  public $body;
  
  //Server information
  public $hostname;
  public $host;
  public $port;
  public $gateway_interface;
  public $server_addr;
  public $server_name;
  public $server_software;
  public $server_protocol;
  public $server_signature;
  public $document_root;

  //Path information
  public $uri;
  public $url;
  public $path;
  public $path_info;

  //Request information
  public $get;
  public $post;
  public $put;
  public $delete;
  public $patch;
  public $head;
  public $options;
  public $connect;
  public $trace;
  public $copy;
  public $link;
  public $unlink;
  public $lock;
  public $unlock;
  public $purge;
  public $propfind;
  public $view;
  public $any;
  public $is_secure;
  public $is_ajax;
  public $is_get;
  public $is_post;
  public $is_put;
  public $is_delete;
  public $is_patch;
  public $is_head;
  public $is_options;
  public $is_connect;
  public $is_trace;
  public $is_copy;
  public $is_link;
  public $is_unlink;
  public $is_lock;
  public $is_unlock;
  public $is_purge;
  public $is_propfind;
  public $is_view;
  public $is_http;
  public $is_https;
  public $files;
  public $session;
  public $cookie;

  //User information
  public $remote_addr;
  public $is_referred;

  function __construct() {
    /**
    * Request Header Information
    * Get Request header information.
    */
    //Get scheme of request (https or http)
    $this->scheme = (isset($_SERVER['HTTPS']) && ($_SERVER['HTTPS'] === 'on' || $_SERVER['HTTPS'] === 1)) || (isset($_SERVER['HTTP_X_FORWARDED_PROTO']) && $_SERVER['HTTP_X_FORWARDED_PROTO'] === 'https') || (isset($_SERVER['HTTP_FRONT_END_HTTPS']) && strtolower($_SERVER['HTTP_FRONT_END_HTTPS'])!== 'off') ? 'https' : 'http';

    //Get request method get, post, put, delete
    $this->method = strtoupper($_SERVER['REQUEST_METHOD']);

    //Get Request time
    $this->time = isset($_SERVER['REQUEST_TIME']) ? $_SERVER['REQUEST_TIME'] : NULL;

    $this->time_float = isset($_SERVER['REQUEST_TIME_FLOAT']) ? $_SERVER['REQUEST_TIME_FLOAT'] : NULL;

    //Get server protocol
    $this->protocol = isset($_SERVER['SERVER_PROTOCOL']) ? $_SERVER['SERVER_PROTOCOL'] : NULL;

    //Get http_accept
    $this->accept = isset($_SERVER['HTTP_ACCEPT']) ? $_SERVER['HTTP_ACCEPT'] : NULL;

    //Get accept language
    $this->language = isset($_SERVER['HTTP_ACCEPT_LANGUAGE']) ? $_SERVER['HTTP_ACCEPT_LANGUAGE'] : NULL;

    //Get connection
    $this->connection = isset($_SERVER['HTTP_CONNECTION']) ? $_SERVER['HTTP_CONNECTION'] : NULL;

    //Get http encoding
    $this->encoding = isset($_SERVER['HTTP_ACCEPT_ENCODING']) ? $_SERVER['HTTP_ACCEPT_ENCODING'] : NULL;

    //Get content type, request MIME type from header
    $this->content_type = isset($_SERVER['CONTENT_TYPE']) ? $_SERVER['CONTENT_TYPE'] : (isset($_SERVER['HTTP_CONTENT_TYPE']) ? $_SERVER['HTTP_CONTENT_TYPE'] : NULL);

    //Get content length
    $this->content_length = isset($_SERVER['CONTENT_LENGTH']) ? $_SERVER['CONTENT_LENGTH'] : (isset($_SERVER['HTTP_CONTENT_LENGTH']) ? $_SERVER['HTTP_CONTENT_LENGTH'] : NULL);

    //Get user agent
    $this->user_agent = isset($_SERVER['HTTP_USER_AGENT']) ? $_SERVER['HTTP_USER_AGENT'] : NULL;

    //Get http referer
    $this->referrer = isset($_SERVER['HTTP_REFERER']) ? $_SERVER['HTTP_REFERER'] : NULL;


    /**
    * Request Body
    * Get Request body information.
    */
    $this->body = file_get_contents('php://input');


    /**
    * Server Information
    * Get web server information.
    */
    //Get hostname
    $this->hostname = isset($_SERVER['SERVER_NAME']) ? $_SERVER['SERVER_NAME'] : NULL;

    //Get host
    $this->host = isset($_SERVER['HTTP_HOST']) ? $_SERVER['HTTP_HOST'] : NULL;

    //Get port
    $this->port = isset($_SERVER['SERVER_PORT']) ? $_SERVER['SERVER_PORT'] : NULL;

    //Get server gateway interface
    $this->gateway_interface = isset($_SERVER['GATEWAY_INTERFACE']) ? $_SERVER['GATEWAY_INTERFACE'] : NULL;

    //Get server addr
    $this->server_addr = isset($_SERVER['SERVER_ADDR']) ? $_SERVER['SERVER_ADDR'] : NULL;

    //Get server name
    $this->server_name = isset($_SERVER['SERVER_NAME']) ? $_SERVER['SERVER_NAME'] : NULL;

    //Get server software
    $this->server_software = isset($_SERVER['SERVER_SOFTWARE']) ? $_SERVER['SERVER_SOFTWARE'] : NULL;

    //Get server protocol
    $this->server_protocol = isset($_SERVER['SERVER_PROTOCOL']) ? $_SERVER['SERVER_PROTOCOL'] : NULL;

    //Get server signature
    $this->server_signature = isset($_SERVER['SERVER_SIGNATURE']) ? $_SERVER['SERVER_SIGNATURE'] : NULL;

    //Get server document root
    $this->document_root = isset($_SERVER['DOCUMENT_ROOT']) ? $_SERVER['DOCUMENT_ROOT'] : NULL;


    /**
    * Path Information
    * Get all urls and path information.
    */
    //Get request URI
    $this->uri = $_SERVER['REQUEST_URI'];

    //Get site full url
    $this->url = $this->scheme.'://'.$_SERVER['HTTP_HOST'].$_SERVER['REQUEST_URI'];

    //Get request path without query string.
    $this->path = isset($_SERVER['PATH_INFO']) ? $_SERVER['PATH_INFO'] : parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);


    /**
    * Request Information
    * Get all Http request information.
    */
    //Get all request data
    parse_str(file_get_contents('php://input'), $request_data);
    $request_data = (object) $request_data;
    $this->get = (object) $_GET;
    $this->post = (object) $_POST;
    $this->put = ($this->method === 'PUT' && isset($request_data)) ? $request_data : NULL;
    $this->delete = ($this->method === 'DELETE' && isset($request_data)) ? $request_data : NULL;
    $this->patch = ($this->method === 'PATCH' && isset($request_data)) ? $request_data : NULL;
    $this->head = ($this->method === 'HEAD' && isset($request_data)) ? $request_data : NULL;
    $this->options = ($this->method === 'OPTIONS' && isset($request_data)) ? $request_data : NULL;
    $this->connect = ($this->method === 'CONNECT' && isset($request_data)) ? $request_data : NULL;
    $this->trace = ($this->method === 'TRACE' && isset($request_data)) ? $request_data : NULL;
    $this->copy = ($this->method === 'COPY' && isset($request_data)) ? $request_data : NULL;
    $this->link = ($this->method === 'LINK' && isset($request_data)) ? $request_data : NULL;
    $this->unlink = ($this->method === 'UNLINK' && isset($request_data)) ? $request_data : NULL;
    $this->lock = ($this->method === 'LOCK' && isset($request_data)) ? $request_data : NULL;
    $this->unlock = ($this->method === 'UNLOCK' && isset($request_data)) ? $request_data : NULL;
    $this->purge = ($this->method === 'PURGE' && isset($request_data)) ? $request_data : NULL;
    $this->propfind = ($this->method === 'PROPFIND' && isset($request_data)) ? $request_data : NULL;
    $this->view = ($this->method === 'VIEW' && isset($request_data)) ? $request_data : NULL;
    $this->any = isset($request_data) ? $request_data : NULL;

    //Check connection is secure
    $this->is_secure = (isset($_SERVER['HTTPS']) && ($_SERVER['HTTPS'] === 'on' || $_SERVER['HTTPS'] === 1)) || (isset($_SERVER['HTTP_X_FORWARDED_PROTO']) && $_SERVER['HTTP_X_FORWARDED_PROTO'] === 'https') || (isset($_SERVER['HTTP_FRONT_END_HTTPS']) && strtolower($_SERVER['HTTP_FRONT_END_HTTPS'])!== 'off') ? TRUE : FALSE;

    //Check request made with ajax
    $this->is_ajax = isset($_SERVER['HTTP_X_REQUESTED_WITH']) && $_SERVER['HTTP_X_REQUESTED_WITH'] === 'XMLHttpRequest' ? TRUE : FALSE;

    //Check request method is get or not
    $this->is_get = $this->method === 'GET' ? TRUE : FALSE;

    //Check request method is post or not
    $this->is_post = $this->method === 'POST' ? TRUE : FALSE;

    //Check request method is put or not
    $this->is_put = $this->method === 'PUT' ? TRUE : FALSE;

    //Check request method is delete or not
    $this->is_delete = $this->method === 'DELETE' ? TRUE : FALSE;

    //Check request method is patch or not
    $this->is_patch = $this->method === 'PATCH' ? TRUE : FALSE;

    //Check request method is head or not
    $this->is_head = $this->method === 'HEAD' ? TRUE : FALSE;

    //Check request method is options or not
    $this->is_options = $this->method === 'OPTIONS' ? TRUE : FALSE;

    //Check request method is connect or not
    $this->is_connect = $this->method === 'CONNECT' ? TRUE : FALSE;

    //Check request method is trace or not
    $this->is_trace = $this->method === 'TRACE' ? TRUE : FALSE;

    //Check request method is copy or not
    $this->is_copy = $this->method === 'COPY' ? TRUE : FALSE;

    //Check request method is link or not
    $this->is_link = $this->method === 'LINK' ? TRUE : FALSE;

    //Check request method is unlink or not
    $this->is_unlink = $this->method === 'UNLINK' ? TRUE : FALSE;

    //Check request method is lock or not
    $this->is_lock = $this->method === 'LOCK' ? TRUE : FALSE;

    //Check request method is unlock or not
    $this->is_unlock = $this->method === 'UNLOCK' ? TRUE : FALSE;

    //Check request method is purge or not
    $this->is_purge = $this->method === 'PURGE' ? TRUE : FALSE;

    //Check request method is propfind or not
    $this->is_propfind = $this->method === 'PROPFIND' ? TRUE : FALSE;

    //Check request method is view or not
    $this->is_view = $this->method === 'VIEW' ? TRUE : FALSE;

    //Check protocol https or not
    $this->is_http = $this->scheme === 'http' ? TRUE : FALSE;

    //Check protocol is http or not
    $this->is_https = $this->scheme === 'https' ? TRUE : FALSE;

    //Get files data
    $this->files = new FileHandler();

    //Get session data
    $this->session = new Session();

    //Get cookie data
    $this->cookie = new Cookie();


    /**
    * User Information
    * Get User web browser information.
    */
    //Get remote ip address
    $this->remote_addr = isset($_SERVER['REMOTE_ADDR']) ? $_SERVER['REMOTE_ADDR'] : NULL;

    //Check request is redirected or not
    $this->is_referred = isset($_SERVER['HTTP_REFERER']) ? TRUE : FALSE;

  }

  /**
  * Get server all informarion
  *
  * @param string $server_index
  * @return string|void
  */
  public function server(string $server_index) {
    $server_index = strtoupper($server_index);
    if(isset($_SERVER[$server_index])) {
      return $_SERVER[$server_index];
    }
  }
}
