<?php
/**
* URL Dispatcher
* URL Dispatcher parse urls and match url routes.
*
* @package : URL Dispatcher
* @category : System
* @author : Unic Framework
* @link : https://github.com/unicframework/unic
*/

class UrlDispatcher {
  /**
  * Store urlpatterns array
  *
  * @var array
  */
  private $urlpatterns;

  function __construct($urlpatterns) {
    $this->urlpatterns = $this->url_parser($urlpatterns);
  }

  /**
  * URL Parser
  *
  * @param array $urlpatterns
  * @return array
  */
  private function url_parser($urlpatterns) : array {
    global $ignore_trailing_slash;
    $routes = array();
    //Parse urlpatterns
    if(is_array($urlpatterns)) {
      //Parse urlpatterns array
      $tmp_routes = $this->parse_urlpatterns($urlpatterns);
      //Parse url parameters
      foreach($tmp_routes as $url => $view) {
        if(preg_match_all('/{([^{]*)}/', $url, $matches)) {
          //Remove first data from array
          array_shift($matches);
          foreach($matches as $match) {
            $params = $match;
          }
        } else {
          $params = array();
        }
        //Convert wildcard patterns to RegEx
        $route = preg_replace(array('/{([^{]*)}/'), array('([^/]+)'), $url);
        //Ignore trailing slashes
        if($ignore_trailing_slash) {
          $route = rtrim($route, '/');
        }
        $routes[$route]['url'] = $route;
        $routes[$route]['view'] = $view;
        $routes[$route]['params'] = $params;
      }
    } else {
      http_response_code(500);
      exit('Error : invalid urlpatterns format');
    }
    return $routes;
  }


  /**
  * Parse urlpatterns
  * Parse application urlpatterns array.
  *
  * @param array $urlpatterns
  * @return array
  */
  private function parse_urlpatterns(array $urlpatterns) : array {
    $tmp_routes = array();
    foreach($urlpatterns as $url => $view) {
      //Parse group middlewares
      if(is_int($url)) {
        if(array_key_exists('view', $view)) {
          //Parse urlpatterns array
          $tmp_parsed_routes = $this->parse_urlpatterns($view['view']);
          //Add group middlewares to urls
          foreach($tmp_parsed_routes as $tmp_url => $tmp_view) {
            $route = $tmp_url;
            $tmp_routes[$route]['view'] = (is_array($tmp_view) ? $tmp_view['view'] : $tmp_view);
            if(isset($tmp_view['middleware'])) {
              if(is_array($tmp_view['middleware'])) {
                $tmp_routes[$route]['middleware'] = array_merge($tmp_view['middleware'], is_array($view['middleware']) ? $view['middleware'] : array($view['middleware']));
              } else {
                $tmp_routes[$route]['middleware'] = array_merge(array($tmp_view['middleware']), is_array($view['middleware']) ? $view['middleware'] : array($view['middleware']));
              }
            } else {
              $tmp_routes[$route]['middleware'] = $view['middleware'];
            }
          }
        } else {
          http_response_code(500);
          exit('Error : invalid urlpatterns format');
        }
      } else {
        //Parse urlpatterns array
        $url = ($url === '/' ? $url : '/'.ltrim($url, '/'));
        if(is_array($view)) {
          //Parse route middleware
          if(array_key_exists('view', $view)) {
            $route = $url;
            $tmp_routes[$route] = $view;
          } else {
            //Parse nested urlpatterns
            foreach($view as $nested_url => $view) {
              //Parse group middlewares
              if(is_int($nested_url)) {
                if(array_key_exists('view', $view)) {
                  //Parse urlpatterns array
                  $tmp_parsed_routes = $this->parse_urlpatterns($view['view']);
                  //Add group middlewares to urls
                  foreach($tmp_parsed_routes as $tmp_url => $tmp_view) {
                    $tmp_url = ($tmp_url === '/' ? $tmp_url : '/'.ltrim($tmp_url, '/'));
                    $route = ($url === '/' ? $tmp_url : rtrim($url, '/').$tmp_url);
                    $tmp_routes[$route]['view'] = (is_array($tmp_view) ? $tmp_view['view'] : $tmp_view);
                    if(isset($tmp_view['middleware'])) {
                      if(is_array($tmp_view['middleware'])) {
                        $tmp_routes[$route]['middleware'] = array_merge($tmp_view['middleware'], is_array($view['middleware']) ? $view['middleware'] : array($view['middleware']));
                      } else {
                        $tmp_routes[$route]['middleware'] = array_merge(array($tmp_view['middleware']), is_array($view['middleware']) ? $view['middleware'] : array($view['middleware']));
                      }
                    } else {
                      $tmp_routes[$route]['middleware'] = $view['middleware'];
                    }
                  }
                } else {
                  http_response_code(500);
                  exit('Error : invalid urlpatterns format');
                }
              } else {
                $nested_url = ($nested_url === '/' ? $nested_url : '/'.ltrim($nested_url, '/'));
                if(is_array($view)) {
                  //Parse route middlewares
                  if(array_key_exists('view', $view)) {
                    if(is_array($view['view'])) {
                      exit('Error : invalid urlpatterns format');
                    } else {
                      $route = ($url === '/' ? $nested_url : rtrim($url, '/').$nested_url);
                      $tmp_routes[$route] = $view;
                    }
                  } else {
                    http_response_code(500);
                    exit('Error : invalid urlpatterns format');
                  }
                } else {
                  $route = ($url === '/' ? $nested_url : rtrim($url, '/').$nested_url);
                  $tmp_routes[$route] = $view;
                }
              }
            }
          }
        } else {
          $route = $url;
          $tmp_routes[$route] = $view;
        }
      }
    }
    return $tmp_routes;
  }


  /**
  * Match Routes
  *
  * @param string $request
  * @return array
  */
  public function match_routes($request) : array {
    global $ignore_trailing_slash, $static_url, $static_dir;
    $route = array();
    //Ignore trailing slashes
    if($ignore_trailing_slash) {
      $request = rtrim($request, '/');
    }
    $tmp_static_dir = BASEPATH.'/application/'.trim($static_dir, '/');
    //Parse static urls and match static url
    if(!is_array($static_url)) {
      $tmp_static_url = rtrim($static_url, '/').'/(.*)';
      if(preg_match('#^'.$tmp_static_url.'$#', $request, $matches)) {
        if(file_exists($tmp_static_dir.'/'.$matches[1])) {
          $route[$matches[0]]['url'] = $matches[0];
          $route[$matches[0]]['file_path'] = $tmp_static_dir.'/'.$matches[1];
          $route[$matches[0]]['mime_type'] = $this->get_mime_type($tmp_static_dir.'/'.$matches[1]);
        }
      }
    }

    //Parse urlpatterns and match routes
    foreach($this->urlpatterns as $url) {
      if(preg_match('#^'.$url['url'].'$#', $request, $matches)) {
        //Remove first data from array
        $route[$matches[0]]['url'] = $matches[0];
        $route[$matches[0]]['view'] = $url['view'];
        $params = array();
        $i = 1;
        foreach($url['params'] as $param) {
          $params[$param] = $matches[$i];
          $i ++;
        }
        $route[$matches[0]]['params'] = $params;
      } else {
        $route[$url['url']]['url'] = $url['url'];
        $route[$url['url']]['view'] = $url['view'];
        $route[$url['url']]['params'] = $url['params'];
      }
    }
    if(array_key_exists($request, $route)) {
      return $route[$request];
    } else {
      return array();
    }
  }

  /**
  * Get Mime Type
  * get files mime type.
  *
  * @param string $file_path
  * @return string|boolean
  */
  private function get_mime_type(string $file_path) {
    //MIME Types
    $mime_types = array(
      'txt' => 'text/plain',
      'htm' => 'text/html',
      'html' => 'text/html',
      'php' => 'text/html',
      'css' => 'text/css',
      'js' => 'application/javascript',
      'json' => 'application/json',
      'xml' => 'application/xml',
      'swf' => 'application/x-shockwave-unic',
      'flv' => 'video/x-flv',

       //Images
      'png' => 'image/png',
      'jpe' => 'image/jpeg',
      'jpeg' => 'image/jpeg',
      'jpg' => 'image/jpeg',
      'gif' => 'image/gif',
      'bmp' => 'image/bmp',
      'ico' => 'image/vnd.microsoft.icon',
      'tiff' => 'image/tiff',
      'tif' => 'image/tiff',
      'svg' => 'image/svg+xml',
      'svgz' => 'image/svg+xml',

      //Archives
      'zip' => 'application/zip',
      'rar' => 'application/x-rar-compressed',
      'exe' => 'application/x-msdownload',
      'msi' => 'application/x-msdownload',
      'cab' => 'application/vnd.ms-cab-compressed',

      //Audio/Video
      'mp3' => 'audio/mpeg',
      'qt' => 'video/quicktime',
      'mov' => 'video/quicktime',

      //Adobe
      'pdf' => 'application/pdf',
      'psd' => 'image/vnd.adobe.photoshop',
      'ai' => 'application/postscript',
      'eps' => 'application/postscript',
      'ps' => 'application/postscript',

      //MS Office
      'doc' => 'application/msword',
      'rtf' => 'application/rtf',
      'xls' => 'application/vnd.ms-excel',
      'ppt' => 'application/vnd.ms-powerpoint',
      'docx' => 'application/msword',
      'xlsx' => 'application/vnd.ms-excel',
      'pptx' => 'application/vnd.ms-powerpoint',

      //Open Office
      'odt' => 'application/vnd.oasis.opendocument.text',
      'ods' => 'application/vnd.oasis.opendocument.spreadsheet',
    );

    $ext_array = explode('.', $file_path);
    $extension = strtolower(end($ext_array));
    if(isset($mime_types[$extension])) {
     return $mime_types[$extension];
    } else {
     return mime_content_type($file_path);
    }
  }
}
