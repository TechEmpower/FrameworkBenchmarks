<?php
/**
* Router
* Unic framework system router.
*
* @package : Router
* @category : System
* @author : Unic Framework
* @link : https://github.com/unicframework/unic
*/

class Router {
  /**
  * URLs Dispatcher object
  *
  * @var object
  */
  private $url_dispatcher;

  function __construct() {
    global $urlpatterns;
    if(!isset($urlpatterns) || !is_array($urlpatterns)) {
      $urlpatterns = array();
    }
    $this->url_dispatcher = new UrlDispatcher($urlpatterns);
  }

  /**
  * Handle Request
  * Handle Http request and render views.
  *
  * @return void
  */
  function handle_request() {
    global $request, $middlewares, $allowed_hosts;

    $request = new Request();

    //Run global middlewares
    if(!isset($middlewares)) {
      $middlewares = array();
    }
    $this->run_middleware($middlewares);

    //Parse allowed hosts
    if(isset($allowed_hosts)) {
      $allowed_hosts = is_array($allowed_hosts) ? $allowed_hosts : array($allowed_hosts);
    } else {
      $allowed_hosts = array();
    }

    //Verify allowed hosts
    if(empty($allowed_hosts) || in_array($request->hostname, $allowed_hosts)) {
      //Match url routes
      $route = $this->url_dispatcher->match_routes($request->path);
      if(!empty($route)) {
        //Serve static files
        if(array_key_exists('file_path', $route)) {
          header('Content-Type: '.$route['mime_type']);
          header('Content-Length: '.$this->get_filesize($route['file_path']));
          readfile($route['file_path']);
        } else {
          $request->params = (object) $route['params'];
          //Check route middleware
          if(is_array($route['view'])) {
            //Run route/group middleware
            if(isset($route['view']['middleware'])) {
              $this->run_middleware($route['view']['middleware']);
            }
            //Run view
            $this->run_view($route['view']['view']);
          } else {
            //Run view
            $this->run_view($route['view']);
          }
        }
      } else {
        raise('404');
      }
    } else {
      raise('403');
    }
  }

  /**
  * Load Middleware
  * Load application middleware.
  *
  * @param array $middlewares
  * @return void
  */
  private function run_middleware($middlewares) {
    global $request;
    if(!is_array($middlewares)) {
      $middlewares = array($middlewares);
    }
    //Load middlewares
    if(!empty($middlewares)) {
      foreach($middlewares as $middleware) {
        if(is_array($middleware)) {
          exit("Error : 'middleware' invalid format");
        } else {
          //Get middleware name
          $middleware_name = basename($middleware);

          //Get views class name and method name
          list($class, $method)=explode('.', $middleware_name);
          //Get middleware path
          $file_path = trim(dirname($middleware), '/').'/'.$class.'.php';
          //Check middleware exists or not
          if(file_exists(BASEPATH.'/application/'.$file_path)) {
            require_once(BASEPATH.'/application/'.$file_path);
            //Check view class exists or not
            if(class_exists($class)) {
              //Create view object
              $class_object = new $class($request);
              //Check in views class method exists or not
              if(isset($method)) {
                if(method_exists($class_object, $method)) {
                  //Execute view method
                  $class_object->$method($request);
                } else {
                  http_response_code(500);
                  exit("Error : '$method' middleware not found");
                }
              }
            } else {
              http_response_code(500);
              exit("Error : '$class' middleware not found");
            }

          } else {
            http_response_code(500);
            exit("Error : '$file_path' middleware file not found");
          }
        }
      }
    }
  }

  /**
  * Load Views
  * Load application views.
  *
  * @param string $view
  * @return void
  */
  private function run_view(string $view) {
    global $request;
    //Get views class name and method name
    list($class, $method)=explode('.', $view);
    //Check view class exists or not
    if(class_exists($class)) {
      //Create view object
      $class_object = new $class($request);
      //Check in views class method exists or not
      if(method_exists($class_object, $method)) {
        //Execute view method
        $class_object->$method($request);
      } else {
        http_response_code(500);
        exit("Error : '$method' view not found");
      }
    } else {
      http_response_code(500);
      exit("Error : '$class' view class not found");
    }
  }

  /**
  * Get Filesize
  * get file size of any file. it support larger then 4 GB file size.
  *
  * @param string $file_path
  * @return integer
  */
  private function get_filesize(string $file_path) {
    $size = filesize($file_path);
    if ($size < 0) {
      if (!(strtoupper(substr(PHP_OS, 0, 3))=='WIN')) {
        $size = trim(`stat -c%s $file_path`);
      } else {
        $fsobj = new COM("Scripting.FileSystemObject");
        $f = $fsobj->GetFile($file_path);
        $size = $f->Size;
      }
    }
    return $size;
  }
}
