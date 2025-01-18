<?php

declare(strict_types=1);

namespace Cyber;

use DI\Container;
use Cyber\Request;
use Cyber\Response;
use Cyber\Middleware;
use Cyber\Utility;
use PDO;
use Illuminate\Database\Capsule\Manager as EloquentDb;
use think\facade\Db as ThinkormDb;

class App
{
    /** App instance object */
    private static ?self $instance = null;
    /** Container instance */
    public Container $container;
    /** Application configuration */
    public array $config;
    public  $data;
    /** Request configuration */
    public Request $request;
    /** Response configuration */
    public Response $response;
    /** Middleware */
    public Middleware $middleware;
    
    /** Route configuration */
    public array $routes;
    /** Route manager */
    public Route $route;
    
    /** Application name */
    public string $appName;
    public $db;
    
    public $start_time;
    public $timestamps;
    /**
    * Constructor
    * Configuration check, initialization configuration, container
    */
    public function __construct($containerConfig = null)
    {
        $this->start_time = time();
        /* Check PHP environment version | extension */
        Utility::checkPHPenv();
        
        /* Build container instance */
        $this->container = new Container($containerConfig);
        
        /* Load route configuration */
        $routes = require_once $this->container->get('route_path');
        /* Create route manager */
        $this->route = $this->container->get('Route');
        /* Call route dispatcher */
        $this->route->dispatcher($routes);
        
        /* Configuration */
        $this->config = $this->container->get('config');
        /* Request object */
        $this->request = $this->container->get('Request');
        
        /* Response object */
        $this->response = $this->container->get('Response');
        
        /* Middleware */
        $this->middleware = $this->container->get('Middleware');
        
        /* Database */
        $this->db = $this->setDb();
        
    }
    /**
    * Run application
    */
    public function run()
    {
        $this->timestamps = time();
        /* cli mode maintains database connection */
        $this->cliMaintainDatabaseConnection($this->getConfig('orm'));
        
        /* Get application name */
        $this->appName = $this->request->getAppName();
        
        /* Request object middleware list */
        $requestMiddlewares = $this->getConfig('request_middleware');
        
        /* Execute request object middleware */
        $this->request = $this->middleware->handleRequest($requestMiddlewares);
        
        /* Parse route and return the closure to be executed */
        $handleRoute = $this->route->handleRoute();
        /* Middleware list */
        $Middlewares = $this->getConfig('middleware');
        /* Execute middleware */
        $response = $this->middleware->handle($Middlewares,function() use ($handleRoute) {
            return $handleRoute;
        });
        /* Return response */
        return $response;
    }
    
    // cli mode maintains database connection every 600 seconds
    public function cliMaintainDatabaseConnection($ormName)
    {
        if (php_sapi_name() === 'cli' and time() - $this->start_time > 600) {
            $this->start_time = time();
            if($ormName=='pdo'){
                // Close the existing connection and recreate the PDO instance
                $this->db = null;
                $this->db = new PDO(...$this->getConfig('pdo'));
            }elseif($ormName=='thinkorm'){
                // Close the existing connection and reconnect to Thinkorm
                $this->db::close();
                $this->db::connect('mysql',true);
            }
        }
    }
    public function setDb()
    {
        if($this->getConfig('orm')=='pdo'){
            return new PDO(...$this->getConfig('pdo'));
        }elseif($this->getConfig('orm')=='eloquent'){
            $EloquentDb = new EloquentDb;
            $EloquentDb->addConnection($this->getConfig('eloquent'));
            $EloquentDb->setAsGlobal();
            $EloquentDb->bootEloquent();
            return $EloquentDb;
        }elseif($this->getConfig('orm')=='thinkorm'){
            ThinkormDb::setConfig($this->getConfig('thinkorm'));
            return ThinkormDb::class;
        }
    }
    /**
    * Get the current application configuration
    * $app->getConfig();                         // Returns the entire configuration content of the current application
    * $app->getConfig('app_name');               // Get the value of ['app_name'] in the current application configuration
    * $app->getConfig('cookie.expires');         // Get the value of ['cookie']['expires']
    * $app->getConfig(null, 'admin');            // Returns the entire configuration content of the admin application
    * $app->getConfig('app_name', 'admin');      // Get the value of ['app_name'] in the admin application configuration
    * $app->getConfig('cookie.expires','admin'); // Get the value of ['cookie']['expires'] in the admin application configuration
    */
    public function getConfig($key = null, $appName = null): mixed
    {
        $appName = $appName ?? $this->appName ?? '';
        $config = $this->config[$appName] ?? null;
        // Get the entire application configuration
        if ($key === null) {
            return $config;
        }
        // Split the key into an array
        $keys = explode('.', $key);
        // Traverse the key array and get the configuration layer by layer
        foreach ($keys as $k) {
            if (is_array($config) && array_key_exists($k, $config)) {
                $config = $config[$k];
            } else {
                return null; // If a layer does not exist, return null
            }
        }
        return $config; // Return the final configuration value
    }
    /**
    * Initialize the application
    * @param string $bootstrap Configuration file
    * @return self
    */
    public static function bootstrap($bootstrap = null): self
    {
        if (!$bootstrap) {
            throw new \Exception('App::bootstrap parameter does not exist');
        }
        if (self::$instance === null) {
            /* Load container configuration file */
            if (!file_exists($bootstrap) || !is_readable($bootstrap)) {
                throw new \Exception("App::bootstrap parameter {$bootstrap} path error");
            }
            $containerConfig = require_once $bootstrap;
            self::$instance = new self($containerConfig);
            return self::$instance;
        }else{
            throw new \Exception('Application has started');
        }
    }
    /**
    * Get the application singleton instance
    * @return self
    */
    public static function getInstance(): self
    {
        if (self::$instance === null) {
            throw new \Exception('Application has not started');
        }
        return self::$instance;
    }
}
