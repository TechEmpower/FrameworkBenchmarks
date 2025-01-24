<?php

declare(strict_types=1);

namespace Cyber;

use DI\Container;
use Cyber\Request;
use Cyber\Response;
use Cyber\Middleware;
use Cyber\Utility;
use PDO;

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
    public $dbWorld;
    public $dbFortune;

    public $start_time;
    public $timestamps;
    /**
    * Constructor
    * Configuration check, initialization configuration, container
    */
    public function __construct($containerConfig = null)
    {
        $this->start_time = time();

        /* Build container instance */
        $this->container = new Container($containerConfig);

        /* Load route configuration */
        $routes = require $this->container->get('route_path');
        /* Create route manager */
        $this->route = $this->container->get('Route');
        /* Call route dispatcher */
        $this->route->dispatcher($routes);

        /* Configuration */
        $this->config = $this->container->get('config');
        /* Request object */
        $this->request = $this->container->get('Request');

        /* Database */
        $pdo =  new PDO(...$this->getConfig('pdo'));
        $this->db = $pdo;
        $this->dbWorld = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id=?');
        $this->dbFortune = $pdo->prepare('SELECT id,message FROM Fortune');

    }
    /**
    * Run application
    */
    public function run()
    {
        $this->timestamps = time();
        /* cli mode maintains database connection */
        if (php_sapi_name() === 'cli' and time() - $this->start_time > 1) {
            $this->start_time = time();
            $pdo =  new PDO(...$this->getConfig('pdo'));
            $this->db = $pdo;
            $this->dbWorld = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id=?');
            $this->dbFortune = $pdo->prepare('SELECT id,message FROM Fortune');
        }

        /* Return response */
        return $this->route->handleRoute();
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
