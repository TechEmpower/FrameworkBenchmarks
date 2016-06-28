<?php

use Yaf\Exception;

/**
 * Multi-connection database manager
 *
 * @author Ilya Sabelnikov <fruit.dev@gmail.com>
 */
class DatabaseManager
{

    /**
     * @var DatabaseManager
     */
    protected static $instance = null;

    /**
     * Initialized database connections
     *
     * @var array
     */
    protected $connections = array();

    /**
     * All database connection parameters
     *
     * @var array
     */
    protected $definedConnections = array();

    private function __construct (array $definedConnections)
    {
        $this->definedConnections = $definedConnections;
    }

    /**
     * @param array $definedConnections
     * @return DatabaseManager
     */
    public static function getInstance ()
    {
        if ( ! isset(self::$instance)) {
            $definedConnections = Yaf\Application::app()->getConfig()->get('db.connections')->toArray();
            self::$instance = new self($definedConnections);
            register_shutdown_function(array(self::$instance, 'resetInstance'));
        }

        return self::$instance;
    }

    /**
     * Initialises database connection by given connection name
     *
     * @param string $connectionName
     * @return \PDO
     */
    public function getConnection ($connectionName = 'default')
    {
        if ( ! isset($this->connections[$connectionName])) {
            $this->connections[$connectionName] = $this->createPdoInstance($connectionName);
        }

        return $this->connections[$connectionName];
    }

    /**
     * Initialize connection defined in application.ini under db.connection area
     *
     * @param string $connectionName
     * @return \PDO
     */
    protected function createPdoInstance ($connectionName)
    {
        $options = $this->getConnectionOptions($connectionName);

        $params = array();

        if (isset($options['params']) && is_array($options['params'])) {
            foreach ($options['params'] as $key => $val) {
                $attributeName = '\\PDO::' . $key;

                if ( ! defined($attributeName)) {
                    throw new Exception(sprintf('Unknown PDO attribute "%s"', $attributeName));
                }

                $params[constant($attributeName)] = $val;
            }
        }

        return new PDO($options['dsn'], $options['user'], $options['pass'], $params);
    }

    /**
     * @param string $connectionName
     * @throws Exception
     * @return array
     */
    protected function getConnectionOptions ($connectionName)
    {
        if ( ! $this->hasConnectionOptions($connectionName)) {
            throw new Exception(sprintf('Unknown connection alias "%s"', $connectionName));
        }

        return $this->definedConnections[$connectionName];
    }

    /**
     * Check whether connection is defined within configuration file
     *
     * @param string $connectionName
     * @return boolean
     */
    protected function hasConnectionOptions ($connectionName)
    {
        return isset($this->definedConnections[$connectionName]);
    }

    public function closeConnections ()
    {
        foreach (array_keys($this->connections) as $connectionName) {
            $this->closeConnection($connectionName);
        }
    }

    public function closeConnection ($connectionName = 'default')
    {
        if ( ! isset($this->connections[$connectionName])) {
            return false;
        }

        unset($this->connections[$connectionName]);

        return true;
    }

    /**
     * Reset the internal static instance
     *
     * @return void
     */
    public static function resetInstance ()
    {
        if ( ! self::$instance) {
            return;
        }

        self::$instance->reset();
        self::$instance = null;
    }

    /**
     * Reset this instance of the manager
     *
     * @return void
     */
    public function reset ()
    {
        $this->closeConnections();

        $this->definedConnections   = array();
    }

}

