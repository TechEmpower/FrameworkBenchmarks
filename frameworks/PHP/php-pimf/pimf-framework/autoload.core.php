<?php
// @codingStandardsIgnoreFile
// @codeCoverageIgnoreStart
// this is an auto-generated file, please do not edit!
spl_autoload_register(
  function ($class) {
    static $classes = null;
    if ($classes === null) {
      $classes = array(
        'Pimf\\Application'                  => '/Pimf/Application.php',
        'Pimf\\Cache'                        => '/Pimf/Cache.php',
        'Pimf\\Cache\\Storages\\Apc'         => '/Pimf/Cache/Storages/Apc.php',
        'Pimf\\Cache\\Storages\\Dba'         => '/Pimf/Cache/Storages/Dba.php',
        'Pimf\\Cache\\Storages\\File'        => '/Pimf/Cache/Storages/File.php',
        'Pimf\\Cache\\Storages\\Memcached'   => '/Pimf/Cache/Storages/Memcached.php',
        'Pimf\\Cache\\Storages\\Memory'      => '/Pimf/Cache/Storages/Memory.php',
        'Pimf\\Cache\\Storages\\Database'    => '/Pimf/Cache/Storages/Database.php',
        'Pimf\\Cache\\Storages\\Redis'       => '/Pimf/Cache/Storages/Redis.php',
        'Pimf\\Cache\\Storages\\Storage'     => '/Pimf/Cache/Storages/Storage.php',
        'Pimf\\Cache\\Storages\\Wincache'    => '/Pimf/Cache/Storages/Wincache.php',
        'Pimf\\Cache\\Storages\\Pdo'         => '/Pimf/Cache/Storages/Pdo.php',
        'Pimf\\Cli'                          => '/Pimf/Cli.php',
        'Pimf\\Cli\\Std'                     => '/Pimf/Cli/Std.php',
        'Pimf\\Contracts\\Arrayable'         => '/Pimf/Contracts/Arrayable.php',
        'Pimf\\Contracts\\Cleanable'         => '/Pimf/Contracts/Cleanable.php',
        'Pimf\\Contracts\\Jsonable'          => '/Pimf/Contracts/Jsonable.php',
        'Pimf\\Contracts\\MessageProvider'   => '/Pimf/Contracts/MessageProvider.php',
        'Pimf\\Contracts\\Renderable'        => '/Pimf/Contracts/Renderable.php',
        'Pimf\\Contracts\\Reunitable'        => '/Pimf/Contracts/Reunitable.php',
        'Pimf\\Controller\\Base'             => '/Pimf/Controller/Base.php',
        'Pimf\\Controller\\Core'             => '/Pimf/Controller/Core.php',
        'Pimf\\Controller\\Exception'        => '/Pimf/Controller/Exception.php',
        'Pimf\\Cookie'                       => '/Pimf/Cookie.php',
        'Pimf\\DataMapper\\Base'             => '/Pimf/DataMapper/Base.php',
        'Pimf\\EntityManager'                => '/Pimf/EntityManager.php',
        'Pimf\\Environment'                  => '/Pimf/Environment.php',
        'Pimf\\Error'                        => '/Pimf/Error.php',
        'Pimf\\Event'                        => '/Pimf/Event.php',
        'Pimf\\Logger'                       => '/Pimf/Logger.php',
        'Pimf\\Memcached'                    => '/Pimf/Memcached.php',
        'Pimf\\Model\\AsArray'               => '/Pimf/Model/AsArray.php',
        'Pimf\\Param'                        => '/Pimf/Param.php',
        'Pimf\\Database'                     => '/Pimf/Database.php',
        'Pimf\\Pdo\\Connector'               => '/Pimf/Pdo/Connector.php',
        'Pimf\\Pdo\\Factory'                 => '/Pimf/Pdo/Factory.php',
        'Pimf\\Pdo\\Mysql'                   => '/Pimf/Pdo/Mysql.php',
        'Pimf\\Pdo\\Postgre'                 => '/Pimf/Pdo/Postgre.php',
        'Pimf\\Pdo\\Sqlite'                  => '/Pimf/Pdo/Sqlite.php',
        'Pimf\\Pdo\\Sqlserver'               => '/Pimf/Pdo/Sqlserver.php',
        'Pimf\\Redis'                        => '/Pimf/Redis.php',
        'Pimf\\Registry'                     => '/Pimf/Registry.php',
        'Pimf\\Request'                      => '/Pimf/Request.php',
        'Pimf\\Response'                     => '/Pimf/Response.php',
        'Pimf\\Resolver'                     => '/Pimf/Resolver.php',
        'Pimf\\Route'                        => '/Pimf/Route.php',
        'Pimf\\Route\\Target'                => '/Pimf/Route/Target.php',
        'Pimf\\Router'                       => '/Pimf/Router.php',
        'Pimf\\Resolver\\Exception'          => '/Pimf/Resolver/Exception.php',
        'Pimf\\Sapi'                         => '/Pimf/Sapi.php',
        'Pimf\\Session'                      => '/Pimf/Session.php',
        'Pimf\\Session\\Payload'             => '/Pimf/Session/Payload.php',
        'Pimf\\Session\\Storages\\Apc'       => '/Pimf/Session/Storages/Apc.php',
        'Pimf\\Session\\Storages\\Cookie'    => '/Pimf/Session/Storages/Cookie.php',
        'Pimf\\Session\\Storages\\Dba'       => '/Pimf/Session/Storages/Dba.php',
        'Pimf\\Session\\Storages\\File'      => '/Pimf/Session/Storages/File.php',
        'Pimf\\Session\\Storages\\Memcached' => '/Pimf/Session/Storages/Memcached.php',
        'Pimf\\Session\\Storages\\Memory'    => '/Pimf/Session/Storages/Memory.php',
        'Pimf\\Session\\Storages\\Database'  => '/Pimf/Session/Storages/Database.php',
        'Pimf\\Session\\Storages\\Redis'     => '/Pimf/Session/Storages/Redis.php',
        'Pimf\\Session\\Storages\\Storage'   => '/Pimf/Session/Storages/Storage.php',
        'Pimf\\Session\\Storages\\Pdo'       => '/Pimf/Session/Storages/Pdo.php',
        'Pimf\\Uri'                          => '/Pimf/Uri.php',
        'Pimf\\Url'                          => '/Pimf/Url.php',
        'Pimf\\Util\\Dom'                    => '/Pimf/Util/Dom.php',
        'Pimf\\Util\\File'                   => '/Pimf/Util/File.php',
        'Pimf\\Util\\Header'                 => '/Pimf/Util/Header.php',
        'Pimf\\Util\\Header\\ResponseStatus' => '/Pimf/Util/Header/ResponseStatus.php',
        'Pimf\\Util\\Header\\ContentType'    => '/Pimf/Util/Header/ContentType.php',
        'Pimf\\Util\\Identifier'             => '/Pimf/Util/Identifier.php',
        'Pimf\\Util\\IdentityMap'            => '/Pimf/Util/IdentityMap.php',
        'Pimf\\Util\\Json'                   => '/Pimf/Util/Json.php',
        'Pimf\\Util\\Message'                => '/Pimf/Util/Message.php',
        'Pimf\\Util\\Serializer'             => '/Pimf/Util/Serializer.php',
        'Pimf\\Util\\String'                 => '/Pimf/Util/String.php',
        'Pimf\\Util\\String\\Clean'          => '/Pimf/Util/String/Clean.php',
        'Pimf\\Util\\Uploaded'               => '/Pimf/Util/Uploaded.php',
        'Pimf\\Util\\Uploaded\\Factory'      => '/Pimf/Util/Uploaded/Factory.php',
        'Pimf\\Util\\Uuid'                   => '/Pimf/Util/Uuid.php',
        'Pimf\\Util\\Validator'              => '/Pimf/Util/Validator.php',
        'Pimf\\Util\\Validator\\Factory'     => '/Pimf/Util/Validator/Factory.php',
        'Pimf\\Util\\Xml'                    => '/Pimf/Util/Xml.php',
        'Pimf\\View'                         => '/Pimf/View.php',
        'Pimf\\View\\Haanga'                 => '/Pimf/View/Haanga.php',
        'Pimf\\View\\Json'                   => '/Pimf/View/Json.php',
        'Pimf\\View\\Twig'                   => '/Pimf/View/Twig.php'
      );
    }

    if (isset($classes[$class])) {
      require __DIR__ . '/core' . $classes[$class];
    }

    return false;
  }
);
// @codeCoverageIgnoreEnd
