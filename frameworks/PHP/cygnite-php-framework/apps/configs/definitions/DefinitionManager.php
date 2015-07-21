<?php
namespace Apps\Configs\Definitions;

if (!defined('CF_SYSTEM')) {
    exit('External script access not allowed');
}
/**
 *  Cygnite Framework
 *  Database Configuration Settings
 *
 *  An open source application development framework for PHP 5.3x or newer
 *
 *   License
 *
 *   This source file is subject to the MIT license that is bundled
 *   with this package in the file LICENSE.txt.
 *   http://www.cygniteframework.com/license.txt
 *   If you did not receive a copy of the license and are unable to
 *   obtain it through the world-wide-web, please send an email
 *   to sanjoy@hotmail.com so I can send you a copy immediately.
 *
 *@package               : Apps
 *@subpackages           : Configurations
 *@filename              : DefinitionManager.php
 *@description           : Define all your property dependencies. Cygnite will
 *                         inject your dependency at run time.
 *@author                : Sanjoy Dey
 *@copyright             : Copyright (c) 2013 - 2014,
 *@link	                 : http://www.cygniteframework.com
 *@since	             : Version 1.2
 *@filesource
 *
 */

 class DefinitionManager
 {

   /**
      * Set controller property dependencies here.
      * Cygnite will inject all your dependencies at runtime
      *
      * @return array
      *  <code>
      *   return
      *     array(
      *          'HomeController' => array(
      *              'service' => 'apps.extensions.general',
      *              'api' => 'apps.extensions.api'
      *           ),
      *          'ProductsController' => array(
      *             'social' => 'apps.extensions.social-share',
      *          ),
      *      );
      *  </code>
      *
      */
     public function getPropertyDependencies()
     {
        return array();
     }

     /**
      *
      * @return type
       *  <code>
      *   return
      *     array(
      *          'GeneralInterface' => '\\Apps\\Extensions\\General',
      *          'ORM' => '\\Cygnite\\Database\\ActiveRecord',
      *      );
      *  </code>
      */
     public function registerAlias()
     {
         return array();
     }
 }