<?php
/**
 * KumbiaPHP web & app Framework.
 *
 * LICENSE
 *
 * This source file is subject to the new BSD license that is bundled
 * with this package in the file LICENSE.txt.
 * It is also available through the world-wide-web at this URL:
 * http://wiki.kumbiaphp.com/Licencia
 * If you did not receive a copy of the license and are unable to
 * obtain it through the world-wide-web, please send an email
 * to license@kumbiaphp.com so we can send you a copy immediately.
 *
 * @copyright  Copyright (c) 2005 - 2017 Kumbia Team (http://www.kumbiaphp.com)
 * @license    http://wiki.kumbiaphp.com/Licencia     New BSD License
 */

/**
 * Esta sección prepara el entorno
 * Todo esto se puede hacer desde la configuracion del
 * Servidor/PHP, en caso de no poder usarlo desde ahí
 * Puedes descomentar  estas lineas.
 */

//*Locale*
//setlocale(LC_ALL, 'es_ES');

//*Timezone*
//ini_set('date.timezone', 'America/New_York');

/**
 * @TODO
 * REVISAR ESTA SECCIÓN
 */
const APP_CHARSET = 'UTF-8';
/**
 * Indicar si la aplicacion se encuentra en produccion
 * directamente desde el index.php.
 */
const PRODUCTION = false;

/**
 * Descomentar para mostrar los errores.
 */
//error_reporting(E_ALL ^ E_STRICT);ini_set('display_errors', 'On');

/**
 * Define el APP_PATH.
 *
 * APP_PATH:
 * - Ruta al directorio de la aplicación (por defecto la ruta al directorio app)
 * - Esta ruta se utiliza para cargar los archivos de la aplicacion
 * - En producción, es recomendable ponerla manual
 */
//define('APP_PATH', dirname(__DIR__).'/app/');
const APP_PATH = '/kumbiaphp/bench/app/';
/**
 * Define el CORE_PATH.
 *
 * CORE_PATH:
 * - Ruta al directorio que contiene el núcleo de Kumbia (por defecto la ruta al directorio core)
 */
//define('CORE_PATH', dirname(dirname(APP_PATH)).'/vendor/Kumbia/core/');
const CORE_PATH = '/kumbiaphp/vendor/Kumbia/core/';
/**
 * Define el PUBLIC_PATH.
 *
 * PUBLIC_PATH:
 * - Path para genera la Url en los links a acciones y controladores
 * - Esta ruta la utiliza Kumbia como base para generar las Urls para acceder de lado de
 *   cliente (con el navegador web) y es relativa al DOCUMENT_ROOT del servidor web
 *
 *  EN PRODUCCION ESTA CONSTANTE DEBERÍA SER ESTABLECIDA MANUALMENTE
 */
//define('PUBLIC_PATH', substr($_SERVER['SCRIPT_NAME'], 0, -9)); // - index.php string[9]
const PUBLIC_PATH = '/';

 /**
  * Obtiene la url usando PATH_INFO.
  */
$url = $_SERVER['PATH_INFO'];

 /**
  * Obtiene la url usando $_GET['_url']
  * Cambiar también en el .htaccess.
  */
 //$url = $_GET['_url'] ?? '/';

/**
 * Carga el gestor de arranque
 * Por defecto el bootstrap del core.
 *
 * @see Bootstrap
 */
//require APP_PATH . 'libs/bootstrap.php'; //bootstrap de app
require CORE_PATH.'kumbia/bootstrap.php'; //bootstrap del core
