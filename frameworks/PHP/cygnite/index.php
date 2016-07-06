<?php
/*
 * This file is part of the Cygnite package.
 *
 * (c) Sanjoy Dey <dey.sanjoy0@gmail.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/*
 * This index file is entry point of the framework,
 * We will define the paths here to start bootstraping
 *
 * @author Sanjoy Dey <dey.sanjoy0@gmail.com>
 */

/*
 | ---------------------------------------------------------------
 | Define Directory Separator
 * ---------------------------------------------------------------
 */
define('DS', DIRECTORY_SEPARATOR);

/**
|---------------------------------------------------------------
| Define PHP file extension
| ---------------------------------------------------------------
*/
defined('EXT') or define('EXT', '.php');

/*---------------------------------------------------------------
| Now that we know the path, set the main path constants
| path to the packages folder.
| ---------------------------------------------------------------
*/
defined('CF_SYSTEM') or define('CF_SYSTEM', 'cygnite-package');

defined('CF_BOOTSTRAP') or define('CF_BOOTSTRAP', 'boot');

/* --------------------------------------------------------------
| Define application folder name
| ---------------------------------------------------------------
*/
defined('APPPATH') or define('APPPATH', 'apps');
//chdir(dirname(__DIR__));

/* --------------------------------------------------------------
| Define `root` directory name
| ---------------------------------------------------------------
*/
$dir = explode(DS, dirname(__FILE__));
defined('ROOTDIR') or define('ROOTDIR', rtrim(end($dir)));
defined('CYGNITE_BASE') or define('CYGNITE_BASE', dirname(__FILE__));

/* ---------------------------------------------------------------
| We have defined path, lets start booting by including start.php
| ----------------------------------------------------------------
*/
require_once CF_BOOTSTRAP.DS.'start'.EXT;
