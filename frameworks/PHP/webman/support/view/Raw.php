<?php
/**
 * This file is part of webman.
 *
 * Licensed under The MIT License
 * For full copyright and license information, please see the MIT-LICENSE.txt
 * Redistributions of files must retain the above copyright notice.
 *
 * @author    walkor<walkor@workerman.net>
 * @copyright walkor<walkor@workerman.net>
 * @link      http://www.workerman.net/
 * @license   http://www.opensource.org/licenses/mit-license.php MIT License
 */
namespace support\view;

use Webman\View;

/**
 * Class Raw
 * @package support\view
 */
class Raw implements View
{
    public static function render($template, $vars, $app = null)
    {
        static $view_suffix;
        $view_suffix = $view_suffix ? : config('view.view_suffix', 'html');
        $app_name = $app == null ? request()->app : $app;
        if ($app_name === '') {
            $view_path = app_path() . "/view/$template.$view_suffix";
        } else {
            $view_path = app_path() . "/$app_name/view/$template.$view_suffix";
        }
        \extract($vars);
        \ob_start();
        // Try to include php file.
        try {
            include $view_path;
        } catch (\Throwable $e) {
            echo $e;
        }
        return \ob_get_clean();
    }
}