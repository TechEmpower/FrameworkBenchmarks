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

use Twig\Loader\FilesystemLoader;
use Twig\Environment;
use Webman\View;

/**
 * Class Blade
 * @package support\view
 */
class Twig implements View
{
    /**
     * @param $template
     * @param $vars
     * @param string $app
     * @return mixed
     */
    public static function render($template, $vars, $app = null)
    {
        static $view, $view_suffix;
        $view = $view ? : new Environment(new FilesystemLoader(app_path()));
        $view_suffix = $view_suffix ? : config('view.view_suffix', 'html');
        $app_name = $app == null ? request()->app : $app;
        if ($app_name === '') {
            $view_path = "view/$template.$view_suffix";
        } else {
            $view_path = "$app_name/view/$template.$view_suffix";
        }
        return $view->render($view_path, $vars);
    }
}