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

use think\Template;
use Webman\View;

/**
 * Class Blade
 * @package support\view
 */
class ThinkPHP implements View
{
    /**
     * @param $template
     * @param $vars
     * @param string $app
     * @return mixed
     */
    public static function render($template, $vars, $app = null)
    {
        static $view;
        $view = $view ? : new Template([
            'view_path'   => app_path().'/',
            'cache_path'  => runtime_path() . '/views/',
            'view_suffix' => config('view.view_suffix', 'html')
        ]);
        $app_name = $app == null ? request()->app : $app;
        if ($app_name === '') {
            $view_path = "view/$template";
        } else {
            $view_path = "$app_name/view/$template";
        }
        \ob_start();
        $view->fetch($view_path, $vars);
        $content = \ob_get_clean();
        return $content;
    }
}