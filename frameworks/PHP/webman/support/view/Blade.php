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

use Jenssegers\Blade\Blade as BladeView;
use Webman\View;

/**
 * Class Blade
 * composer require jenssegers/blade
 * @package support\view
 */
class Blade implements View
{
    /**
     * @param $template
     * @param $vars
     * @param string $app
     * @return mixed
     */
    public static function render($template, $vars, $app = null)
    {
        static $balde;
        $balde = $balde ? : new BladeView(app_path(), runtime_path() . '/views');
        $app_name = $app == null ? request()->app : $app;
        if ($app_name === '') {
            $view_path = "view/$template";
        } else {
            $view_path = "$app_name/view/$template";
        }
        return $balde->render($view_path, $vars);
    }
}