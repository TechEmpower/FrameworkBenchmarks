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
namespace support\bootstrap;

use Webman\Bootstrap;
use Symfony\Component\Translation\Translator;
use Symfony\Component\Translation\Loader\PhpFileLoader;

/**
 * Class Translation
 * @package support\bootstrap
 * @method static string trans(?string $id, array $parameters = [], string $domain = null, string $locale = null)
 * @method static void setLocale(string $locale)
 * @method static string getLocale()
 */
class Translation implements Bootstrap {

    /**
     * @var array
     */
    protected static $_translator = [];

    /**
     * @param \Workerman\Worker $worker
     * @return void
     */
    public static function start($worker)
    {
        if (!class_exists('\Symfony\Component\Translation\Translator')) {
            return;
        }
        $config = config('translation', []);
        static::$_translator = $translator = new Translator($config['locale']);
        $translator->addLoader('phpfile', new PhpFileLoader());
        $translator->setFallbackLocales($config['fallback_locale']);
        if (!$translations_path = realpath($config['path'])) {
            return;
        }
        foreach (glob($translations_path . DIRECTORY_SEPARATOR . '*' . DIRECTORY_SEPARATOR . '*.php') as $file) {
            $domain = basename($file, '.php');
            $dir_name = pathinfo($file, PATHINFO_DIRNAME);
            $locale = substr(strrchr($dir_name, DIRECTORY_SEPARATOR), 1);
            if ($domain && $locale) {
                $translator->addResource('phpfile', $file, $locale, $domain);
            }
        }
    }

    /**
     * @param $name
     * @param $arguments
     * @return mixed
     */
    public static function __callStatic($name, $arguments)
    {
        return static::$_translator->{$name}(... $arguments);
    }
}
