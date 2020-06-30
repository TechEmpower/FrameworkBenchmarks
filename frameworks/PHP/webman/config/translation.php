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

/**
 * 多语言配置
 */
return [
    // 默认语言
    'locale' => 'zh_CN',
    // 回退语言
    'fallback_locale' => ['zh_CN', 'en'],
    // 语言文件存放的文件夹
    'path' => base_path() . '/resource/translations',
];