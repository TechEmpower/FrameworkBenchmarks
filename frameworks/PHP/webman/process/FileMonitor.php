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

namespace process;

use Workerman\Timer;
use Workerman\Worker;

class FileMonitor
{
    /**
     * @var array
     */
    protected $_paths = [];

    /**
     * @var array
     */
    protected $_extensions = [];

    /**
     * FileMonitor constructor.
     * @param $monitor_dir
     * @param $monitor_extenstions
     */
    public function __construct($monitor_dir, $monitor_extenstions)
    {
        if (Worker::$daemonize) {
            return;
        }
        $this->_paths = (array)$monitor_dir;
        $this->_extensions = $monitor_extenstions;
        Timer::add(1, function () {
            foreach ($this->_paths as $path) {
                $this->check_files_change($path);
            }
        });
    }

    /**
     * @param $monitor_dir
     */
    public function check_files_change($monitor_dir)
    {
        static $last_mtime;
        if (!$last_mtime) {
            $last_mtime = time();
        }

        clearstatcache();

        if (!is_dir($monitor_dir)) {
            return;
        }
        // recursive traversal directory
        $dir_iterator = new \RecursiveDirectoryIterator($monitor_dir);
        $iterator = new \RecursiveIteratorIterator($dir_iterator);
        foreach ($iterator as $file) {
            /** var SplFileInfo $file */
            if (is_dir($file)) {
                continue;
            }
            // check mtime
            if ($last_mtime < $file->getMTime() && in_array($file->getExtension(), $this->_extensions)) {
                $var = 0;
                exec("php -l " . $file, $out, $var);
                if ($var) {
                    $last_mtime = $file->getMTime();
                    continue;
                }
                echo $file . " update and reload\n";
                // send SIGUSR1 signal to master process for reload
                posix_kill(posix_getppid(), SIGUSR1);
                $last_mtime = $file->getMTime();
                break;
            }
        }
    }
}