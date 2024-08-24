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
namespace support\bootstrap\db;

use Webman\Bootstrap;
use Workerman\Worker;
use PDOStatement;
use PDO;

/**
 * Class Raw
 * @package support\bootstrap\db
 */
class Raw implements Bootstrap
{

    public static PDO $pdo;

    public static PDOStatement $fortune;

    public static PDOStatement $random;

    public static PDOStatement $update;

    /**
     * @param Worker $worker
     *
     * @return void
     */
    public static function start($worker)
    {
        $pdo = new PDO('pgsql:host=tfb-database;dbname=hello_world',
            'benchmarkdbuser', 'benchmarkdbpass',
            [PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC,
             PDO::ATTR_EMULATE_PREPARES    => false]
        );
        self::$random    = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id=?');
        self::$fortune   = $pdo->prepare('SELECT id,message FROM Fortune');
        self::$update    = $pdo->prepare('UPDATE World SET randomNumber=? WHERE id=?');
        self::$pdo = $pdo;
    }
}
