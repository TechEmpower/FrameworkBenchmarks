<?php
/**
 * Controller
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Controller;

use Pimf\Registry, Pimf\Util\String, Pimf\Cli\Std, Pimf\Pdo\Factory, \Pimf\Controller\Exception as Bomb, Pimf\Util\File;

/**
 * @package Controller
 * @author  Gjero Krsteski <gjero@krsteski.de>
 * @codeCoverageIgnore
 */
class Core extends Base
{
  /**
   * Because it is a PIMF restriction!
   */
  public function indexAction()
  {

  }

  /**
   * Checks the applications architecture and creates some security and safety measures.
   */
  public function initCliAction()
  {
    clearstatcache();

    $conf = Registry::get('conf');
    $app  = 'app/' . $conf['app']['name'] . '/';

    $assets = array(
      BASE_PATH . $app . '_session/',
      BASE_PATH . $app . '_cache/',
      BASE_PATH . $app . '_database/',
      BASE_PATH . $app . '_templates/',
    );

    echo 'Check app assets' . PHP_EOL;

    foreach ($assets as $asset) {

      if (!is_dir($asset)) {
        echo "[ Please create '$asset' directory! ]" . PHP_EOL;
      }

      if (!is_writable($asset)) {
        echo "[ Please make '$asset' writable! ]" . PHP_EOL;
      }
    }

    echo 'Secure root directory' . PHP_EOL;
    chmod(BASE_PATH, 0755);

    echo 'Secure .htaccess' . PHP_EOL;
    chmod(BASE_PATH . '.htaccess', 0644);

    echo 'Secure index.php' . PHP_EOL;
    chmod(BASE_PATH . 'index.php', 0644);

    echo 'Secure autoload.core.php' . PHP_EOL;
    chmod(BASE_PATH . 'pimf-framework/autoload.core.php', 0644);

    echo 'Create logging files' . PHP_EOL;
    $handle = fopen($file = $conf['bootstrap']['local_temp_directory'] . 'pimf-logs.txt', "at+");
    fclose($handle);
    chmod($file, 0777);
    $handle = fopen($file = $conf['bootstrap']['local_temp_directory'] . 'pimf-warnings.txt', "at+");
    fclose($handle);
    chmod($file, 0777);
    $handle = fopen($file = $conf['bootstrap']['local_temp_directory'] . 'pimf-errors.txt', "at+");
    fclose($handle);
    chmod($file, 0777);

    clearstatcache();
  }

  public function create_session_tableCliAction()
  {
    $std  = new Std();
    $type = $std->read('database type [mysql|sqlite]', '(mysql|sqlite)');

    var_dump(
      $this->createTable($type, 'session')
    );
  }

  public function create_cache_tableCliAction()
  {
    $std  = new Std();
    $type = $std->read('database type [mysql|sqlite]', '(mysql|sqlite)');

    var_dump(
      $this->createTable($type, 'cache')
    );
  }

  /**
   * @param string $type
   * @param string $for
   *
   * @return bool
   * @throws \DomainException
   */
  protected function createTable($type, $for)
  {
    $type = trim($type);

    try {
      $pdo = $file = null;

      $conf = Registry::get('conf');

      switch ($for) {
        case 'cache':
          $pdo  = Factory::get($conf['cache']['database']);
          $file = 'create-cache-table-' . $type . '.sql';
          break;
        case 'session':
          $pdo  = Factory::get($conf['session']['database']);
          $file = 'create-session-table-' . $type . '.sql';
          break;
      }

      $file = str_replace('/', DS, BASE_PATH . 'pimf-framework/core/Pimf/_database/' . $file);

      return $pdo->exec(file_get_contents(new File($file))) or print_r($pdo->errorInfo(), true);

    } catch (\PDOException $pdoe) {
      throw new Bomb($pdoe->getMessage());
    }
  }
}
