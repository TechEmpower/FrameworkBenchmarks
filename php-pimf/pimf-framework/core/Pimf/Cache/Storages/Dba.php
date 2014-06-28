<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Cache\Storages;

/**
 * This class provides the functionality required to store
 * and retrieve PHP strings, integers or arrays.
 *
 * It uses the database (dbm-style) abstraction layer for persistence.
 * Even instances of SimpleXMLElement can be stored. You don't have
 * to matter about the size of the cache-file. It depends on the free
 * space of your disk.
 *
 * @package Cache_Storages
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Dba extends Storage
{
  /**
   * @var resource
   */
  protected $dba;

  /**
   * @var resource
   */
  protected $handler;

  /**
   * @var string
   */
  protected $file;

  /**
   * @param string  $file    the cache-file.
   *
   * @param string  $handler the dba handler.
   *
   * You have to install one of this handlers before use.
   *
   * cdb      = Tiny Constant Database - for reading.
   * cdb_make = Tiny Constant Database - for writing.
   * db4      = Oracle Berkeley DB 4   - for reading and writing.
   * qdbm     = Quick Database Manager - for reading and writing.
   * gdbm     = GNU Database Manager   - for reading and writing.
   * flatfile = default dba extension  - for reading and writing.
   *
   * Use flatfile-handler only when you cannot install one,
   * of the libraries required by the other handlers,
   * and when you cannot use bundled cdb handler.
   *
   * @param string  $mode    For read/write access, database creation if it doesn't currently exist.
   *
   * @param boolean $persistently
   *
   * @throws \RuntimeException If no DBA extension or handler installed.
   */
  public function __construct($file, $handler = 'flatfile', $mode = 'c', $persistently = true)
  {
    if (false === extension_loaded('dba')) {
      throw new \RuntimeException('The DBA extension is required for this wrapper, but the extension is not loaded');
    }

    if (false === in_array($handler, dba_handlers(false))) {
      throw new \RuntimeException('The ' . $handler . ' handler is required for the DBA extension, but the handler is not installed');
    }

    $this->dba = (true === $persistently) ? dba_popen($file, $mode, $handler) : dba_open($file, $mode, $handler);

    $this->file    = $file;
    $this->handler = $handler;
  }

  /**
   * Closes an open dba resource
   *
   * @return void
   */
  public function __destruct()
  {
    if ($this->dba) {
      @dba_close($this->dba);
      $this->dba = null;
    }
  }

  /**
   * @param string $key
   * @param mixed  $value
   * @param int    $minutes
   *
   * @return bool
   */
  public function put($key, $value, $minutes)
  {
    if ($minutes <= 0) {
      return;
    }

    $value = $this->expiration($minutes) . serialize($value);

    if (true === $this->has($key)) {
      return dba_replace($key, $value, $this->dba);
    }

    return dba_insert($key, $value, $this->dba);
  }

  /**
   * @param string $key
   * @param null $default
   *
   * @return bool|mixed|null
   */
  public function get($key, $default = null)
  {
    $res = $this->retrieve($key);

    if (false === $res) {
      $this->forget($key);

      return false;
    }

    return $res;
  }

  /**
   * @param string $key
   *
   * @return bool|mixed
   */
  protected function retrieve($key)
  {
    $value = dba_fetch($key, $this->dba);

    if (false === $value) {
      return false;
    }

    // compare the timestamp to the current time when we read the value.
    if (time() >= substr($value, 0, 10)) {
      return $this->forget($key);
    }

    return unserialize(substr($value, 10));
  }

  /**
   * @param string $key
   *
   * @return boolean
   */
  public function forget($key)
  {
    if (false === is_resource($this->dba)) {
      return false;
    }

    return dba_delete($key, $this->dba);
  }

  /**
   * @param string $key
   *
   * @return boolean
   */
  public function has($key)
  {
    return dba_exists($key, $this->dba);
  }

  /**
   * Write an item to the cache for five years.
   *
   * @param $key
   * @param $value
   *
   * @return boolean
   */
  public function forever($key, $value)
  {
    return $this->put($key, $value, 2628000);
  }

  /**
   * Cleans and optimizes the cache from all expired entries.
   *
   * @return bool
   */
  public function clean()
  {
    $dba = $this->dba;
    $key = dba_firstkey($dba);

    while ($key !== false && $key !== null) {
      $this->retrieve($key);
      $key = dba_nextkey($dba);
    }

    return dba_optimize($dba);
  }

  /**
   * Flush the whole storage.
   *
   * @return bool
   */
  public function flush()
  {
    if (file_exists($this->file)) {

      // We close the dba file before deleting
      // and reopen on next use.
      $this->__destruct();

      @unlink($this->file);

      clearstatcache();

      return true;
    }

    return false;
  }

  /**
   * @return string
   */
  public function getFile()
  {
    return $this->file;
  }
}
