<?php
/**
 * Util
 *
 * @copyright Copyright (c) 2010-2013 Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Util;

/**
 * Represents a file in the file system and uses SplFileInfo a high-level object oriented interface to
 * information for an individual file.
 *
 * @package Util
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class File extends \SplFileInfo
{
  /**
   * Constructs a new file from the given path.
   *
   * @param string  $path  The path to the file
   * @param boolean $check Whether to check the path or not
   *
   * @throws \OutOfRangeException If the given path is not a file
   */
  public function __construct($path, $check = true)
  {
    if ($check && !is_file($path)) {
      throw new \OutOfRangeException("path $path is not a file");
    }

    parent::__construct($path);
  }

  /**
   * Returns the extension of the file.
   * SplFileInfo::getExtension() is not available before PHP 5.3.6
   *
   * @return string
   */
  public function getExtension()
  {
    return pathinfo($this->getBasename(), PATHINFO_EXTENSION);
  }

  /**
   * Moves the file to a new location.
   *
   * @param string $dir  The destination folder
   * @param string $name The new file name
   *
   * @return File A File object representing the new file
   * @throws \RuntimeException if the target file could not be created
   */
  public function move($dir, $name = null)
  {
    $target = $this->getTargetFile($dir, $name);

    if (!@rename($this->getPathname(), $target)) {
      $error = error_get_last();
      throw new \RuntimeException("Could not move the file {$this->getPathname()} to $target ({$error['message']})");
    }

    @chmod($target, 0666 & ~umask());

    return $target;
  }

  /**
   * @param string      $dir The destination folder
   * @param null|string $name
   *
   * @return File
   * @throws \RuntimeException
   */
  protected function getTargetFile($dir, $name = null)
  {
    if (!is_dir($dir) || false === @mkdir($dir, 0777, true)) {
        throw new \RuntimeException("The destination folder $dir");
    }

    if (!is_writable($dir)) {
      throw new \RuntimeException("Unable to write in the $dir directory");
    }

    return new self($dir . DS . (null === $name ? $this->getBasename() : $this->getName($name)), false);
  }

  /**
   * Returns locale independent base name of the given path.
   *
   * @param string $name The new file name
   *
   * @return string
   */
  protected function getName($name)
  {
    $originalName = str_replace('\\', '/', $name);
    $pos          = strrpos($originalName, '/');

    return (false === $pos) ? $originalName : substr($originalName, $pos + 1);
  }
}
