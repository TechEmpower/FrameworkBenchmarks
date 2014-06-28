<?php
/**
 * Util
 *
 * @copyright Copyright (c) 2010-2013 Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Util;

/**
 * A file uploaded through a form.
 *
 * <code>
 *
 *   // Create a file instance.
 *   $upload = new Uploaded(
 *     $_FILES['tmp_name'], $_FILES['name'], $_FILES['type'], $_FILES['size'], $_FILES['error']
 *   );
 *
 *   if ($upload instanceof Uploaded) {
 *     $upload->move('path/to/your/images/dir', $upload->getClientOriginalName());
 *   }
 *
 * </code>
 *
 * @package Util
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Uploaded extends File
{
  /**
   * Whether the test mode is activated.
   * Local files are used in test mode hence the code should not enforce HTTP uploads.
   *
   * @var bool
   */
  private $test = false;

  /**
   * The original name of the uploaded file.
   *
   * @var string
   */
  private $name;

  /**
   * The mime type provided by the uploader.
   *
   * @var string
   */
  private $mime;

  /**
   * The file size provided by the uploader.
   *
   * @var string
   */
  private $size;

  /**
   * The UPLOAD_ERR_XXX constant provided by the uploader.
   *
   * @var integer
   */
  private $error;

  /**
   * Accepts the information of the uploaded file as provided by the PHP global $_FILES.
   *
   * <code>
   *   // Create a file instance.
   *   $file = new Uploaded(
   *     $_FILES['tmp_name'], $_FILES['name'], $_FILES['type'], $_FILES['size'], $_FILES['error']
   *   );
   * </code>
   *
   * @param string      $path  The full temporary path to the file
   * @param string      $name  The original file name
   * @param string|null $mime  The type of the file as provided by PHP
   * @param string|null $size  The file size
   * @param int|null    $error The error constant of the upload (one of PHP's UPLOAD_ERR_XXX constants)
   * @param bool        $test  Whether the test mode is active
   *
   * @throws \RuntimeException If file_uploads is disabled
   */
  public function __construct($path, $name, $mime = null, $size = null, $error = null, $test = false)
  {
    if (!ini_get('file_uploads')) {
      throw new \RuntimeException('Unable to create file because "file_uploads" is disabled in your php.ini');
    }

    $this->name  = $this->getName($name);
    $this->mime  = $mime ? : 'application/octet-stream';
    $this->size  = $size;
    $this->error = $error ? : UPLOAD_ERR_OK;
    $this->test  = (bool)$test;

    parent::__construct($path, UPLOAD_ERR_OK === $this->error);
  }

  /**
   * Returns the original file name.
   *
   * It is extracted from the request from which the file has been uploaded.
   * Then is should not be considered as a safe value.
   *
   * @return string
   */
  public function getClientOriginalName()
  {
    return $this->name;
  }

  /**
   * Returns the file mime type.
   *
   * It is extracted from the request from which the file has been uploaded.
   * Then is should not be considered as a safe value.
   *
   * @return string
   */
  public function getClientMimeType()
  {
    return $this->mime;
  }

  /**
   * Returns the file size.
   *
   * It is extracted from the request from which the file has been uploaded.
   * Then is should not be considered as a safe value.
   *
   * @return string|null
   */
  public function getClientSize()
  {
    return $this->size;
  }

  /**
   * Returns the upload error.
   *
   * If the upload was successful, the constant UPLOAD_ERR_OK is returned.
   * Otherwise one of the other UPLOAD_ERR_XXX constants is returned.
   *
   * @return integer
   */
  public function getError()
  {
    return $this->error;
  }

  /**
   * Returns whether the file was uploaded successfully.
   *
   * @return boolean True if no error occurred during uploading
   */
  public function isValid()
  {
    return $this->error === UPLOAD_ERR_OK;
  }

  /**
   * Moves the file to a new location.
   *
   * @param string $dir
   * @param null   $name
   *
   * @return \Pimf\Util\File
   * @throws \RuntimeException If the file has not been uploaded via Http or can not move the file.
   */
  public function move($dir, $name = null)
  {
    if ($this->isValid()) {

      if ($this->test) {
        return parent::move($dir, $name);
      }

      if (is_uploaded_file($this->getPathname())) {

        $target = $this->getTargetFile($dir, $name);

        if (!@move_uploaded_file($this->getPathname(), $target)) {
          $error = error_get_last();
          throw new \RuntimeException("Could not move the file {$this->getPathname()} to $target ({$error['message']})");
        }

        @chmod($target, 0666 & ~umask());

        return $target;
      }
    }

    throw new \RuntimeException("The file {$this->getPathname()} has not been uploaded via Http");
  }

  /**
   * Returns the maximum size of an uploaded file in bytes as configured in php.ini
   *
   * @return int
   */
  public static function getMaxFilesize()
  {
    $max = trim(ini_get('upload_max_filesize'));

    if ('' === $max) {
      return PHP_INT_MAX;
    }

    $unit = strtolower(substr($max, -1));

    if (in_array($unit, array('g', 'm', 'k'), true)) {
      $max *= 1024;
    }

    return (integer)$max;
  }
}
