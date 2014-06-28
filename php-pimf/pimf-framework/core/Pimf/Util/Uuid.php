<?php
/**
 * Util
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Util;

use Pimf\Registry;

/**
 * A class that generates RFC 4122 UUIDs
 *
 * <pre>
 * This specification defines a Uniform Resource Name namespace for
 * UUIDs (Universally Unique IDentifier), also known as GUIDs (Globally
 * Unique IDentifier).  A UUID is 128 bits long, and requires no central
 * registration process.
 * </pre>
 *
 * @package Util
 * @author  Gjero Krsteski <gjero@krsteski.de>
 * @see     http://www.ietf.org/rfc/rfc4122.txt
 */
final class Uuid
{
  /**
   * 32-bit integer that identifies this host.
   *
   * @var integer
   */
  private static $node = null;

  /**
   * Process identifier.
   *
   * @var integer
   */
  private static $pid = null;

  /**
   * Returns a 32-bit integer that identifies this host.
   *
   * The node identifier needs to be unique among nodes
   * in a cluster for a given application in order to
   * avoid collisions between generated identifiers.
   *
   * @return integer
   */
  private static function getNodeId()
  {
    $host     = Registry::get('env')->getIp();
    $hostname = Registry::get('env')->getHost();

    if ($host === null && true === function_exists('gethostname')) {
      $hostname = gethostname();
      $host     = gethostbyname($hostname);
    }

    if ($host === null && true === function_exists('php_uname')) {
      $hostname = php_uname('n');
      $host     = gethostbyname($hostname);
    }

    if ($host === null && $hostname !== null) {
      $host = crc32($hostname);
    }

    if ($host === null) {
      $host = '127.0.0.1';
    }

    return ip2long($host);
  }

  /**
   * Returns a process identifier.
   *
   * In multi-process servers, this should be the system process ID.
   * In multi-threaded servers, this should be some unique ID to
   * prevent two threads from generating precisely the same UUID
   * at the same time.
   *
   * @return integer
   */
  private static function getLockId()
  {
    return getmypid();
  }

  /**
   * Generate an RFC 4122 UUID.
   *
   * This is pseudo-random UUID influenced by the system clock, IP
   * address and process ID.
   *
   * The intended use is to generate an identifier that can uniquely
   * identify user generated posts, comments etc. made to a website.
   * This generation process should be sufficient to avoid collisions
   * between nodes in a cluster, and between apache children on the
   * same host.
   *
   * @return string
   */
  public static function generate()
  {
    if (self::$node === null) {
      self::$node = self::getNodeId();
    }

    if (self::$pid === null) {
      self::$pid = self::getLockId();
    }

    list($time_mid, $time_lo) = explode(' ', microtime());

    $time_low = (int)$time_lo;
    $time_mid = (int)substr($time_mid, 2);

    $time_and_version = mt_rand(0, 0xfff);
    /* version 4 UUID */
    $time_and_version |= 0x4000;

    $clock_seq_low = mt_rand(0, 0xff);

    /* type is pseudo-random */
    $clock_seq_high = mt_rand(0, 0x3f);
    $clock_seq_high |= 0x80;

    $node_low = self::$pid;
    $node     = self::$node;

    return sprintf(
      '%08x-%04x-%04x-%02x%02x-%04x%08x', $time_low, $time_mid & 0xffff, $time_and_version, $clock_seq_high, $clock_seq_low, $node_low,
      $node
    );
  }
}
