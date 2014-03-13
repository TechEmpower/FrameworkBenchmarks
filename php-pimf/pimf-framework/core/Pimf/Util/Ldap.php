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
 * Wrapper for Lightweight Directory Access Protocol and for a access to "Directory Servers"
 *
 * For use please add the following to the end of the config.app.php file:
 *
 * <code>
 *
 * 'ldap' => array(
 *
 *    // where in the directory tree to be started for specific objects searching (is optional)
 *    'basedn' => 'dc=example,dc=com'
 *
 *    // hostname of the domain controller
 *    'host' => 'dc',
 *
 *    // the domain name
 *    'domain' => 'example.com',
 *
 *    // optionally require users to be in this group
 *    //'group' => 'AppUsers',
 *
 *    // domain credentials the app should use to validate users
 *    // this user does not need any privileges - it's just used to connect to the DC.
 *    'user' => 'ldap-user_here',
 *    'password' => 'ldap-password-here',
 * ),
 *
 * </code>
 *
 * @link    http://www.php.net/manual/en/intro.ldap.php
 * @package Util
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Ldap
{
  /**
   * @var resource A positive LDAP link identifier
   */
  protected $conn;

  public function __construct()
  {
    if (!function_exists('ldap_connect')) {
      throw new \RuntimeException('LDAP-auth requires the php-ldap extension to be installed');
    }
  }

  public function __destruct()
  {
    if (is_resource($this->conn)) {
      ldap_unbind($this->conn);
    }
  }

  /**
   * Get the current user of the application.
   *
   * @param $userDN
   *
   * @return null|Ldap\User
   */
  public function retrieve($userDN)
  {
    if (!is_resource($this->conn)) {
      $config = Registry::get('ldap');
      $this->connect($config['user'], $config['password']);
    }

    return $this->getUser($userDN);
  }

  /**
   * Attempt to log a user into the application.
   *
   * @param $username
   * @param $password
   *
   * @return Ldap\User
   */
  public function attempt($username, $password)
  {
    $config = Registry::get('ldap');

    return $this->login($username, $password, $config['group']);
  }

  /**
   * @param $user
   * @param $password
   *
   * @throws \RuntimeException
   */
  protected function connect($user, $password)
  {
    $config = Registry::get('ldap');

    // guess base DN from domain
    if (!isset($config['basedn'])) {
      $length           = strrpos($config['domain'], '.');
      $config['basedn'] = sprintf(
        'dc=%s,dc=%s', substr($config['domain'], 0, $length), substr($config['domain'], $length + 1)
      );

      // override the basedn
      Registry::set('ldap', $config);
    }

    // connect to the controller
    if (!$this->conn = ldap_connect("ldap://{$config['host']}.{$config['domain']}")) {
      throw new \RuntimeException("could not connect to LDAP host {$config['host']}.{$config['domain']}");
    }

    // required for Windows AD
    ldap_set_option($this->conn, LDAP_OPT_PROTOCOL_VERSION, 3);
    ldap_set_option($this->conn, LDAP_OPT_REFERRALS, 0);

    // try to authenticate
    if (!@ldap_bind($this->conn, "{$user}@{$config['domain']}", $password)) {
      throw new \RuntimeException('could not bind to AD: ' . "{$user}@{$config['domain']}");
    }
  }

  /**
   * @param      $user
   * @param      $password
   * @param null $group
   *
   * @return Ldap\User
   * @throws \RuntimeException
   */
  protected function login($user, $password, $group = null)
  {
    $this->connect($user, $password);

    $config      = Registry::get('ldap');
    $groupObject = $this->getAccount($group, $config['basedn']);
    $userObject  = $this->getAccount($user, $config['basedn']);

    if ($group && !$this->checkGroup($userObject['dn'], $groupObject['dn'])) {
      throw new \RuntimeException('user is not part of the "' . $group . '" group.');
    }

    return $this->fetch($userObject);
  }

  /**
   * @param array $user
   *
   * @return Ldap\User
   * @throws \RuntimeException
   */
  protected function fetch(array $user)
  {
    if (!isset($user['cn'][0])) {
      throw new \RuntimeException('not a valid user object');
    }

    return Ldap\User::factory($user);
  }

  /**
   * Searches the LDAP tree for the specified account or group
   *
   * @param $account
   * @param $basedn
   *
   * @return null|array
   */
  protected function getAccount($account, $basedn)
  {
    $result = ldap_search(
      $this->conn, $basedn, "(samaccountname={$account})", array('dn', 'givenname', 'sn', 'cn', 'memberof', 'objectguid')
    );

    if (!$result) {
      return null;
    }

    $entries = ldap_get_entries($this->conn, $result);

    if ($entries['count'] > 0) {
      return $entries[0];
    }
  }

  /**
   * Checks group membership of the user, searching
   * in the specified group and its children (recursively)
   *
   * @param $userDN
   * @param $groupDN
   *
   * @return bool
   * @throws \RuntimeException
   */
  public function checkGroup($userDN, $groupDN)
  {
    if (!$user = $this->getUser($userDN)) {
      throw new \RuntimeException('invalid user DN');
    }

    $memberof = $user->getMemberof();

    for ($i = 0; $i < $memberof['count']; $i++) {
      if ($groupDN == $memberof[$i]) {
        return true;
      }
    }

    return false;
  }

  /**
   * @param $userDN
   *
   * @return null|Ldap\User
   * @throws \RuntimeException
   */
  public function getUser($userDN)
  {
    if (!is_resource($this->conn)) {
      throw new \RuntimeException('no LDAP connection bound');
    }

    if (!$result = ldap_read($this->conn, $userDN, '(objectclass=*)')) {
      return null;
    }

    $entries = ldap_get_entries($this->conn, $result);

    if (!$entries['count']) {
      return null;
    }

    return $this->fetch($entries[0]);
  }
}
