<?php
/*
 * This file is part of the Cygnite package.
 *
 * (c) Sanjoy Dey <dey.sanjoy0@gmail.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
namespace Application\Components\Authentication;

use Cygnite\Auth\AuthManager;
use Cygnite\Auth\AuthInterface;
use Cygnite\Common\UrlManager\Url;
use Cygnite\Foundation\Application;
use Cygnite\Common\SessionManager\Session;
use Cygnite\Auth\Exception\InvalidCredentialException;

class Auth extends AuthManager implements AuthInterface
{
    public static $user = array();
    public static $msg = 'Welcome! ';
    public $username;
    public $valid = false;
    public $attempt = 0;
    protected $item = array();
    protected $errors = array();
    private $credential = array();
    private $table;

    /**
     * We will make Auth instance and return singleton
     * instance to the user
     *
     * @return object
     */
    public static function make()
    {
        $app = self::getContainer();
        $auth = __CLASS__;
        return $app->singleton(
            'auth',
            function ($c) use ($auth) {
                return new $auth;
            }
        );
    }

    /**
     * Get user credentials
     *
     * @return array|null
     */
    public function getCredential()
    {
        return !empty($this->credential) ? $this->credential : null;
    }

    /**
     * Set User Credentials to authentication
     *
     * @param $credential
     */
    public function setCredential($credential)
    {
        $this->credential = $credential;
    }

    /**
     * We will validate user and return boolean value
     *
     * $input = array('email' => 'dey.sanjoy0@gmail.com', 'password' => 'xyz@324', 'status' => 1);
     * $auth->verify($input);
     *
     * @param      $user
     * @param null $password
     * @param bool $status
     * @throws \Exception
     * @return bool
     */
    public function verify($user, $password = null, $status = false)
    {
        $this->table = $this->table();
        $credential = array();

        if (is_array($user)) {
            $credential = $this->credential($user)->getCredential();
        } else {
            $credential = $this->credential($user, $password, $status)->getCredential();
        }

        /**
        | Get user information from model
        | to verify against user input
         */
        $userInfo = $this->setWhere()->findAll();

        if ($userInfo->count() > 0) {

            /*
             | Validate user against password
             | if user validated return true
             */
            if (trim($userInfo[0]->password) == trim($credential['password'])) {

                $this->valid = true;
                self::$user = $userInfo;
                $this->attempt = 0;

                return true;

            } else {

                return $this->setFailure('password');
            } // password validation end

        } else {

            return $this->setFailure('user');
        } // no user found
    }

    /**
     * Login user with user credentials
     *
     * @throws \Cygnite\Auth\Exception\InvalidCredentialException
     * @return boolean
     */
    public function login()
    {
        if ($this->valid) {
            return $this->createSession();
        } else {

            $credential = $this->getCredential();
            if (empty($credential)) {
                throw new InvalidCredentialException('Please set credential using Auth::setCredential($credential) to login.');
            }

            if ($valid = $this->verify($credential)) {
                return ($valid) ? $this->createSession() : $valid;
            }
        }
    }

    /**
     * Check user logged in or not
     *
     * @return boolean
     */
    public function isLoggedIn()
    {
        //If user has valid session, and such is logged in
        if (Session::has('auth:' . trim($this->table))) {

            $session = Session::get('auth:' . trim($this->table));

            return (isset($session['isLoggedIn']) && $session['isLoggedIn'] == true) ? true : false;
        }

        return false;
    }

    public function rememberMe()
    {

    }

    /**
     * Return number of un-successful attempt by user
     *
     * @return int
     */
    public function attempts()
    {
        return $this->attempt;
    }

    /**
     * Magic Method for handling dynamic data access.
     */
    public function __get($key)
    {
        return $this->item[$key];
    }

    /**
     * Magic Method for handling the dynamic setting of data.
     */
    public function __set($key, $value)
    {
        $this->item[$key] = $value;
    }

    /**
     * We will destroy current user session and return to
     * application base url
     */
    public function logout($redirect = true)
    {
        Session::delete();

        ($redirect) ? Url::redirectTo(Url::getBase()) : '';
    }

    public function userInfo()
    {
        if (Session::has('auth:' . trim($this->table))) {
            $user = Session::get('auth:' . trim($this->table));
            return $user;
        }
    }

    /**
     * Set user credentials into array
     *
     * @param      $user
     * @param null $password
     * @param bool $status
     * @return $this
     */
    protected function credential($user, $password = null, $status = false)
    {
        /**
        | We will check is array passed as first argument
        | then we will simply return Auth instance
         */
        if (is_array($user)) {
            $this->setCredential($user);
            return $this;
        }

        $credential = array();

        if ($status) {
            $credential = array('username' => $user, 'password' => $password, 'status' => $status);
        } else {
            $credential = array('username' => $user, 'password' => $password);
        }

        $this->setCredential($credential);

        return $this;
    }

    private function setFailure($key)
    {
        $this->valid = false;
        $this->attempt++;
        $this->setError($key, 0);

        return false;
    }

    /**
     * @return array|null
     */
    private function setWhere()
    {
        $credentials = $this->getCredential();

        $i = 0;
        foreach ($credentials as $key => $value) {

            if ($i == 0) {
                $this->username = $value;
                $where = static::user()->where($key, '=', $value);
            }

            if ($i == 2 || $key == 'status') {
                $where = static::user()->where($key, '=', $value);
            }

            $i++;
        }

        return $where;
    }

    /**
     * @return bool
     */
    private function createSession()
    {
        $hasSession = $this->setSession();
        $this->setUserInfo(self::$user);

        return ($hasSession) ? true : false;
    }

    /**
     * We will set session
     *
     * @return mixed
     */
    private function setSession()
    {
        $primaryKey = null;
        $data = array();
        $primaryKey = self::$user[0]->getPrimaryKey();

        $data[$primaryKey] = self::$user[0]->{$primaryKey};

        foreach (self::$user[0]->getAttributes() as $key => $val) {
            $data[$key] = $val;
        }

        $data['isLoggedIn'] = true;
        $data['flashMsg'] = static::$msg . ucfirst($this->username);

        Session::set('auth:' . trim($this->table), $data);

        return true;
    }

    /**
     * We will set authentication error as property
     *
     * @param $key
     * @param $value
     */
    private function setError($key, $value)
    {
        $this->errors[$key] = $value;
    }

    /**
     * We will set user information into Auth property
     * So that you can easily access those information directly
     * from the auth instance
     *
     * @param $userInfo
     */
    private function setUserInfo($userInfo)
    {
        foreach ($userInfo as $key => $value) {
            $this->{$key} = $value;
        }
    }
}
