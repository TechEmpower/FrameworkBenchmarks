<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
require_once("Model/DAO/AccountDAO.php");
require_once("verysimple/Authentication/IAuthenticatable.php");

// these are some generic permission settings.  You should set your own in
// your account object
define("ACCOUNT_PERMISSION_NONE",0);
define("ACCOUNT_PERMISSION_READ",1);
define("ACCOUNT_PERMISSION_WRITE",2);
define("ACCOUNT_PERMISSION_ADMIN",4);

/**
 * @package    verysimple::Phreeze
 */

/**
 * This is a sample account object that can  be extended under the condition that your
 * account object meets the following criteria:
 * 
 * The Model is name Account with the following properties/methods:
 * - Id (int/primary key)
 * - Username (string)
 * - Password (string)
 * - Modifed (datetime)
 * - GetRole (returns a role object with a 'Permission' property that contains a bit-wise integer)
 *
 * Extending your account object from this base class will provide the following features:
 * - Your class implements IAuthenticatable for use with Controller authentication
 * - Login method will load this object
 * - Password changes will be detected on save and passwords will be one-way crypted
 *
 * @package    verysimple::Phreeze
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.0
 */
class AuthAccount extends AccountDAO implements IAuthenticatable
{
	/** @var string this is public for serialization */
	public $_original_password = "";

	/**
	 * Checks if the current user is "anonymous" meaning they have not authenticated
	 * @return bool true if user is anonymous
	 */
	function IsAnonymous()
	{
		return (!$this->Id);
	}
	
	function PasswordWasChanged()
	{
		return ($this->Password != $this->_original_password && $this->Password != "");
	}
	
	/**
	 * Returns true if the current account has the specified permission
	 * @param int $permission a bitwise integer representing a unique permission in the application
	 * @return bool true if the current account is authorize for the given permission
	 */
	function IsAuthorized($permission)
	{
		if ($this->IsAnonymous())
		{
			return false;
		}
		
		return (($this->GetRole()->Permission & $permission) > 0);
	}
	
	/**
	 * Attempts to authenticate based on the provided username/password.  if
	 * successful, the object is populated with data from the data store
	 *
	 * @param string $username
	 * @param string $password
	 * @return bool true if login was successful
	 */
	function Login($username, $password)
	{
		// for backwards compatibility with Phreeze 2x, look in multiple places for the AccountCriteria class
		if (!class_exists("AccountCriteria")) @include_once("Model/AccountCriteria.php");
		if (!class_exists("AccountCriteria")) @include_once("Model/DAO/AccountCriteria.php");
		if (!class_exists("AccountCriteria")) throw new Exception("Unable to locate AccountCriteria class.");
		
		if ($username == "" || $password == "")
		{
			return false;
		}
		
		$this->_phreezer->Observe("AuthAccount.Login Searching For Matching Account...");

		$criteria = new AccountCriteria();
		// set both the name and the _Equals properties for backwards compatibility
		$criteria->Username = $username;
		$criteria->Username_Equals = $username;
		$criteria->Password = base64_encode(crypt($password,$username));
		$criteria->Password_Equals = base64_encode(crypt($password,$username));
		
		$ds = $this->_phreezer->Query("Account", $criteria);
		
		// we have to clear the cache, this resolves an issue where logging in repeatedly 
		// will retain the same cached child objects
		$this->ClearCache();
		
		if ($account = $ds->Next())
		{
			// we can't do $this = $account, so instead just clone all the properties:
			$this->LoadFromObject($account);
			$this->GetRole(); // this triggers the role to load so it will be cached
			
			// we need to update the login date and count
			//$this->LastLogin = date('Y-m-d H:i:s');
			//$this->LoginCount++;
			//$this->Save();

			return true;
		}
		else
		{
			return false;
		}
	}
	
	/** 
	 * if the password has changed since load, then we want to crypt it
	 * otherwise we don't want to touch it because it is already crypted
	 *
	 * @param bool $is_insert
	 * @return bool
	 */
	function OnSave($is_insert)
	{
		// if the password has changed since load, then we want to crypt it
		// otherwise we don't want to touch it because it is already crypted
		if ($is_insert || $this->PasswordWasChanged() )
		{
			$this->_phreezer->Observe("Account-&gt;OnSave: The password has changed");
			$this->Password = base64_encode(crypt($this->Password,$this->Username));
		}
		else
		{
			$this->Password = $this->_original_password;
			$this->_phreezer->Observe("Account->OnSave: The password was not changed");
		}		

		// update the modified date
		$this->Modified = date('Y-m-d H:i:s');
		return true;
	}
	
	/**
	 * stores the original password so we can detect if it has been changed
	 */
	function OnLoad()
	{
		$this->_original_password = $this->Password;
	}
	
	/**
	 * Updates the password for this account
	 * @param string $new_pass
	 */
	function UpdatePassword($new_pass)
	{
		$this->_original_password = ""; // force Save to crypt the password
		$this->Password = $new_pass; //base64_encode(crypt($this->Password,$this->Username));
		$this->Save();
	}
	
}

?>