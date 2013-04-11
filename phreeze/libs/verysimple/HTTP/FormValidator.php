<?php
/** @package    verysimple::HTTP */

/** import supporting libraries */


/**
 * Static utility class for validating form input
 *
 * Contains various methods for validating standard information such
 * as email, dates, credit card numbers, etc
 *
 * @package    verysimple::HTTP 
 * @author     VerySimple Inc.
 * @copyright  1997-2008 VerySimple, Inc. http://www.verysimple.com
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
class FormValidator
{

	/** 
	 * Returns true if the provided email is valid
	 * 
	 * @param string email address
	 * @return bool
	 */
	static function IsValidEmail($email)
	{
		return (
			eregi("^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,3})$", $email)
			);
	}
	
	/** 
	 * Returns true if the provided credit card appears to be valid.  If type is
	 * provided, then the validation makes sure it is valid for that specific
	 * type, otherwise it just makes sure it is valid for any type
	 * 
	 * @param string credit card number
	 * @param string type [optional] (American, Dinners, Discover, Master, Visa)
	 * @return bool
	 */
	static function IsValidCreditCard($cc_num, $type = "") 
	{
		
		if($type == "American") {
			$denum = "American Express";
		} elseif($type == "Dinners") {
			$denum = "Diner's Club";
		} elseif($type == "Discover") {
			$denum = "Discover";
		} elseif($type == "Master") {
			$denum = "Master Card";
		} elseif($type == "Visa") {
			$denum = "Visa";
		}
		
		$verified = false;
		
		if($type == "American" || $type == "") {
			$pattern = "/^([34|37]{2})([0-9]{13})$/";//American Express
			if (preg_match($pattern,$cc_num)) {
				$verified = true;
			}
			
			
		}
		
		if($type == "Dinners" || $type == "") {
			$pattern = "/^([30|36|38]{2})([0-9]{12})$/";//Diner's Club
			if (preg_match($pattern,$cc_num)) {
				$verified = true;
			}
			
			
		}
		
		if($type == "Discover" || $type == "") {
			$pattern = "/^([6011]{4})([0-9]{12})$/";//Discover Card
			if (preg_match($pattern,$cc_num)) {
				$verified = true;
			}
			
			
		}
		
		if($type == "Master" || $type == "") {
			$pattern = "/^([51|52|53|54|55]{2})([0-9]{14})$/";//Mastercard
			if (preg_match($pattern,$cc_num)) {
				$verified = true;
			}
			
			
		} 
		
		if($type == "Visa" || $type == "") {
			$pattern = "/^([4]{1})([0-9]{12,15})$/";//Visa
			if (preg_match($pattern,$cc_num)) {
				$verified = true;
			}
			
		}
		
		return $verified;
		
	}
	
}

?>