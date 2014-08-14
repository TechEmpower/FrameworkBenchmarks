<?php
/** @package    verysimple::Phreeze */

/**
 * A validation response is an object that stores the results when an
 * object is validated.  This code can be serialized as JSON and used
 * in AJAX validation
 * @package    verysimple::Phreeze
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.0
 */
class ValidationResponse
{

	public function __construct($success = false, $message = "", $primaryKey = "", $errors = null)
	{
		$this->Success = $success;
		$this->Message = $message;
		$this->Errors = $errors ? $errors : Array();
		$this->PrimaryKeyValue = $primaryKey;
	}

	public $PrimaryKeyValue;
	public $Success = false;
	public $Errors = Array();
	public $Message = "";
	public $Data;
}

?>