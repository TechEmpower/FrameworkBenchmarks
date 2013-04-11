<?php
/** @package    verysimple::DB */
 
 /**
 * DatabaseException is thrown when an error occurs that involves the database
 * @package    verysimple::DB
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
class DatabaseException extends Exception
{
	
	/** codes used to determine error sub-type */
	static $UNKNOWN = 0;
	static $CONNECTION_ERROR = 1;
	static $ERROR_IN_QUERY = 2;
	
	public $data;
	
    // Redefine the constructor so message isn't optional
    public function __construct($message, $code = 0, $data = "") 
	{
        // make sure everything is assigned properly
        parent::__construct($message, $code);
        
        $this->data = $data;
    }

}

?>