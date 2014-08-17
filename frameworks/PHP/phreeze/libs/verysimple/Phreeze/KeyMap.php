<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
define("KM_LOAD_LAZY",1);
define("KM_LOAD_EAGER",2);
define("KM_LOAD_INNER",4);
define("KM_TYPE_ONETOMANY",1);
define("KM_TYPE_MANYTOONE",2);

/**
 * KeyMap is a class for storing mapping information for Foreign Keys.  KeyMaps are
 * used by Phreeze to map DB relationships to their respective model objects
 *
 * @package    verysimple::Phreeze 
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
class KeyMap
{
	public $KeyName;
	public $KeyProperty;
	public $ForeignObject;
	public $ForeignKeyProperty;
	public $KeyType;
	public $LoadType;
	
	/**
	 * Initializes the KeyMap
	 *
	 * @param string $kn KeyName a unique name for this key
	 * @param string $kp KeyProperty the foreign key property
	 * @param string $fo ForeignObject the name of the foreign model object
	 * @param string $fkp ForeignKeyProperty the primary key property of the foreign model object
	 * @param int $kt Key Type (optional default = KM_TYPE_ONETOMANY)
	 * @param int $lt Load Type KM_LOAD_LAZY | KM_LOAD_EAGER (optional default = KM_LOAD_LAZY)
	 */
	public function __construct($kn, $kp, $fo, $fkp, $kt = KM_TYPE_ONETOMANY, $lt = KM_LOAD_LAZY)
	{
		$this->KeyName = $kn;
		$this->KeyProperty = $kp;
		$this->ForeignObject = $fo;
		$this->ForeignKeyProperty = $fkp;
		$this->KeyType = $kt;
		$this->LoadType = $lt;
	}
}

?>