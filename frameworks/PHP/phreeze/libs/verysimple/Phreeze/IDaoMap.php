<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
require_once("FieldMap.php");
require_once("KeyMap.php");

/**
 * IDaoMap is an interface for a mapped object that can be persisted by Phreezer
 *
 * @package    verysimple::Phreeze
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.0
 */
interface IDaoMap
{
	static function GetFieldMaps();
	static function GetKeyMaps();
}

?>