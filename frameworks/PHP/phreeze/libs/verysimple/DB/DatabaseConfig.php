<?php 
/** @package verysimple::DB::DataDriver */


/**
 * A static class that holds global database configuration options.
 *
 * @package    verysimple::DB::DataDriver
 * @author     VerySimple Inc. <noreply@verysimple.com>
 * @copyright  1997-2010 VerySimple Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
class DatabaseConfig
{	
	/** @var boolean set to true to convert NULL values to empty string when inserting/updating */
	public static $CONVERT_NULL_TO_EMPTYSTRING = true;

}

?>