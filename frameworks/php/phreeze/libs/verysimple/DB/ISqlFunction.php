<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */

/**
 * ISqlFunction is an interface that defines a SQL function.  This can be used
 * to insert/update or query a database with a value that is not quoted,
 * for example sysdate().

 * @package    verysimple::Phreeze
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.0
 */
interface ISqlFunction
{
	/**
	 * Return the quoted SQL that will be used for the insert/update/select
	 * @param Phreezer
	 * @return string
	 */
	public function GetQuotedSql($phreezer);
}

?>