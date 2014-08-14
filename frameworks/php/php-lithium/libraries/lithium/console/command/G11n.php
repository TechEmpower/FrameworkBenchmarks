<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\console\command;

use lithium\console\command\g11n\Extract;

/**
 * The `G11n` set of commands deals with the extraction and merging of message templates.
 */
class G11n extends \lithium\console\Command {

	/**
	 * The main method of the command.
	 *
	 * @return void
	 */
	public function run() {}

	/**
	 * Runs the `Extract` command.
	 *
	 * @return void
	 */
	public function extract() {
		$extract = new Extract(array('request' => $this->request));
		return $extract->run();
	}
}

?>