<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\data\source\mongo_db;

use MongoGridFSFile;

class Result extends \lithium\data\source\Result {

	/**
	 * Fetches the result from the resource and caches it.
	 *
	 * @return boolean Return `true` on success or `false` if it is not valid.
	 */
	protected function _fetchFromResource() {
		if ($this->_resource && $this->_resource->hasNext()) {
			$result = $this->_resource->getNext();
			$isFile = ($result instanceof MongoGridFSFile);
			$result = $isFile ? array('file' => $result) + $result->file : $result;
			$this->_key = $this->_iterator;
			$this->_current = $this->_cache[$this->_iterator++] = $result;
			return true;
		}
		return false;
	}
}

?>