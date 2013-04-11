<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\g11n\catalog\adapter;

class MockGettext extends \lithium\g11n\catalog\adapter\Gettext {

	public $mo = true;

	public $po = true;

	protected function _files($category, $locale, $scope) {
		$files = parent::_files($category, $locale, $scope);

		foreach ($files as $key => $file) {
			$extension = pathinfo($file, PATHINFO_EXTENSION);

			if ((!$this->mo && $extension === 'mo') || (!$this->po && $extension === 'po')) {
				unset($files[$key]);
			}
		}
		return $files;
	}
}

?>