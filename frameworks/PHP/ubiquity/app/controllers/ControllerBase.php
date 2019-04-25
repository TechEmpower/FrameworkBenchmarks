<?php
namespace controllers;

use Ubiquity\controllers\Controller;
use Ubiquity\utils\http\URequest;

/**
 * ControllerBase.
 **/
abstract class ControllerBase extends Controller{
	protected $headerView = "@activeTheme/main/vHeader.html";
	protected $footerView = "@activeTheme/main/vFooter.html";

	public function initialize() {
		if (! URequest::isAjax ()) {
			$this->loadView ( $this->headerView );
		}
	}

	public function finalize() {
		if (! URequest::isAjax ()) {
			$this->loadView ( $this->footerView );
		}
	}
}

