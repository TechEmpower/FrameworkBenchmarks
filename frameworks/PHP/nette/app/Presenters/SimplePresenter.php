<?php

declare(strict_types=1);

namespace App\Presenters;

use Nette;

final class SimplePresenter extends Nette\Application\UI\Presenter
{

	public function renderPlainText() {
		$this->getHttpResponse()->setContentType('text/plain', 'UTF-8');
		$this->sendResponse(new \Nette\Application\Responses\TextResponse('Hello, World!'));
	}

	public function renderJson() {
		$this->sendJson(['message' => 'Hello, World!']);
	}
}
