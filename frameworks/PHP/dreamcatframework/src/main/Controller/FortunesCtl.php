<?php

namespace DreamCat\Benchmark\Controller;

use DreamCat\Benchmark\Service\FortuneTestService;
use DreamCat\Benchmark\View\FortuneView;
use Zend\Diactoros\Response\HtmlResponse;

class FortunesCtl
{
    /**
     * @Autowire
     * @var FortuneView
     */
    private $view;
    /**
     * @Autowire
     * @var FortuneTestService
     */
    private $service;

    /**
     * @return FortuneView
     */
    public function getView(): FortuneView
    {
        return $this->view;
    }

    /**
     * @param FortuneView $view
     * @return FortunesCtl
     */
    public function setView(FortuneView $view): FortunesCtl
    {
        $this->view = $view;
        return $this;
    }

    /**
     * @return FortuneTestService
     */
    public function getService(): FortuneTestService
    {
        return $this->service;
    }

    /**
     * @param FortuneTestService $service
     * @return FortunesCtl
     */
    public function setService(FortuneTestService $service): FortunesCtl
    {
        $this->service = $service;
        return $this;
    }

    /**
     * fortunes控制器
     * @return HtmlResponse
     */
    public function index()
    {
        $html = $this->view->messageHtml($this->service->getAllMessages());
        return new HtmlResponse($html);
    }
}

# end of file
