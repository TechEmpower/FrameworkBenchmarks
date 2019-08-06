<?php

namespace App\Controller;

use App\Entity\Fortune;
use App\Repository\FortuneRepository;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;
use Twig\Environment;

class FortunesController
{
    /** @var Environment */
    private $twig;
    /** @var FortuneRepository */
    private $fortuneRepository;

    public function __construct(Environment $twig, FortuneRepository $fortuneRepository)
    {
        $this->twig = $twig;
        $this->fortuneRepository = $fortuneRepository;
    }

    /**
     * @Route("/fortunes")
     * @throws
     */
    public function fortunes(): Response
    {
        $fortunes = $this->fortuneRepository->findAll();

        $runtimeFortune = new Fortune();
        $runtimeFortune->setId(0);
        $runtimeFortune->setMessage('Additional fortune added at request time.');

        $fortunes[] = $runtimeFortune;

        usort(
            $fortunes,
            static function ($left, $right) {
                return $left->message <=> $right->message;
            }
        );

        $content = $this->twig->render(
            'fortunes.html.twig',
            [
                'fortunes' => $fortunes,
            ]
        );

        return new Response($content);
    }
}
