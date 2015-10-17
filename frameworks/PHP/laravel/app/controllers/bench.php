<?php

class BenchController extends BaseController {

    public $layout = 'layouts.main';

    public function fortunes() {
        $fortunes = DB::table('Fortune')->get();

        $runtimeFortune = new stdClass;
        $runtimeFortune->id = 0;
        $runtimeFortune->message = 'Additional fortune added at request time.';

        $fortunes[] = $runtimeFortune;

        usort($fortunes, function($left, $right) {
            if ($left->message === $right->message) {
                return 0;
            } else if ($left->message > $right->message) {
                return 1;
            } else {
                return -1;
            }
        });

        $this->layout->nest('content', 'bench.fortunes', [
            'fortunes' => $fortunes
        ]);
    }

}