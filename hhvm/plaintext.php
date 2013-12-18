<?php
//
// 6. Plaintext Test
require_once dirname(__FILE__).'/once.php.inc';

function main() {
    $b = new Benchmark();
    $b->bench_plaintext();
}

main();
