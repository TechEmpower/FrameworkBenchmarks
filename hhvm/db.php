<?php
//
// 2. Single database query
//

require_once dirname(__FILE__).'/once.php.inc';

function main() {
    $b = new Benchmark();
    $b->bench_db();
}

main();
