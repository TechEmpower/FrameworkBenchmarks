<?php
//
// 1. JSON Test
require_once dirname(__FILE__).'/once.php.inc';

$b = new Benchmark();
$b->bench_json();
