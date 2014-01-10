<?php
//
// 5. Database updates
//

require_once dirname(__FILE__).'/once.php.inc';

function main() {
  // Read number of queries to run from URL parameter
  $query_count = 1;
  if (!empty($_GET['queries'])) {
    $query_count = intval($_GET['queries']);
  }

  // Fix the queries limits
  $query_count = $query_count < 1 ? 1 : ($query_count > 500 ? 500 : $query_count);

  $b = new Benchmark();
  $b->bench_updates($query_count);
}
main();
