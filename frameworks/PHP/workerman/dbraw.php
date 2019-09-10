<?php
function dbraw($pdo) {
  if (! isset($_GET['queries'])) {
    $statement = $pdo->query( 'SELECT id,randomNumber FROM World WHERE id = '. mt_rand(1, 10000) );
    echo json_encode($statement->fetch(PDO::FETCH_ASSOC));

    return;
  }

  $query_count = 1;
  if ($_GET['queries'] > 1) {
    $query_count = min($_GET['queries'], 500);
  }
 
  $arr = [];
  $statement = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id = ?');

  while ($query_count--) {
    $statement->execute([mt_rand(1, 10000)]);
    $arr[] = $statement->fetch(PDO::FETCH_ASSOC);
  }

  echo json_encode($arr);
}
