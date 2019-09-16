<?php
function updateraw($pdo) {
  $query_count = 1;
  if ($_GET['queries'] > 1) {
    $query_count = min($_GET['queries'], 500);
  }

  $arr = [];
  $statement = $pdo->prepare('SELECT randomNumber FROM World WHERE id = ?');
  $updateStatement = $pdo->prepare('UPDATE World SET randomNumber = ? WHERE id = ?');

  while ($query_count--) {
    $id = mt_rand(1, 10000);
    $statement->execute([$id]);
    
    $world = ['id' => $id, 'randomNumber' => $statement->fetchColumn()];
    $updateStatement->execute(
      [$world['randomNumber'] = mt_rand(1, 10000), $id]
    );
 
    $arr[] = $world;
  }

  return json_encode($arr);
}
