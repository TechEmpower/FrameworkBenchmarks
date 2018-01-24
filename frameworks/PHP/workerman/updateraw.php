<?php
function updateraw($pdo) {
  $query_count = 1;
  if (isset($_GET['queries']) && $_GET['queries'] > 0) {
    $query_count = $_GET['queries'];
  }
  if ($query_count > 500) $query_count=500;

  $arr = [];
  $id = mt_rand(1, 10000);
  $randomNumber = mt_rand(1, 1000);

  $statement = $pdo->prepare('SELECT randomNumber FROM World WHERE id = :id');
  $statement->bindParam(':id', $id, PDO::PARAM_INT);

  $updateStatement = $pdo->prepare('UPDATE World SET randomNumber = :randomNumber WHERE id = :id');
  $updateStatement->bindParam(':id', $id, PDO::PARAM_INT);
  $updateStatement->bindParam(':randomNumber', $randomNumber, PDO::PARAM_INT);

  while ($query_count--) {
    $statement->execute();
    
    $world = ['id' => $id, 'randomNumber' => $statement->fetchColumn()];
    $world['randomNumber'] = $randomNumber;
    $updateStatement->execute();
    
    $arr[] = $world;
    $id = mt_rand(1, 10000);
    $randomNumber = mt_rand(1, 10000);
  }

  echo json_encode($arr);
}
