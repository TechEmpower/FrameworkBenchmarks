<?php
function dbraw($pdo) {
  $query_count = 1;
  if (isset($_GET['queries']) && $_GET['queries'] > 0) {
    $query_count = $_GET['queries'];
  }
  if ($query_count > 500) $query_count=500;
  $is_one = $query_count == 1;

  $arr = [];
  $id = mt_rand(1, 10000);
  $statement = $pdo->prepare('SELECT randomNumber FROM World WHERE id = :id');
  $statement->bindParam(':id', $id, PDO::PARAM_INT);

  while ($query_count--) {
    $statement->execute();
    $arr[] = ['id' => $id, 'randomNumber' => $statement->fetchColumn()];
    $id = mt_rand(1, 10000);
  }
  if ($is_one) $arr = $arr[0];

  echo json_encode($arr);
}
