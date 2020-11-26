<?php
//
// Database Test
//

// Database connection
// http://www.php.net/manual/en/ref.pdo-mysql.php
$pdo = new PDO('mysql:host=tfb-database;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', [
    PDO::ATTR_PERSISTENT => true
]);
  
// Define query and store result in array.
$arr = $pdo->query( 'SELECT id, message FROM Fortune' )->fetchAll(PDO::FETCH_KEY_PAIR); 
$arr[0] = 'Additional fortune added at request time.';

asort($arr);
?>
<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
<?php foreach ( $arr as $id => $fortune ) : ?>
<tr><td><?= $id ?></td><td><?= htmlspecialchars($fortune, ENT_QUOTES, 'UTF-8') ?></td></tr>
<?php endforeach ?></table></body></html>