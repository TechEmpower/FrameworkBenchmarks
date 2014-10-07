<?php
//
// Database Test
//

// Database connection
// http://www.php.net/manual/en/ref.pdo-mysql.php
$pdo = new PDO('mysql:host=localhost;dbname=hello_world;charset=utf8', 'benchmarkdbuser', 'benchmarkdbpass', array(
    PDO::ATTR_PERSISTENT => true
));

// Define query
$statement = $pdo->query( 'SELECT id, message FROM Fortune' );
  
// Store result in array.
$arr = $statement->fetchAll(PDO::FETCH_KEY_PAIR); 
$arr[0] = 'Additional fortune added at request time.';

asort($arr);
?>
<!DOCTYPE html>
<html>
<head>
<title>Fortunes</title>
</head>
<body>
<table>
<tr>
<th>id</th>
<th>message</th>
</tr>
<?php
foreach ( $arr as $id => &$fortune ) {
?>
<tr>
<td><?php echo htmlspecialchars($id, ENT_QUOTES, 'UTF-8'); ?></td>  
<td><?php echo htmlspecialchars($fortune, ENT_QUOTES, 'UTF-8'); ?></td>
</tr>
<?php } ?>
</table>
</body>
</html>
