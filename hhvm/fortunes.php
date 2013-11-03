<?php
//
// 4. Fortunes
//

// Database connection
$pdo = new PDO('mysql:host=localhost;dbname=hello_world;charset=utf8', 'benchmarkdbuser', 'benchmarkdbpass', array(
    PDO::ATTR_PERSISTENT => true,
    PDO::MYSQL_ATTR_INIT_COMMAND => "SET NAMES 'utf8'"
));

// Define query
$statement = $pdo->query( 'SELECT id, message FROM Fortune' );

// Store result in array.
$arr = $statement->fetchAll(PDO::FETCH_KEY_PAIR);
$arr[0] = 'Additional fortune added at request time.';

asort($arr);
header("Content-Type: text/html; charset=utf-8");
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
<td><?php echo htmlspecialchars($id, ENT_QUOTES, 'utf-8'); ?></td>
<td><?php echo htmlspecialchars($fortune, ENT_QUOTES, 'utf-8'); ?></td>
</tr>
<?php } ?>
</table>
</body>
</html>
