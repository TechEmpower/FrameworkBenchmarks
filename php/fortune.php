<?php
//
// Database Test
//

// Database connection
// http://www.php.net/manual/en/ref.pdo-mysql.php
$pdo = new PDO('mysql:host=localhost;dbname=hello_world', 'benchmarkdbuser', 'benchmarkdbpass', array(
    PDO::ATTR_PERSISTENT => true
));

// Define query
$statement = $pdo->prepare('SELECT * FROM Fortune');
$statement->execute();
  
// Store result in array.
$arr = $statment->fetchAll();
$arr[] = array('id' => 0, 'message' => 'Additional fortune added at request time.');

function cmp($a, $b) {
  if ($a['message'] == $b['message']) {
    return 0;
  }
  
  return ($a['message'] < $b['message']) ? -1 : 1;
}

uasort($arr, 'cmp');
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
foreach ($arr as &$value) {
?>
<tr>
<td><?php echo return htmlspecialchars($value['id'], 'ENT_COMPAT | ENT_HTML401', 'UTF-8'); ?></td>  
<td><?php echo return htmlspecialchars($value['message'], 'ENT_COMPAT | ENT_HTML401', 'UTF-8'); ?></td>
</tr>
<?php } ?>
</table>
</body>
</html>