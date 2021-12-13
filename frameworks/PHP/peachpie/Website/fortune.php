<!DOCTYPE html>
<html><head><title>Fortunes</title></head>
<body>
  <table>
  <tr><th>id</th><th>message</th></tr>
<?php table(); ?>
  </table>
</body>
</html><?php
//
// Database Test
//

function fortune() {
  // Database connection // TODO: use PDO once implemented
  $link = mysql_pconnect('tfb-database', 'benchmarkdbuser', 'benchmarkdbpass');
  mysql_select_db('hello_world', $link);
    
  // Store result in array.
  $result = mysql_query('SELECT id, message FROM Fortune', $link);
  $arr = array();
  while ($row = mysql_fetch_array($result)) {
    $arr[$row['id']] = $row['message'];
  }

  mysql_close($link);

  $arr[0] = 'Additional fortune added at request time.';

  asort($arr);
  return $arr;
}

function table() {
  foreach ( fortune() as $id => $fortune ) : ?>
<tr><td><?= $id ?></td><td><?= htmlspecialchars($fortune, ENT_QUOTES, 'UTF-8') ?></td></tr>
<?php endforeach;
}
