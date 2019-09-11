<?php
function fortune($pdo) {
    $statement = $pdo->query( 'SELECT id,message FROM Fortune' );
    $arr = $statement->fetchAll(PDO::FETCH_KEY_PAIR); 
    $arr[0] = 'Additional fortune added at request time.';
    asort($arr);
?>
<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
<?php foreach ( $arr as $id => $fortune ) : ?>
<tr><td><?= $id ?></td><td><?= htmlspecialchars($fortune, ENT_QUOTES, 'UTF-8'); ?></td></tr>
<?php endforeach ?></table></body></html>
<?php
}