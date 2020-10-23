<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
<?php foreach ( $arr as $id => $fortune ) { ?>

<tr><td><?= $id ?></td><td><?= __h($fortune) ?></td></tr>
<?php } ?>

</table></body></html>