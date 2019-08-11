<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
<?php foreach ($fortunes as $id=>$message) : ?>
<tr><td><?= $id ?></td><td><?= \htmlentities($message) ?></td></tr>
<?php endforeach ?></table></body></html>