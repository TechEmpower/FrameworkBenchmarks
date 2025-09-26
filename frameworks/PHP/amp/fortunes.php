<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
<?php foreach ($items as $id => $fortune): ?>
<tr><td><?= $id ?></td><td><?= \htmlspecialchars($fortune, ENT_QUOTES, 'UTF-8') ?></td></tr>
<?php endforeach ?></table></body></html>
