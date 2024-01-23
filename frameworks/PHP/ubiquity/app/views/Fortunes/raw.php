<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
<?php foreach ($fortunes as $id => $value) : ?>
<tr><td><?= $id ?></td><td><?= htmlspecialchars($value) ?></td></tr>
<?php endforeach ?></table></body></html>