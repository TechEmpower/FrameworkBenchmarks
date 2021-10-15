<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
<?php foreach ($rows as $row) : ?>
<tr><td><?= $row->id ?></td><td><?= htmlspecialchars($row->message) ?></td></tr>
<?php endforeach; ?></table></body></html>