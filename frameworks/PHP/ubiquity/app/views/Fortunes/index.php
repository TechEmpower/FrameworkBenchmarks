<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
<?php foreach ($fortunes as $item) : ?>
<tr><td><?= $item->id ?></td><td><?= $item->message ?></td></tr>
<?php endforeach ?></table></body></html>