<!DOCTYPE html><html><head><title>Fortunes</title></head><body>
<table><tr><th>id</th><th>message</th></tr>
    <?php foreach($fortunes as $fortune): ?>
        <tr>
            <td><?= $fortune['id'] ?></td>
            <td><?= esc($fortune['message']) ?></td>
        </tr>
    <?php endforeach ?>
</table></body></html>