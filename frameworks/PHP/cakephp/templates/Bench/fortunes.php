<?php
$this->autoLayout = false;
?>
<!DOCTYPE html>
<html>
<head><title>Fortunes</title></head>
<body>
<table>
    <tr>
        <th>id</th>
        <th>message</th>
    </tr>
    <?php
    foreach ($fortunes as $fortune) {
        ?>
        <tr>
            <td><?= $fortune->id ?></td>
            <td><?= h($fortune->message) ?></td>
        </tr>
    <?php } ?>
</table>
</body>
</html>
