<?php /*** @var array $rows */?><!DOCTYPE html>
<html lang="en">
<head><title>Fortunes</title></head>
<body>
<table>
    <tr>
        <th>id</th>
        <th>message</th>
    </tr>
    <?php foreach ($rows as $row): ?>
        <tr>
            <td><?= \htmlspecialchars($row['id']) ?></td>
            <td><?= \htmlspecialchars($row['message']) ?></td>
        </tr>
    <?php endforeach; ?>
</table>
</body>
</html>
