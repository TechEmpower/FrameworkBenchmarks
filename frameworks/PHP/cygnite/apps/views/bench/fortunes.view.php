<!DOCTYPE html>
<html>
<head><title>Fortunes</title></head>
<body>
<table>
    <tr>
        <th>id</th>
        <th>message</th>
    </tr>
    <?php foreach ($fortunes as $key => $fortune) { ?>
    <tr>
        <td><?php echo $fortune->id; ?></td>
        <td><?php echo htmlentities($fortune->message, ENT_QUOTES, 'UTF-8', false); ?></td>

    </tr>
    <?php } ?>

</table>
</body>
</html>