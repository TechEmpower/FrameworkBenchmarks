<!DOCTYPE html>
<html>
  <head><title>Fortunes</title></head>
  <body>
    <table>
        <tr>
            <th>id</th>
            <th>message</th>
        </tr>
        <?php foreach($fortunes as $id =>$fortune) : ?>
            <tr>
                <td><?= $id ?></td>
                <td><?= htmlspecialchars($fortune) ?></td>
            </tr>
        <?php endforeach ?>
    </table>
  </body>
</html>
