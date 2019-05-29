<!DOCTYPE html>
<html>
  <head><title>Fortunes</title></head>
  <body>
    <table>
        <tr>
            <th>id</th>
            <th>message</th>
        </tr>
        <?php foreach($data['fortunes'] as $id =>$fortune){ ?>
            <tr>
                <td><?php echo $id; ?></td>
                <td><?php echo htmlspecialchars($fortune); ?></td>
            </tr>
        <?php } ?>
    </table>
  </body>
</html>
