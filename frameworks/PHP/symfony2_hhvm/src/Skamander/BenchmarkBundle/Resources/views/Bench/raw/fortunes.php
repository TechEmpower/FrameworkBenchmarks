<table>
    <tr>
        <th>id</th>
        <th>message</th>
    </tr>

    <?php foreach($fortunes as $fortune): ?>
        <tr>
            <td><?php echo $fortune->id; ?></td>
            <td><?php echo htmlspecialchars($fortune->message, ENT_QUOTES, "UTF-8", false); ?></td>
        </tr>
    <?php endforeach; ?>

</table>