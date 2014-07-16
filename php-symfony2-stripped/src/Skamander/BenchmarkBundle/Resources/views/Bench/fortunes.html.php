<?php $view->extend('SkamanderBenchmarkBundle::layout.html.php') ?>

<?php $view['slots']->set('title', 'Fortunes') ?>

<?php $view['slots']->start('content') ?>
<table>
    <tr>
        <th>id</th>
        <th>message</th>
    </tr>
    <?php foreach($fortunes as $fortune): ?>
        <tr>
            <td><?php echo $view->escape($fortune->id); ?></td>
            <td><?php echo $view->escape($fortune->message); ?></td>
        </tr>
    <?php endforeach; ?>
</table>
<?php $view['slots']->stop() ?>