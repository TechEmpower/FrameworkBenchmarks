<?php $view->extend('::base.html.php') ?>

<?php $view['slots']->set('title', 'Fortunes') ?>

<?php $view['slots']->start('body') ?>
        <?php $view['slots']->output('content') ?>
<?php $view['slots']->stop() ?>