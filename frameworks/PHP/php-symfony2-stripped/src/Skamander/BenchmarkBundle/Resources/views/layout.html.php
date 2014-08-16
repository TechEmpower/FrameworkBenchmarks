<?php $view->extend('::base.html.php') ?>

<?php $view['slots']->set('title', 'Benchmark Bundle') ?>

<?php $view['slots']->start('body') ?>
    <div class="block">
        <?php $view['slots']->output('content') ?>
    </div>
<?php $view['slots']->stop() ?>