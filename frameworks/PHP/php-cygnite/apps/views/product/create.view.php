<?php use Cygnite\AssetManager\Asset; ?>

<div class="pull-right">
    <?php echo Asset::link('product', 'Back', array('class' => 'btn btn-default btn-small btn-inverse')); ?>
</div>

<div style="color:#FF0000;">
    <?php echo $validation_errors; ?>
</div>

<div >
    <?php echo $form; ?>
</div>