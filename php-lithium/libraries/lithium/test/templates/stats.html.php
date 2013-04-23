<?php

$passes = intval($count['passes']) ?: 0;
$asserts = intval($count['asserts']) ?: 0;
$fails = intval($count['fails']) ?: 0;
$exceptions = intval($count['exceptions']) ?: 0;

?>
<div class="test-result test-result-<?php echo ($success ? 'success' : 'fail') ?>">
	<span class="digit"><?php echo $passes; ?></span> /
	<span class="digit"><?php echo $asserts; ?></span> <?php echo $fails == 1 ? 'pass': 'passes'; ?>,
	<span class="digit"><?php echo $fails; ?></span> <?php echo $fails == 1 ? 'fail' : 'fails'; ?>
	and <span class="digit"><?php echo $exceptions ?></span> <?php echo $exceptions == 1 ? ' exception' : ' exceptions'; ?>
</div>

<?php foreach ((array) $stats['errors'] as $error): ?>
	<?php if ($error['result'] == 'fail' || $error['result'] == 'exception'): ?>
		<?php echo $self->render("{$error['result']}", compact('error')); ?>
	<?php endif ?>
<?php endforeach ?>

<?php foreach ((array) $stats['skips'] as $skip): ?>
	<?php $trace = $skip['trace'][1]; ?>
	<div class="test-skip">
		<?php $method = $trace['function']; ?>
		<?php $test = $trace['class'] . ($method != 'skip' ? "::{$method}()" : ''); ?>
		Skipped test <?php echo $test ?>
		<span class="content"><?php echo $skip['message'] ?></span>
	</div>
<?php endforeach ?>