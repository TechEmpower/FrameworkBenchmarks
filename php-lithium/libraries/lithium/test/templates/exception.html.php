<div class="test-exception">
	<strong>Exception</strong> thrown in <strong><?php echo "{$error['class']}::{$error['method']}()"; ?></strong>
	on line <?php echo $error['line'] ?>
	<span class="content"><?php echo $error['message'] ?></span>
	<?php if (isset($error['trace']) && !empty($error['trace'])): ?>
		Trace
		<span class="trace"><?php echo $error['trace'] ?></span>
	<?php endif ?>
</div>