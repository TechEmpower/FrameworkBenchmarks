<div class="test-assert test-assert-failed">
	Assertion '<strong><?php echo $error['assertion'] ?></strong>' failed in
	<strong><?php echo $error['class'] ?>::<?php echo $error['method']?>()</strong>
	on line <?php echo $error['line'] ?>:

	<span class="content"><?php echo htmlspecialchars($error['message'], ENT_QUOTES, 'UTF-8') ?></span>
</div>