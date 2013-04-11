<?php

$i = 0;
foreach ((array) $stats['errors'] as $error) {
	$i++;

	if ($error['result'] == 'fail') {
		echo "{:red}Failed{:end} assertion {$error['assertion']}.\n";
		echo " File    : {$error['file']}\n";
		echo " Class   : {$error['class']}\n";
		echo " Method  : {$error['method']}()\n";
		echo " Line    : {$error['line']}\n";
		echo " ________\n";
		echo "{$error['message']}\n";
		echo " ________\n";
		echo "\n";
	} elseif ($error['result'] == 'exception') {
		echo "{:purple}Exception{:end} thrown.\n";
		echo " File    : {$error['file']}\n";
		echo " Class   : {$error['class']}\n";
		echo " Method  : {$error['method']}()\n";
		echo " Line    : {$error['line']}\n";
		echo " ________\n";
		echo "{$error['message']}\n";
		echo " ________\n";

		if (isset($error['trace']) && !empty($error['trace'])) {
			echo "{$error['trace']}\n";
			echo " ________\n";
		}
		echo "\n";
	}
}

?>