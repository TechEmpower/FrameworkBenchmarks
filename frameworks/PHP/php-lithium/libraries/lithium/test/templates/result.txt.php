<?php

if ($success && $count['skips']) {
	echo "{:green}OK, but skipped tests.{:end}\n";
} elseif ($success) {
	echo "{:green}OK{:end}\n";
} else {
	echo "{:red}FAIL{:end}\n";
}
echo "\n";

printf(
	"%d / %d %s\n",
	$count['passes'],
	$count['asserts'],
	$count['passes'] == 1 ? 'pass' : 'passes'
);
printf(
	'%d %s',
	$count['fails'],
	$count['fails'] == 1 ? 'fail' : 'fails'
);
printf(
	" and %d %s\n",
	$count['exceptions'],
	$count['exceptions'] == 1 ? 'exception' : 'exceptions'
);

?>