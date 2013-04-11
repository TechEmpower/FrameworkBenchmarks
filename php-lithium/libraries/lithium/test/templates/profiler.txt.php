{:heading}Benchmarks{:end}
<?php

$width = array_reduce(array_keys($data['totals']), function($v, $w) {
	return $v = (($l = strlen($w)) > $v) ? $l : $v;
});
foreach ($data['totals'] as $title => $result) {
	printf("%-{$width}s   %s\n", $title, $result['formatter']($result['value']));
}

?>