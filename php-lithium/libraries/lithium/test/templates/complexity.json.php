<?php

$worstOffender = null;
$averages = array();

foreach (array_slice($data['max'], 0, 10) as $method => $count) {
	if ($count <= 7) {
		continue;
	}
	$worstOffender = compact('method', 'count');
}
foreach (array_slice($data['class'], 0, 10) as $class => $count) {
	$averages[$class] = $count;
}

echo json_encode(compact('worstOffender', 'averages'));

?>