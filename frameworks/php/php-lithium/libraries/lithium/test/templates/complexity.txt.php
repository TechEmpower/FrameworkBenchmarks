{:heading}Cyclomatic Complexity{:end}
<?php
foreach (array_slice($data['max'], 0, 10) as $method => $count) {
	if ($count <= 7) {
		continue;
	}
	echo "Worst Offender\n\t{$method} - {$count}\n";
}
?>
{:heading}Class Averages{:end}
<?php
foreach (array_slice($data['class'], 0, 10) as $class => $count) {
	echo "\t{$class} - ";
	echo round($count, 2) . "\n";
}
?>