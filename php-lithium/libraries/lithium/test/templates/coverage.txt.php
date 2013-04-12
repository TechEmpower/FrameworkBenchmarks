{:heading}Code Coverage{:end}
<?php

	$colorMap = array(
		'ignored' => 'white',
		'covered' => 'success',
		'uncovered' => 'error',
	);

	foreach ($data as $class => $coverage) {
		echo ($coverage['percentage'] >= 85 ? "{:success}" : "{:error}");
		echo "{$class}{:end}: ";
		echo count($coverage['covered']) . " of " . count($coverage['executable']);
		echo " lines covered (";
		echo ($coverage['percentage'] >= 85 ? "{:success}" : "{:error}");
		echo "{$coverage['percentage']}%{:end})\n";

		if ($coverage['percentage'] == 100) {
			continue;
		}
		echo "\n{:heading}Coverage analysis{:end}\n";

		foreach ($coverage['output'] as $file => $lines) {
			echo "\n{$file}:\n";

			foreach ($lines as $num => $line) {
				$color = $colorMap[$line['class']];
				echo "{:{$color}}{$num} {$line['data']}{:end}\n";
			}
		}
		echo "\n";
	}

?>