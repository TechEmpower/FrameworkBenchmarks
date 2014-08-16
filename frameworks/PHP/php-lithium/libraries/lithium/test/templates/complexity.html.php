<h3>Cyclomatic Complexity</h3>
<table class="metrics"><tbody>
<?php foreach (array_slice($data['max'], 0, 10) as $method => $count): ?>
	<?php
		if ($count <= 7) {
			continue;
		}
	?>
	<tr>
		<td class="metric-name">Worst Offender</td>
		<td class="metric"><?php echo $method . ' - ' . $count ?></td>
	</tr>
<?php endforeach ?>
	<tr>
		<th colspan="2">Class Averages</th>
	</tr>
<?php foreach (array_slice($data['class'], 0, 10) as $class => $count): ?>
	<tr>
		<td class="metric-name"><?php echo $class ?></td>
		<td class="metric"><?php echo round($count, 2) ?></td>
	</tr>
<?php endforeach ?>
</tbody></table>