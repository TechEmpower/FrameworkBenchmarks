<?php

$summary = array(
	'classes' => 0, 'executable' => 0, 'covered' => 0, 'uncovered' => 0, 'percentage' => 0
);

?>

<h3>Code Coverage</h3>
<?php foreach ($data as $class => $coverage): ?>
	<?php
		$summary['classes']++;
		$summary['executable'] += count($coverage['executable']);
		$summary['covered'] += count($coverage['covered']);
		$summary['uncovered'] += count($coverage['uncovered']);
		$summary['percentage'] += $coverage['percentage'];
	?>
	<h4 class="coverage">
		<?php echo $class ?>:
		<?php echo count($coverage['covered']) ?> of <?php echo count($coverage['executable']) ?>
		lines covered (<em><?php echo $coverage['percentage'] ?>%</em>)
	</h4>
	<?php foreach ($coverage['output'] as $file => $data): ?>
		<?php if (!empty($data)): ?>
			<div class="code-coverage-results">
				<?php foreach ($data as $line => $row): ?>
					<div class="code-line <?php echo $row['class'] ?>">
						<span class="line-num"><?php echo $line ?></span>
						<span class="content"><?php
							echo htmlspecialchars(str_replace("\t", "	", $row['data']))
						?></span>
					</div><!-- code-line -->
				<?php endforeach ?>
			</div>
			<h4 class="code-coverage-name"><?php echo $file ?></h4>
			<!-- code-coverage-results -->
		<?php endif ?>
	<?php endforeach ?>
<?php endforeach ?>

<?php
	if (!$summary['classes'] || !$summary['executable']) {
		return;
	}
?>

<br /><br />

<h4>Summary</h4>
<table class="metrics"><tbody>
	<tr>
		<td class="metric-name">Classes Covered</td>
		<td class="metric"><?php echo $summary['classes'] ?></td>
	</tr>
	<tr>
		<td class="metric-name">Executable Lines</td>
		<td class="metric"><?php echo $summary['executable'] ?></td>
	</tr>
	<tr>
		<td class="metric-name">Lines Covered</td>
		<td class="metric"><?php echo $summary['covered'] ?></td>
	</tr>
	<tr>
		<td class="metric-name">Lines Uncovered</td>
		<td class="metric"><?php echo $summary['uncovered'] ?></td>
	</tr>
	<tr>
		<td class="metric-name">Total Coverage</td>
		<td class="metric">
			<?php echo round(($summary['covered'] / $summary['executable']) * 100, 2) ?>%
		</td>
	</tr>
	<tr>
		<td class="metric-name">Average Per Class</td>
		<td class="metric">
			<?php echo round($summary['percentage'] / $summary['classes'], 2) ?>%
		</td>
	</tr>
</tbody></table>
