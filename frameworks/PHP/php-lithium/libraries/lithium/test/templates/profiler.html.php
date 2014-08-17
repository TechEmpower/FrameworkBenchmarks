<h3>Benchmarks</h3>
<table class="metrics"><tbody>

<?php foreach ($data['totals'] as $title => $result): ?>
	<tr>
		<td class="metric-name"><?php echo $title ?></td>
		<td class="metric"><?php echo $result['formatter']($result['value']) ?></td>
	</tr>
<?php endforeach ?>
</tbody></table>