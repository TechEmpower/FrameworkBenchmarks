<table>
	<tr>
		<th>id</th>
		<th>message</th>
	</tr>
	<?php
	foreach ($fortunes as $key => $fortune) {
		?>
		<tr>
			<td><?php echo $key; ?></td>
			<td><?php echo h($fortune); ?></td>
		</tr>
	<?php } ?>
</table>
