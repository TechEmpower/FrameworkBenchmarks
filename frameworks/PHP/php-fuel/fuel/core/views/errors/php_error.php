<div style="border: 1px solid #CCC; padding: 10px; background-color: #FFF; color: #333;">
	<p style="font-size: 13px; color: #333;"><strong><?php echo $severity; ?>!</strong></p>
	<p style="font-size: 13px; color: #333;"><?php echo $type; ?> [ <?php echo $severity; ?> ]: <?php echo $message; ?></p>
	<p style="font-size: 13px; color: #333;"><strong><?php echo $filepath; ?> @ line <?php echo $error_line; ?>:</strong></p>
	<pre style="font-size: 13px; color: #333; border: 1px solid #EEE; padding: 3px 3px 3px 8px; margin: 0;overflow: auto;"><code><?php
if (is_array($debug_lines)):
	echo ($error_line - 1).":\t".e(trim($debug_lines[$error_line - 1]))."\n";
	echo "<strong>{$error_line}:\t".e(trim($debug_lines[$error_line]))."</strong>\n";
	echo ($error_line + 1).":\t".e(trim($debug_lines[$error_line + 1]))."\n";
endif;
?></code></pre>
</div>