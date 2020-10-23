<?php declare(strict_types=1);
// change me if you can
//var_dump(get_defined_vars());
?>
<fieldset>
	<legend><?="$error_desc($errno)"?></legend>
<pre>
<?="$error_shortfile:$errline"?>

<?=$errstr?>
</pre>
</fieldset>
