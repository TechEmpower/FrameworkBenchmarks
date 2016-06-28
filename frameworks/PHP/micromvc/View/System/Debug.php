<div style="margin: 60px 0; padding:2em; background:#ECF5FA; color:#000; clear:both;">

<b>Memory Usage</b>
<pre>
<?php print number_format(memory_get_usage() - START_MEMORY_USAGE); ?> bytes
<?php print number_format(memory_get_usage()); ?> bytes (process)
<?php print number_format(memory_get_peak_usage(TRUE)); ?> bytes (process peak)
</pre>

<b>Execution Time</b>
<pre><?php print round((microtime(true) - START_TIME), 5); ?> seconds</pre>

<b>URL Path</b>
<?php print dump(PATH); ?>

<b>Locale</b>
<?php print dump(Locale::getDefault()); ?>

<b>Timezone</b>
<?php print dump(date_default_timezone_get()); ?>

<?php
if(class_exists('\Micro\Database', FALSE))
{
	$highlight = function($string)
	{
		return str_replace(array("&lt;?php", "?&gt;"),'',substr(highlight_string('<?php '.$string.' ?>', TRUE),36));
	};

	foreach(\Micro\Database::$queries as $type => $queries)
	{
		print '<b>'.$type.' ('. count($queries). ' queries)</b>';
		foreach($queries as $data)
		{
			print '<pre>'. $highlight(wordwrap($data[1])."\n/* ".round(($data[0]*1000), 2).'ms */'). '</pre>';
		}
	}

	if(\Micro\Error::$found)
	{
		print '<b>Last Query Run</b>';
		print '<pre>'. $highlight(\Micro\DataBase::$last_query). '</pre>';
	}
}
?>

<?php if(!empty($_POST)) { ?>
<b>$_POST Data</b>
<?php print dump($_POST); ?>
<?php } ?>

<?php if(!empty($_GET)) { ?>
<b>$_GET Data</b>
<?php print dump($_GET); ?>
<?php } ?>

<?php if(!empty($_SESSION)) { ?>
<b>Session Data</b>
<?php print dump($_SESSION); ?>
<?php } ?>

<?php $included_files = get_included_files(); ?>
<b><?php print count($included_files); ?> PHP Files Included:</b>
<pre>
<?php foreach($included_files as $file) print str_replace(SP, '', $file). "\n"; ?>
</pre>

<b>Server Info</b>
<?php print dump($_SERVER); ?>

</div>
