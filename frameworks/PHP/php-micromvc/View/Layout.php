<!doctype html>
<html>
<head>
	<meta charset="utf-8"/>
	<title>MicroMVC</title>

	<!--[if lt IE 9]>
		<script src="http://html5shim.googlecode.com/svn/trunk/html5.js"></script>
	<![endif]-->

	<link rel="stylesheet" media="all" href="style.css"/>

	<?php
	//Print all CSS files
	if( ! empty($css)) foreach($css as $file) print '<link rel="stylesheet" media="all" href="'. $file. '" />';

	//Print all JS files
	if( ! empty($javascript)) foreach($javascript as $file) print '<script type="text/javascript" src="'. $file. '"></script>';

	//Print any other header data
	if( ! empty($head_data)) print $head_data;
	?>

	<meta name="viewport" content="width=device-width, initial-scale=1"/>

</head>
<body lang="en">
<?php if( ! empty($sidebar)) { ?>

	<div id="content">
		<?php print $content; ?>
	</div>

	<div id="sidebar">
		<?php print $sidebar; ?>
	</div>

<?php } else { // Else they want to do the content layout themselves... ?>

	<div id="page">
		<?php print $content; ?>
	</div>

<?php } ?>

<?php if(isset($pagination)) print $pagination;?>

<?php if(isset($debug)) print '<div id="debug">'. $debug. '</div>';?>

</body>
</html>
