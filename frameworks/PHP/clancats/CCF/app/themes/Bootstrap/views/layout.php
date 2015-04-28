<!DOCTYPE html>
<html>
  <head>
	<meta charset="<?php echo ClanCats::$config->charset; ?>">
	<meta name="viewport" content="width=device-width, initial-scale=1.0">
	
	<title><?php echo $title; ?></title>
	<meta name="description" content="<?php echo $description; ?>">
	
	<!-- styling -->
	<?php echo CCAsset::code( 'css', 'theme' ); ?>
	<?php echo CCAsset::code( 'css' ); ?>

	<!--[if lt IE 9]>
	  <script src="https://oss.maxcdn.com/libs/html5shiv/3.7.0/html5shiv.js"></script>
	  <script src="https://oss.maxcdn.com/libs/respond.js/1.3.0/respond.min.js"></script>
	<![endif]-->
	
	<!-- header scripts -->
	<?php echo CCAsset::code( 'js', 'header' ); ?>
  </head>
  <body>
	<div id="main-container" class="container">
		<div>
			<?php if ( $sidebar !== false ) : ?>
			<div class="row">
				<div class="col-md-3">
					<?php echo $sidebar; ?>
				</div>
				<div class="col-md-9">
					<?php echo $content; ?>
				</div>
			</div>
			<?php else : ?>
				<?php echo $content; ?>
			<?php endif; ?>
		</div>
	</div>

	<!-- footer scripts -->
	<?php echo CCAsset::code( 'js', 'lib' ); ?>
	<?php echo CCAsset::code( 'js', 'theme' ); ?>
	<?php echo CCAsset::code( 'js' ); ?>
  </body>
</html>