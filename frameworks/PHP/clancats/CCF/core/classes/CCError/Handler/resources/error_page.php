<!DOCTYPE html>
<html>
	<head>
		<meta charset="utf-8">
		<meta http-equiv="X-UA-Compatible" content="IE=edge">
		<title>CCF Exception</title>
		<style>
		<?php require __DIR__."/style.css"; ?>
		</style>
	</head>
	<body>
		<div id="main-container">
			<div class="content content-dark content-error clearfix">
				<h1>CCF Exception <small><?php echo $inspector->exception_name(); ?></small></h1>
			</div>
			<div class="content clearfix">
				<h2><?php echo $inspector->message(); ?></h2>
				<hr>
			</div>
			<?php foreach( $inspector->trace( true ) as $key => $trace ) : ?>
			<div class="trace-item" id="trace-item-<?php echo $key; ?>">
				<?php if ( $trace->file() !== null ) : ?>
				<div class="content content-header">
					<span class="code"><span class="root-path">.../</span><?php echo $trace->file(); ?><span style="color: #333;">:<?php echo $trace->line(); ?></span></span>
				</div>
				<!-- The trace source -->
				<div class="content content-dark clearfix">
					<?php
					$source = $trace->source();
					$start = key( $source ) + 1;
					$source = htmlentities( implode( '', $source ) ); 
					?>
					<pre class="error-source code prettyprint lang-php linenums:<?php echo $start; ?>" data-target="<?php echo $trace->line(); ?>" data-start="<?php echo $start; ?>"><?php echo $source; ?></pre>
				</div>
				<?php endif; ?>
				
				<!-- Do we have reflection information? -->
				<?php if ( $info = $trace->reflection_function_info() ) : ?>
				<hr><br><hr>
				<!-- Output reflection information -->
				<div class="content clearfix">
					<h4 class="code"><span class="main-color"><?php echo $trace->class_name(); ?></span> <strong><?php echo $trace->function_name(); ?></strong></h4>
					<!-- Output the message ( comment ) of the reflection -->
					<?php $in_code = false; $lines = explode( "\n", $info['message'] ); foreach( $lines as $index => $line )
					{
						// check if we are on the last line
						if ( array_key_exists( $index+1, $lines ) ) {
							$line = $line."\n";
						}
						
						// check if a quote starts
						if ( strpos( $line, '<pre>' ) !== false ) {
							$line = str_replace( '<pre>', '', $line ); $in_code = true;
							echo '<br></div><div class="content content-dark clearfix"><pre class="prettyprint linenums code">';
						}
						// check if a quote ends 
						elseif ( strpos( $line, '</pre>' ) !== false ) {
							$line = str_replace( '</pre>', '', $line ); $in_code = false;
							echo '</pre></div><div class="content clearfix">';
						}
						
						// dont make breaks if we are in a quote
						if ( $in_code ) {
							$line = htmlentities( $line );
						} else {
							$line = nl2br( $line );
						}
						// ouptu the line
						echo $line;
					}
					?>
					<!-- Output the parameters of the comment -->
					<div class="clearfix">
					<?php if ( array_key_exists( 'param', $info ) ) : ?>
					<div style="width: 80%; float: left; border-right: 1px solid #ddd;">
						<h4>parameters</h4>	
						<table class="table table-striped">
							<tr>
								<th>type</th>	
								<th>var</th>
								<th>comment</th>	
							</tr>
						<?php foreach( $info['param'] as $param ) :
							$param = explode( '    ', $param ); 	
						?>
							<tr>
								<td class="code main-color" ><?php echo isset( $param[0] ) ? $param[0] : ''; ?></td>	
								<td class="code"><?php echo isset( $param[1] ) ? $param[1] : ''; ?></td>	
								<td><?php echo isset( $param[2] ) ? $param[2] : ''; ?></td>	
							</tr>
						<?php endforeach; ?>
						</table>
					</div>
					<?php endif; ?>
					<!-- Output the return value of the comment -->
					<?php if ( array_key_exists( 'return', $info ) ) : ?>
					<div style="width: 19%; float: left; text-align: center;">
						<h4>returns</h4>
						<p class="main-color code"><?php echo $info['return'][0]; ?></p>
					</div>
					<?php endif; ?>
					</div>
				</div>
				<?php endif; ?>
			</div>
			<?php endforeach; ?>
			<!-- the runtime data tables -->
			
			<?php foreach( $inspector->tables() as $table_name => $data ) : ?>
			<hr><br><hr>
			<div class="content clearfix">
				<h2 class="main-color"><?php echo $table_name; ?></h2>
				<table class="code runtime-table">
				<?php foreach( $data as $key => $value ) : ?>
					<tr>
						<td class="key"><strong><?php echo $key; ?></strong></td>
						<td class="value"><?php echo $value; ?></td>
					</tr>
				<?php endforeach; ?>
				</table>
			</div>
			<?php endforeach; ?>
		</div>
		
		<!-- Our trace navigation -->
		<div id="navigation-container">
			<?php foreach( $inspector->trace( true ) as $key => $trace ) : ?>
			<a id="trace-link-<?php echo $key; ?>" href="#trace-item-<?php echo $key; ?>" class="trace-link">
				<span class="line1">
					<strong class="main-color"><?php echo $trace->class_name(); ?></strong> <?php echo $trace->function_name(); ?>
				</span>
				<?php if ( $trace->file() !== null ) : ?>
				<span class="line2">
					<span class="code"><?php echo $trace->file(); ?>:<?php echo $trace->line(); ?></span>
				</span>
				<?php endif; ?>
			</a>
			<?php endforeach; ?>
		</div>
		
		<script src="//cdnjs.cloudflare.com/ajax/libs/jquery/2.0.3/jquery.min.js"></script>
		<script src="//cdnjs.cloudflare.com/ajax/libs/prettify/r298/prettify.js"></script>
		<script>
		(function ( $ ) {
			// run code prettyfier
			prettyPrint();
			
			// highlight the target line
			$(".error-source").each( function(){
				var $this = $(this)
					index = $this.data('start')
					target = $this.data('target');
				
				$this.find( 'ol li' ).each( function() {
					if ( index == target )
					{
						$(this).addClass( 'target' );
					}
					index++;
				});
			});
			
			// display the root path on hover
			$( ".root-path" ).mouseenter( function() {
				$(this).text( '<?php echo CCROOT; ?>' );
			}).mouseleave( function() {
				$(this).text( '.../' );
			});
			
			
			// open special trace
			$(".trace-link").click( function( e ) {
				e.preventDefault();
				$(".trace-item").hide();
				$(".trace-link").removeClass("active");
				$(this).addClass('active');
				$($(this).attr('href')).show();
			});
			
			// inital trigger
			$("#trace-link-0").click();
		}( jQuery ));
		</script>
	</body>
</html>
