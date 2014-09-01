<div class="row">
	<div class="col-md-4">
		<h4>Functions</h4>
		<a href="<?php echo CCUrl::action( 'index' ); ?>" class="btn btn-default">Reload</a>
		<a href="<?php echo CCUrl::action( 'regenerate' ); ?>" class="btn btn-warning">Regenerate</a>
		<a href="<?php echo CCUrl::action( 'destroy' ); ?>" class="btn btn-danger">Destroy</a>
		<hr>
		
		<h4>Set</h4>
		<form class="form" method="post">
			<div class="form-group">
				<input class="form-control" name="key" type="text" placeholder="key">
			</div>
			<div class="form-group">
				<input class="form-control" name="value" type="text" placeholder="value">
			</div>
			<div class="form-group">
				<input class="btn btn-default" type="submit" value="Save">
			</div>
		</form>
		<hr>
		
		<h4>Switch manager</h4>
		<form class="form" method="post" action="<?php echo CCUrl::action( 'manager' ); ?>">
			<div class="form-group">
				<input class="form-control" name="name" type="text" placeholder="Session manager name">
			</div>
			<div class="form-group">
				<input class="btn btn-default" type="submit" value="Switch">
			</div>
		</form>
		
	</div>
	<div class="col-md-8">
		<h4>Data</h4>
		<pre style="overflow: auto; overflow-x:scroll;white-space: nowrap;"><?php echo nl2br( str_replace( array( " ", "\t" ), array( '&nbsp;', '&nbsp;&nbsp;' ), $data_dump ) ); ?></pre>
		
		<h4>Manager</h4>
		<pre style="overflow: auto; overflow-x:scroll;white-space: nowrap;"><?php echo nl2br( str_replace( array( " ", "\t" ), array( '&nbsp;', '&nbsp;&nbsp;' ), $config_dump ) ); ?></pre>
	</div>
</div>
