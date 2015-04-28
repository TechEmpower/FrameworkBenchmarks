<h1>Mail Sandbox</h1>

<div class="row">
	
	<div class="col-md-6">
		<h4>Sending</h4>
		<form role="form" method="post">
		  <div class="form-group">
			<label for="input-to">To</label>
			<input name="to" type="email" class="form-control" id="input-to" placeholder="E-Mail">
		  </div>
		  <div class="form-group">
		  	<label for="input-bcc">BCC</label>
		  	<input name="bcc" type="email" class="form-control" id="input-bcc" placeholder="E-Mail">
		  	<small>comma separated.</small>
		  </div>
		  <div class="form-group">
			<label for="input-from">From</label>
			<input name="from" type="email" class="form-control" id="input-from" value="<?php echo $from_email; ?>" placeholder="E-Mail">
		  </div>
		  <div class="form-group">
		  	<label for="input-from_name">From name</label>
		  	<input name="from_name" type="text" class="form-control" id="input-from_name" value="<?php echo $from_name; ?>" placeholder="E-Mail">
		  </div>
		  <div class="form-group">
		   	<label for="input-subject">Subject</label>
		   	<input name="subject" type="text" class="form-control" id="input-subject" value="Dev Mail Sandbox" placeholder="E-Mail">
		  </div>
		  <div class="form-group">
		   	<label for="input-from_name">Message</label>
		   	<textarea name="message" class="form-control" rows="3"></textarea>
		  </div>
		  <div class="checkbox">
			<label>
			  <input value="1" name="plaintext" type="checkbox"> Plaintext?
			</label>
		  </div>
		  <button type="submit" class="btn btn-primary">Send</button>
		</form>
	</div>
	
	<div class="col-md-6">
		
		<h4>Environment <small><?php echo ClanCats::environment(); ?></small></h4>
		<pre style="overflow: auto; overflow-x:scroll;white-space: nowrap;"><?php echo $config_dump; ?></pre>
	</div>
</div>
