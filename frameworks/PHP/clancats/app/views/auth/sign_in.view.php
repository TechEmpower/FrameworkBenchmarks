{% use UI\Form; %}
<div class="row">
	<div class="col-md-6 col-md-offset-3">

		<div class="row">
			<div class="col-sm-offset-2 col-sm-10">
				<h2>{{__( ':action.topic' )}}</h2>
			</div>
		</div>

	<!-- sing in form -->
	{{ Form::start( 'sign-in', array( 'method' => 'post', 'class' => 'form-horizontal' ) ) }}

		<!-- identifier -->
		<div class="form-group">
			{{ Form::label( 'identifier', __( 'model/user.label.email' ) )->add_class( 'col-sm-2' ) }}
			<div class="col-sm-10">
		  		{{ Form::input( 'identifier', $last_identifier )
		  			->placeholder( __( 'model/user.label.email' ) ) }}
			</div>
		</div>

		<!-- password -->
		<div class="form-group">
			{{ Form::label( 'password', __( 'model/user.label.password' ) )->add_class( 'col-sm-2' ) }}
			<div class="col-sm-10">
		  		{{ Form::input( 'password', null, 'password' )
		  			->placeholder( __( 'model/user.label.password' ) ) }}
			</div>
		</div>

		<!-- remember me -->
		<div class="form-group">
			<div class="col-sm-offset-2 col-sm-10">
				{{ Form::checkbox( 'retain', __( 'model/user.label.retain' ), true ) }}
			</div>
		</div>
		
		<!-- buttons -->
		<div class="form-group">
			<div class="col-sm-offset-2 col-sm-10">
				<button type="submit" class="btn btn-primary">{{__( ':action.topic' )}}</button>
			</div>
		</div>
		
	{{ Form::end(); }}
	</div>
</div>