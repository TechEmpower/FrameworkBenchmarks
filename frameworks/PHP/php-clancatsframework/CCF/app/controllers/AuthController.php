<?php
/*
 *---------------------------------------------------------------
 * Basic Authentication Controller
 *---------------------------------------------------------------
 * 
 * This is an example that should help to explain how CCAuth 
 * and a basic controller works. Feel free to edit.
 */
class AuthController extends CCViewController 
{	
	/**
	 * Sign in action
	 *
	 * @return CCResponse
	 */
	public function action_sign_in() 
	{
		// Here we set the page topic seen in the title tag
		$this->theme->topic = __( ':action.topic' );
		
		// lets assign the view. Instead of getting the view directly
		// using CCView::create( 'path/to/view' ) we get the view from the
		// theme this allows us to have a diffrent sign_in for every theme.
		// If the view does not exist in the theme it will load the view from 
		// the default view folder.
		$this->view = $this->theme->view( 'auth/sign_in.view' );
		
		$this->view->last_identifier = CCIn::post( 'identifier' );
		
		// By checking the HTTP method we figure out if this is a post request or not.
		if ( CCIn::method( 'post' ) )
		{
			// Validate the data and get the user object.
			// We use the key "identifier" because you can configure on
			// what fields  the user is able to login. You could add for example
			// the username or the customer number etc.
			if ( $user = CCAuth::validate( CCIn::post( 'identifier' ), CCIn::post( 'password' ) ) )
			{
				// sign in the user with the current session.
				CCAuth::sign_in( $user );
				
				// flash a success message to the user that he has been
				// logged in succesfully.
				UI\Alert::flash( 'success', __( ':action.message.success' ) );
				
				// Redirect the user back to the url where he came from
				// this will only work when the next get parameter is set.
				return CCRedirect::next();
			}
			
			// If we could not recive a user object the login data were clearly invalid.
			UI\Alert::add( 'danger', __( ':action.message.invalid' ) );
		}
	}
	
	/**
	 * Sign up action
	 *
	 * @return CCResponse
	 */
	public function action_sign_up() 
	{
		// When the user is already authenticated we redirect him home.
		if ( CCAuth::valid() )
		{
			return CCRedirect::to( '/' );
		}
		
		$this->theme->topic = __( ':action.topic' );
		$this->view = $this->theme->view( 'auth/sign_up.view' );
		
		// create a new user object as data holder
		$user = new User;
		
		// bind the newly created user object to our view
		$this->view->bind( 'user', $user );
		
		if ( CCIn::method( 'post' ) )
		{
			// Lets assign the email and the password to our 
			// user object using the stirct assign method wich 
			// will ignore all other post values in the assing process.
			$user->strict_assign( array( 'email', 'password' ), CCIn::all( 'post' ) );
			
			$validator = CCValidator::post();
			
			// assign the labels to the validator this way we get 
			// correct translated error messages.
			$validator->label( array(
				'email' => __( 'model/user.label.email' ),
				'password' => __( 'model/user.label.password' ),
				'password_match' => __( 'model/user.label.password_match' )
			));
			
			// does the user already exist
			$validator->set( 'same_email', User::find( 'email', $user->email ) );
			$validator->message( __(':action.message.email_in_use'), 'negative', 'same_email' );
			
			// validate the other fields
			$validator->rules( 'email', 'required', 'email' );
			$validator->rules( 'password', 'required', 'min:6' );
			$validator->rules( 'password_match', 'required', 'match:password' );
			
			// when the data passes the validation
			if ( $validator->success() )
			{
				// because the user input is correct we can now save the 
				// object to the database and sign the user in.
				$user->save();
				
				CCAuth::sign_in( $user );
				
				UI\Alert::flash( 'success', __( ':action.message.success' ) );
				
				return CCRedirect::to( '/' );
			}
			else
			{
				UI\Alert::add( 'danger', $validator->errors() );
			}
		}
	}
	
	/**
	 * Sign out action
	 */
	public function action_sign_out() 
	{
		if ( !CCSession::valid_fingerprint() )
		{
			return CCRedirect::to( '/' );
		}
		
		CCAuth::sign_out(); return CCRedirect::to( '/' );
	}
}