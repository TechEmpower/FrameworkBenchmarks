<?php
class BenchController extends CCController 
{	
	/**
	 * Sign out action
	 */
	public function action_fortunes() 
	{
		$view = CCView::create( 'bench/fortune' );
		
		$fortunes = DB::select( 'Fortune' )->run();
		
		$runtimeFortune = new stdClass;
		$runtimeFortune->id = 0;
		$runtimeFortune->message = 'Additional fortune added at request time.';
		
		$fortunes[] = $runtimeFortune;
		
		usort($fortunes, function($left, $right) {
			if ($left->message === $right->message) {
				return 0;
			} else if ($left->message > $right->message) {
				return 1;
			} else {
				return -1;
			}
		});
		
		$view->fortunes = $fortunes;
		
		return CCResponse::create( $view->render() );
	}
}