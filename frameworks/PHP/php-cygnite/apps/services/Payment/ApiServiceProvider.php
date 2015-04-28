<?php
namespace Apps\Services\Payment;

use Cygnite\DependencyInjection\ServiceProvider;
use Cygnite\Foundation\Application;

class ApiServiceProvider extends ServiceProvider
{
	protected $container;
	
	public function register(Application $app)
	{
		$app['payment.api'] = $app->share (function($c) {
			//return new PayPal();
		});
	}
}