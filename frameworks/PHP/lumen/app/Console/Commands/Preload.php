<?php
namespace App\Console\Commands;

use Illuminate\Console\Command;
use DarkGhostHunter\Preloader\Preloader;

class Preload extends Command {

	/**
	 * The name and signature of the console command.
	 *
	 * @var string
	 */
	protected $signature = 'preload';

	/**
	 * The console command description.
	 *
	 * @var string
	 */
	protected $description = 'Create an op-cache preloader file';

	/**
	 * Create a new command instance.
	 *
	 * @return void
	 */
	public function __construct() {
		parent::__construct();
	}

	/**
	 * Execute the console command.
	 *
	 * @return mixed
	 */
	public function handle() {
		$app = include './bootstrap/app.php';

		$app->run();

		Preloader::make()->whenOneIn(100)
			->memoryLimit(64)
			->writeTo('./config/preload.php');
	}
}

