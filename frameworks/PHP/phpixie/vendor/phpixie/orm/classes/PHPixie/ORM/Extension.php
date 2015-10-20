<?php 
namespace PHPixie\ORM;

/**
 * Abstract extension for the ORM Model.
 * Actual extensions should extend this class.
 *
 * @package ORM
 */
abstract class Extension {

	/**
	 * Pixie Dependancy Container
	 * @var \PHPixie\Pixie
	 */
	protected $pixie;
	
	/**
	 * Associated ORM Model
	 * @var \PHPixie\ORM\Model
	 */
	protected $model;
	
	/**
	 * Initializes the extension.
	 *
	 * @param \PHPixie\Pixie $pixie Pixie dependency container
	 */
	public function __construct($pixie, $model) {
		$this->pixie = $pixie;
		$this->model = $model;
	}
	
}