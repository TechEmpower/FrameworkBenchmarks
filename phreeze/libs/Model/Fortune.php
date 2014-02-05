<?php
/** @package    HelloWorld::Model */

/** import supporting libraries */
require_once("DAO/FortuneDAO.php");
require_once("FortuneCriteria.php");

/**
 * The Fortune class extends FortuneDAO which provides the access
 * to the datastore.
 *
 * @package HelloWorld::Model
 * @author ClassBuilder
 * @version 1.0
 */
class Fortune extends FortuneDAO
{

	/**
	 * Override default validation
	 * @see Phreezable::Validate()
	 */
	public function Validate()
	{
		// example of custom validation
		// $this->ResetValidationErrors();
		// $errors = $this->GetValidationErrors();
		// if ($error == true) $this->AddValidationError('FieldName', 'Error Information');
		// return !$this->HasValidationErrors();

		return parent::Validate();
	}

	/**
	 * @see Phreezable::OnSave()
	 */
	public function OnSave($insert)
	{
		// the controller create/update methods validate before saving.  this will be a
		// redundant validation check, however it will ensure data integrity at the model
		// level based on validation rules.  comment this line out if this is not desired
		if (!$this->Validate()) throw new Exception('Unable to Save Fortune: ' .  implode(', ', $this->GetValidationErrors()));

		// OnSave must return true or eles Phreeze will cancel the save operation
		return true;
	}
	
	/**
	 * Override ToString for sorting purposes
	 */
	public function ToString()
	{
		return $this->Message;
	}

}

?>