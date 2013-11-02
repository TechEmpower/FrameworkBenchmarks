<?php
/**
 * @package ActiveRecord
 */
namespace ActiveRecord;

/**
 * Generic base exception for all ActiveRecord specific errors.
 *
 * @package ActiveRecord
 */
class ActiveRecordException extends \Exception {};

/**
 * Thrown when a record cannot be found.
 *
 * @package ActiveRecord
 */
class RecordNotFound extends ActiveRecordException {};

/**
 * Thrown when there was an error performing a database operation.
 *
 * The error will be specific to whatever database you are running.
 *
 * @package ActiveRecord
 */
class DatabaseException extends ActiveRecordException
{
	public function __construct($adapter_or_string_or_mystery)
	{
		if ($adapter_or_string_or_mystery instanceof Connection)
		{
			parent::__construct(
				join(", ",$adapter_or_string_or_mystery->connection->errorInfo()),
				intval($adapter_or_string_or_mystery->connection->errorCode()));
		}
		elseif ($adapter_or_string_or_mystery instanceof \PDOStatement)
		{
			parent::__construct(
				join(", ",$adapter_or_string_or_mystery->errorInfo()),
				intval($adapter_or_string_or_mystery->errorCode()));
		}
		else
			parent::__construct($adapter_or_string_or_mystery);
	}
};

/**
 * Thrown by {@link Model}.
 *
 * @package ActiveRecord
 */
class ModelException extends ActiveRecordException {};

/**
 * Thrown by {@link Expressions}.
 *
 * @package ActiveRecord
 */
class ExpressionsException extends ActiveRecordException {};

/**
 * Thrown for configuration problems.
 *
 * @package ActiveRecord
 */
class ConfigException extends ActiveRecordException {};

/**
 * Thrown when attempting to access an invalid property on a {@link Model}.
 *
 * @package ActiveRecord
 */
class UndefinedPropertyException extends ModelException
{
	/**
	 * Sets the exception message to show the undefined property's name.
	 *
	 * @param str $property_name name of undefined property
	 * @return void
	 */
	public function __construct($class_name, $property_name)
	{
		if (is_array($property_name))
		{
			$this->message = implode("\r\n", $property_name);
			return;
		}

		$this->message = "Undefined property: {$class_name}->{$property_name} in {$this->file} on line {$this->line}";
		parent::__construct();
	}
};

/**
 * Thrown when attempting to perform a write operation on a {@link Model} that is in read-only mode.
 *
 * @package ActiveRecord
 */
class ReadOnlyException extends ModelException
{
	/**
	 * Sets the exception message to show the undefined property's name.
	 *
	 * @param str $class_name name of the model that is read only
	 * @param str $method_name name of method which attempted to modify the model
	 * @return void
	 */
	public function __construct($class_name, $method_name)
	{
		$this->message = "{$class_name}::{$method_name}() cannot be invoked because this model is set to read only";
		parent::__construct();
	}
};

/**
 * Thrown for validations exceptions.
 *
 * @package ActiveRecord
 */
class ValidationsArgumentError extends ActiveRecordException {};

/**
 * Thrown for relationship exceptions.
 *
 * @package ActiveRecord
 */
class RelationshipException extends ActiveRecordException {};

/**
 * Thrown for has many thru exceptions.
 *
 * @package ActiveRecord
 */
class HasManyThroughAssociationException extends RelationshipException {};
?>