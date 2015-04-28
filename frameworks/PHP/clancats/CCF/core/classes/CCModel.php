<?php namespace Core;
/**
 * Base Model 
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCModel 
{
	/**
	 * The static data array
	 *
	 * Because we don't want to generate the defauts etc. on every
	 * created model again we save them static. But because of the 
	 * behavior of php and static vars we cannot just set these in 
	 * static vars, so we add them to a static array with our class name.
	 *
	 * @var array
	 */
	public static $_static_array = array();

	/*
	 * The model defaults
	 */
	// protected static $_defaults = array();

	/*
	 * Fields that should not be returned trough 
	 * the as_array or as_json function
	 */
	// protected static $_hidden = array();

	/*
	 * Fields that should be returned trough 
	 * the as_array or as_json function even if they dont exist.
	 */
	// protected static $_visible = array();

	/**
	 * Static init
	 * 
	 * Here we prepare the model properties and settings for future use.
	 */
	public static function _init() 
	{
		if ( ( $class = get_called_class() ) == get_class() ) 
		{
			return;
		}

		// lets prepare the settings
		static::$_static_array[$class] = static::_prepare( static::$_static_array, $class );
	}

	/**
	 * Prepare the model
	 *
	 * @param string 	$settings	The model properties
	 * @param string 	$class 		The class name.
	 * @return array
	 */
	protected static function _prepare( $setting, $class )
	{
		$settings['defaults'] = array();
		$settings['types'] = array();

		// get the default's, fix them and clean them.
		if ( property_exists( $class, '_defaults') ) 
		{
			foreach( static::$_defaults as $key => $value )
			{
				if ( is_numeric( $key ) ) 
				{
					$settings['defaults'][$value] = null;
				}
				else
				{
					if ( is_array( $value ) && !empty( $value ) )
					{
						$settings['types'][$key] = array_shift( $value );
						$settings['defaults'][$key] = array_shift( $value );
					}
					else
					{
						$settings['defaults'][$key] = $value;
					}
				}
			}

			static::$_defaults = $settings['defaults'];
		}

		// add also the hidden fields properties
		if ( property_exists( $class, '_hidden') ) 
		{
			$settings['hidden'] = array_flip( static::$_hidden );
		}
		else
		{
			$settings['hidden'] = array();
		}

		// and of course the visible ones
		if ( property_exists( $class, '_visible') ) 
		{
			$settings['visible'] = array_flip( static::$_visible );
		}
		else
		{
			$settings['visible'] = array();
		}

		// check if the key property isset if not we generate one using
		// the current class name
		if ( property_exists( $class, '_name') ) 
		{
			$settings['name'] = static::$_name;
		}
		else
		{
			$settings['name'] = strtolower( str_replace( array( "\\", '_' ), '/', get_called_class() ) );

			// if it starts with model remove it
			if ( substr( $settings['name'], $length = strlen( 'model/' ) ) == 'model/' )
			{
				$settings['name'] = substr( $settings['name'], $length );
			}
		}

		return $settings;
	}

	/**
	 * Access to model's properties
	 *
	 * @param string 			$key
	 * @return array|miexed
	 */
	public static function _model( $key = null ) 
	{
		if ( $key ) 
		{
			return static::$_static_array[get_called_class()][$key];
		}
		return static::$_static_array[get_called_class()];
	}

	/**
	 * just get an empty instance
	 *
	 * @param array 	$data
	 * @return CCModel
	 */
	public static function create( $data ) 
	{
		return new static( $data );
	}

	/**
	 * Assign an model with some data
	 *
	 * @param array 			$data
	 * @return CCModel
	 */
	public static function assign( $data ) 
	{
		// return null if we have no data
		if ( $data === null || empty( $data ) ) 
		{
			return null;
		}

		// do we have a collection or a single result?
		if ( CCArr::is_collection( $data ) ) 
		{
			$return = array();

			foreach( $data as $key => $item ) 
			{
				$return[$key] = static::create( $item );
			}
			return $return;
		}

		return static::create( $data );
	}

	/**
	 * The model data store
	 *
	 * @var array
	 */
	protected $_data_store = array();

	/**
	 * model constructor
	 *
	 * @param array 		$data
	 * @return void
	 */
	public function __construct( $data = null ) 
	{
		// set the defaults first
		$this->_data_store = static::_model( 'defaults' );

		if ( !is_null( $data ) )
		{
			$this->_assign( $data );
		}
	}

	/**
	 * Get all or a single model setting
	 *
	 * @param string			$key
	 * @return mixed
	 */
	public function model_settings( $key = null )
	{
		return static::_model( $key );
	}

	/**
	 * Label translation helper 
	 *
	 * @param string 		$key
	 * @param array 			$params
	 * @return string
	 */
	public function __( $key, $params = array() )
	{
		return __( 'model/'.static::_model( 'name' ).'.label.'.$key, $params );
	}

	/**
	 * Assign an model with some data
	 *
	 * @param array 		$data
	 * @return self
	 */
	public function _assign( $data ) 
	{
		$data = $this->_before_assign( $data );

		$types = static::_model( 'types' );

		foreach( $data as $key => $value ) 
		{
			if ( array_key_exists( $key, $this->_data_store ) ) 
			{	
				// do we have to to something with the data type?
				if ( array_key_exists( $key, $types ) )
				{
					$value = $this->_type_assignment_get( $types[$key], $value );
				}

				$this->_data_store[$key] = $value;
			}
		}

		return $this;
	}

	/**
	 * Strict assign only sets some values out of an array.
	 * This can be useful to easly set some values from post.
	 *
	 * example:
	 *     $model->srtict_assign( array( 'name', 'description' ), CCIn::all( 'post' ) )
	 *
	 * @param array 			$fields
	 * @param array 			$data
	 * @return void 
	 */
	public function strict_assign( array $fields, array $data )
	{
		foreach( $fields as $field )
		{
			if ( isset( $data[$field] ) )
			{
				$this->__set( $field, $data[$field] );
			}
		}
	}

	/**
	 * Assign the data type in a set operation
	 *
	 * @param string 	$type
	 * @param mixed 		$value
	 * @return mixed
	 */
	protected function _type_assignment_set( $type, $value )
	{
		switch ( $type ) 
		{
			case 'int':
			case 'timestamp':
				return (int) $value;
			break;

			case 'string':
				return (string) $value;
			break;

			case 'bool':
				return (bool) $value;
			break;

			// json datatype simply encode
			case 'json':
				return json_encode( $value );
			break;
		}

		return $value;
	}

	/**
	 * Assign the data type in a get operation
	 *
	 * @param string 	$type
	 * @param mixed 		$value
	 * @return mixed
	 */
	protected function _type_assignment_get( $type, $value )
	{
		switch ( $type ) 
		{
			case 'int':
			case 'timestamp':
				return (int) $value;
			break;

			case 'string':
				return (string) $value;
			break;

			case 'bool':
				return (bool) $value;
			break;

			// json datatype try to decode return array on failure
			case 'json':
				if ( is_array( $value ) )
				{
					return $value;
				}
				if ( is_array( $value = json_decode( $value, true ) ) )
				{
					return $value;
				}
				return array();
			break;
		}

		return $value;
	}

	/**
	 * Assign data hook 
	 * This function lets you modify passed data before gettings assignt
	 *
	 * @param array 			$data
	 * @return array
	 */
	protected function _before_assign( $data ) { return $data; }

	/**
	 * Get a value by key from the model data
	 *
	 * @param string			$key 
	 * @return mixed
	 */
	public function &__get( $key ) 
	{
		$value = null;

		// check if the modifier exists
		$has_modifier = method_exists( $this, '_get_modifier_'.$key );

		// try getting the item
		if ( array_key_exists( $key, $this->_data_store ) ) 
		{
			if ( !$has_modifier )
			{
				return $this->_data_store[$key];
			}
			else
			{
				return $this->{'_get_modifier_'.$key}( $this->_data_store[$key] );
			}
		}

		if ( $has_modifier )
		{
			$modifier_result = $this->{'_get_modifier_'.$key}();
			return $modifier_result;
		}

		// when a function extists we forward the call
		if ( method_exists( $this, $key ) )
		{
			return $this->__call_property( $key );
		}

		throw new \InvalidArgumentException( "CCModel - Invalid or undefined model property '".$key."'." );
	}

	/**
	 * Call a function as a property
	 */
	public function __call_property( $key )
	{
		return call_user_func( array( $this, $key ) );
	}

	/**
	 * Set a value by key in the model data
	 *
	 * @param string			$key
	 * @param mixed			$value
	 * @return void
	 */
	public function __set( $key, $value ) 
	{
		if ( method_exists( $this, '_set_modifier_'.$key ) ) 
		{
			$value = $this->{'_set_modifier_'.$key}( $value );
		}

		$this->_data_store[$key] = $value;
	}

	/**
	 * Check if model data isset
	 * This also checks if a get modifier function exists which will also count as ture.
	 *
	 * @param $key
	 * @return bool
	 */
	public function __isset( $key ) 
	{
		return ( array_key_exists( $key, $this->_data_store ) || method_exists( $this, '_get_modifier_'.$key ) ) ? true : false;
	}

	/**
	 * Returns all raw data of the object or a single property
	 *
	 *     $model->raw();
	 *     $model->raw( 'key' );
	 *
	 * @param string 			$key
	 * @return array
	 */
	public function raw( $key = null ) 
	{
		if ( !is_null( $key ) )
		{
			if ( isset( $this->_data_store[$key] ) )
			{
				return $this->_data_store[$key];
			}

			throw new \InvalidArgumentException( "CCModel - Invalid or undefined model property '".$key."'." );
		}

		return $this->_data_store;
	}

	/**
	 * Set a value to the model without using the modifiers
	 *
	 *     $model->raw_set( 'key', 'value' );
	 *
	 * @param string 		$key
	 * @param string			$value
	 * @return void
	 */
	public function raw_set( $key, $value )
	{
		$this->_data_store[$key] = $value;
	}

	/**
	 * Get the object as array
	 * When $modifiers is true, then the data will be passed trough the modifiers
	 *
	 * @param bool		$modifiers
	 * @return array
	 */
	public function as_array( $modifiers = true )
	{
		$settings = static::_model();

		// get the available keys
		$keys = ( $this->_data_store );

		// remove the hidden ones
		$keys = array_diff_key( $keys, $settings['hidden'] );

		// add this moment we can simply return when 
		// we dont wish to execute the modifiers
		if ( $modifiers === false )
		{
			return $keys;
		}

		// add the visible keys
		foreach( $settings['visible'] as $key => $value )
		{
			$keys[$key] = null;
		}

		// run the modifiers
		foreach( $keys as $key => $value )
		{
			$keys[$key] = $this->$key;
		}

		return $keys;
	}

	/**
	 * Get the object as json
	 * When $modifiers is true, then the data will be passed trough the modifiers
	 *
	 * @param bool		$modifiers
	 * @param bool		$beautify
	 * @return string
	 */
	public function as_json( $modifiers = true, $beautify = true )
	{
		return CCJson::encode( $this->as_array( $modifiers ), $beautify );
	}
}