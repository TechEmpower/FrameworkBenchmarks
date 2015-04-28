<?php namespace Core;
/**
 * Array
 * php array helpers
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCArr 
{	
	/**
	 * Get the first element of an array
	 *
	 * @param array 		$array
	 * @return mixed
	 */
	public static function first( $arr ) 
	{
		return array_shift( $arr );
	}
	
	/**
	 * Get the last element of an array
	 *
	 * @param array 		$array
	 * @return mixed
	 */
	public static function last( $arr ) 
	{
		return array_pop( $arr );
	}
	
	/**
	 * Adds a single item or array of items at the end of the referenced array
	 * If you want to combine multiple arrays recursivly or use key => value pairs, please use CCArr::merge()
	 * 
	 * Example:
	 * 	$bar = array( 'bar' );
	 * 	CCArr::push( 'foo', $bar ); // $bar = array( 'bar', 'foo' )
	 * 	CCArr::push( array( 'foo', 'baz' ), $bar ); // $bar = array( 'bar', array( 'foo', 'baz' ) )
	 * 	CCArr::push( array( 'foo', 'baz' ), $bar, true ); // $bar = array( 'bar', 'foo', 'baz' )
	 *
	 * @param mixed		$item		The item you would like to add to the array
	 * @param array 	$array		The input array by reference
	 * @param bool		$merge		If $merge is set to true, push will merge each element of $item into $array
	 * @return array
 	 */
	public static function push( $item, &$arr, $merge = false ) 
	{	
		if( !is_array( $arr ) ) 
		{
			throw new \InvalidArgumentException('CCArr::push - second argument has to be an array.');
		}
		
		if ( $merge && is_array( $item ) ) 
		{
			foreach ( $item as $value ) 
			{
				$arr[] = $value;
			}	
			return $arr;
		}
		$arr[] = $item;
		return $arr;
	}
	
	/**
	 * Adds an item to an element in the array
	 * 
	 * Example:
	 *     CCArr::add( 'foo.bar', 'test' );
	 * 
	 * Results: 
	 *     array( 'foo' => array( 'bar' => array( 'test' ) ) )
	 *
	 * @param string		$key 
	 * @param mixed		$item		The item you would like to add to the array
	 * @param array 		$array
	 * @return array
 	 */
	public static function add( $key, $item, &$arr ) 
	{	
		if( !is_array( $arr ) ) 
		{
			throw new \InvalidArgumentException('CCArr::add - second argument has to be an array.');
		}
		
		if ( !is_array( static::get( $key, $arr ) ) )
		{
			return static::set( $key, array( $item ), $arr );
		}
		
		return static::set( $key, static::push( $item, static::get( $key, $arr ) ), $arr );
	}
	
	/**
	 * get a special item from every array
	 *
	 * @param mixed			$key
	 * @param array[array]	$arr
	 * @return array
	 */
	public static function pick( $key, $arr ) 
	{	
		if( !is_array( $arr ) ) 
		{
			throw new \InvalidArgumentException('CCArr::pick - second argument has to be an array.');
		}
		
		$return = array();
		
		foreach( $arr as $array ) 
		{
			$return[] = CCArr::get( $key, $array );
		}
		
		return $return;
	}
	
	/**
	 * Check if an array is multidimensional
	 * Elements with empty arrays doesn't count!
	 *
	 * Example:
	 * 	CCArr::is_multi( array( 'foo', array( 'bar', 'baz' ) ) ) === true
	 * 	CCArr::is_multi( array( array() ) ) === false
	 * 	CCArr::is_multi( false ) === false
	 * 
	 * @param array 		$arr
	 * @return bool
	 */
	public static function is_multi( $arr ) 
	{
		// if $arr isn't an array both count() will return useless values 0 (count(null)) or 1 (count(false)) and so the function will return false
		if ( count( $arr ) == count( $arr, COUNT_RECURSIVE ) ) 
		{
			return false;
		}
		return true;
	}
	
	/**
	 * Check if first element of an array is an array
	 *
	 * Example:
	 * 	CCArr::is_collection( array( 'foo', array( 'bar', 'baz' ) ) ) === false
	 * 	CCArr::is_collection( array( array() ) ) === true
	 * 	CCArr::is_collection( false ) // Exception
	 *
	 * @param array 		$arr
	 * @return bool
	 */
	public static function is_collection( $arr ) 
	{
		return is_array( reset( $arr ) );
	}
	
	/**
	 * sum items in an array or use special item in the array
	 *
	 * @param array[array]	$arr
	 * @param string			$key
	 */
	public static function sum( $arr, $key = null ) 
	{	
		if( !is_array( $arr ) ) 
		{
			throw new \InvalidArgumentException('CCArr::sum - first argument has to be an array.');
		}
		
		$sum = 0;
		
		if ( is_string( $key ) && CCArr::is_multi( $arr ) ) 
		{
			$arr = CCArr::pick( $key, $arr );
		}
		
		foreach ( $arr as $item ) 
		{
			if ( is_numeric( $item ) ) 
			{
				$sum += $item;	
			}
		}
	
		return $sum;
	}
	
	/**
	 * get the average of the items 
	 *
	 * @param array[array]	$arr
	 * @param string			$key
	 */
	public static function average( $arr, $key = null ) 
	{
		if( !is_array( $arr ) ) 
		{
			throw new \InvalidArgumentException('CCArr::average - first argunent has to be an array.');
		}
		
		if ( is_string( $key ) && CCArr::is_multi( $arr ) ) 
		{
			$arr = CCArr::pick( $key, $arr );
		}
		
		return ( static::sum( $arr ) / count( $arr ) );
	}
	
	/** 
	 * create an object from an array
	 *
	 * @param array		$array
	 * @return object
	 */
	public static function object( $arr ) 
	{
		if( !is_array( $arr ) ) 
		{
			throw new \InvalidArgumentException("CCArr::object - only arrays can be passed.");
		}
	   
		$object = new \stdClass();
		
		if ( !empty( $arr ) ) 
		{
			foreach ( $arr as $name => $value) 
			{
				if ( is_array( $value ) ) 
				{
					$value = static::object( $value );
				}
				$object->$name = $value;
			}
		}
		
		return $object;
	}
	
	/**
	 * merge arrays recursivly together
	 *
	 * @param array 		$array ...
	 * @return array
	 */ 
	public static function merge() 
	{
		// get all arguments
		$arrs = func_get_args();
		$return = array();
	
		foreach ( $arrs as $arr )
		{
			if ( !is_array( $arr ) ) 
			{
				throw new \InvalidArgumentException('CCArr::merge - all arguments have to be arrays.');
			}
	
			foreach ( $arr as $key => $value ) 
			{	
				if ( array_key_exists( $key, $return ) ) 
				{
					if ( is_array( $value ) && is_array( $return[$key] ) ) 
					{
						$value = static::merge( $return[$key], $value );
					}
				}
				$return[$key] = $value;
			}
		}
		
		return $return;
	}
	
	/**
	 * return an item from an array with dottet dimensions
	 *
	 * @param string		$key 
	 * @param array		$arr
	 * @param mixed		$default
	 * @return mixed 
	 */
	public static function get( $key, $arr, $default = null ) 
	{
		if ( isset( $arr[$key] ) ) 
		{
			return $arr[$key];
		}
		
		if ( strpos( $key, '.' ) !== false ) 
		{
			$kp = explode( '.', $key );
			
			switch ( count( $kp ) ) 
			{
				case 2:
				if ( isset( $arr[$kp[0]][$kp[1]] ) ) 
				{
					return $arr[$kp[0]][$kp[1]]; 
				}
				break;
				case 3:
				if ( isset( $arr[$kp[0]][$kp[1]][$kp[2]] ) ) 
				{
					return $arr[$kp[0]][$kp[1]][$kp[2]]; 
				}
				break;	
				case 4:
				if ( isset( $arr[$kp[0]][$kp[1]][$kp[2]][$kp[3]] ) ) 
				{
					return $arr[$kp[0]][$kp[1]][$kp[2]][$kp[3]]; 
				}
				break;
				
				// if there are more then 4 parts loop trough them
				default:
					$curr = $arr;
					foreach( $kp as $k ) 
					{
						if ( isset( $curr[$k] ) ) {
							$curr = $curr[$k];
						} else { 
							return $default; 
						}
					}
					return $curr;
				break;
			}	
		}
		
		return $default;
	}
	
	/**
	 * checks if the array has an item with dottet dimensions
	 *
	 * @param string		$key 
	 * @param array		$arr
	 * @return bool
	 */
	public static function has( $key, $arr ) 
	{	
		if ( isset( $arr[$key] ) ) 
		{
			return true;
		}
		
		if ( strpos( $key, '.' ) !== false ) 
		{
			$kp = explode( '.', $key );
			
			switch ( count( $kp ) ) {
				case 2:
					return isset( $arr[$kp[0]][$kp[1]] ); break;
				case 3:
					return isset( $arr[$kp[0]][$kp[1]][$kp[2]] ); break;	
				case 4:
					return isset( $arr[$kp[0]][$kp[1]][$kp[2]][$kp[3]] ); break;
					
				// if there are more then 4 parts loop trough them
				default:
					$curr = $arr;
					foreach( $kp as $k ) 
					{
						if ( isset( $curr[$k] ) ) {
							$curr = $curr[$k];
						} else { 
							return false; 
						}
					}
					return true;
				break;
			}
		}
		return false;
	}
	
	/**
	 * sets an item from an array with dottet dimensions
	 *
	 * @param string	$key 
	 * @param mixed		$value
	 * @param array		$arr
	 * @return array
	 */
	public static function set( $key, $value, &$arr ) 
	{	
		if ( strpos( $key, '.' ) === false ) 
		{
			$arr[$key] = $value;
			
		} 
		else 
		{
			$kp = explode( '.', $key );
			
			switch ( count( $kp ) ) 
			{
				case 2:
					$arr[$kp[0]][$kp[1]] = $value; break;
				case 3:
					$arr[$kp[0]][$kp[1]][$kp[2]] = $value; break;	
				case 4:
					$arr[$kp[0]][$kp[1]][$kp[2]][$kp[3]] = $value; break;
					
				// if there are more then 4 parts loop trough them
				default:
					$kp = array_reverse( $kp );
					$curr = $value;
					
					foreach( $kp as $k ) 
					{
						$curr = array( $k => $curr );
					}
					
					$arr = static::merge( $arr, $curr );
				break;
			}
		}
		return $arr;
	}
	
	/**
	 * deletes an item from an array with dottet dimensions
	 *
	 * @param string		$key 
	 * @param array		$arr
	 * @return void
	 */
	public static function delete( $key, &$arr ) 
	{	
		if ( isset( $arr[$key] ) ) 
		{
			unset( $arr[$key] ); return;
		}
		
		if ( strpos( $key, '.' ) !== false ) 
		{	
			$kp = explode( '.', $key );
			
			switch ( count( $kp ) ) {
				case 2:
					unset( $arr[$kp[0]][$kp[1]] ); return; break;
				case 3:
					unset( $arr[$kp[0]][$kp[1]][$kp[2]] ); return; break;
				case 4:
					unset( $arr[$kp[0]][$kp[1]][$kp[2]][$kp[3]] ); return; break;
			}
		}
	}
}