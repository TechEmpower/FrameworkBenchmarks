<?php
/**
 * Le System init
 *
 * @package 		ClanCats-Framework
 * @author     		Mario Döring <mariodoering@me.com>
 * @version 		0.4
 * @copyright 		2010 - 2013 ClanCats GmbH
 *
 */
 
/*
 * escape function shortcut
 */
if ( !function_exists( '_e' ) ) {
    function _e( $in, $recursive = false ) {
        return CCStr::htmlentities( $in, $recursive = false );
    }
}

/*
 * container callback function shortcut
 */
if ( !function_exists( '_c' ) ) {
    function _c() {
        return call_user_func_array( 'CCContainer::call', func_get_args() );
    }
}

/*
 * create url shortcut
 */
if ( !function_exists( 'to' ) ) {
    function to() {
        return call_user_func_array( 'CCUrl::to', func_get_args() );
    }
}

/**
 * Get the session fingerprint
 *
 * @param string    $name
 * @return string
 */
if ( !function_exists( 'fingerprint' ) ) 
{
    function fingerprint( $name = null ) 
    {
        return CCSession::manager( $name )->fingerprint;
    }
}

/*
 * ui maker shortcut
 */
if ( !function_exists( 'ui' ) ) 
{
    function ui() 
    {
        return call_user_func_array( "\\UI\\HTML::maker", func_get_args() );
    }
}

/*
 * translate
 */
if ( !function_exists( '__' ) ) {
    function __( $key, $params = array() ) {
        return CCLang::line( $key, $params );
    }
}

/**
 * check if we have a closure function
 */
if ( !function_exists( 'is_closure' ) ) {
    function is_closure( $t ) {
        return is_object( $t ) && ( $t instanceof Closure);
    }
}

/**
 * class dump
 */
if ( !function_exists( 'class_dump' ) ) {
    function class_dump( $class ) {
        $class = new ReflectionClass( $class );
        _d( $class->getStaticProperties() );
    }
}

/**
 * dump
 * var dump wrapper without die
 */
if ( !function_exists( '_d' ) ) {
    function _d( $var ) 
    {
        if ( ClanCats::is_cli() )
        {
            var_dump( $var );
        } else 
        {
            echo "<pre>"; var_dump( $var ); echo "</pre>";
        }
    }
}

/**
 * dump and die
 * var dump wrapper with die
 */
if ( !function_exists( '_dd' ) ) {
    function _dd( $var ) 
    {
        if ( ClanCats::is_cli() )
        {
            var_dump( $var );
        } else 
        {
            echo "<pre>"; var_dump( $var ); echo "</pre>";
        }
        die;
    }
}