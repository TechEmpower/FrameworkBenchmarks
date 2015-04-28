<?php
/**
 * CCFTheme default configuration
 */
return array(
    
    'default' => array(
        
        /*
         * the topic gets added to the title
         */
        'topic'     => 'no title',
        
        /*
         * the html title template
         */
        'title'     => '%s | '.ClanCats::runtime( 'name' ),
        
        /*
         * the default html description
         */
        'description'   => 'Change your default description under theme.config -> defatul.description.',
        
        /*
         * sidebar ( if false full container gets used )
         */
        'sidebar'	=> false,
        
        /*
         * Footer appended scripts
         * When you have a view specifc script you can append them to the js var just like:
         *
         *     $theme->capture_append( 'js', function() {
	     *         echo "<script>console.log( 'hello world' );</script>";
         *     });
         */
        'js' => '',
    ), 
    
    /*
     * Assets configuration
     *
     * you can pack files to gether:
     * <holder>@<pack>
     */
	'assets' => array(
		// load bootstrap core
		'css/bootstrap.min.css'		=> 'theme@style',
		'css/style.css'				=> 'theme@style',

		// add mixins
		'less/mixins/mixins.less'		=> 'theme@style',
		'less/mixins/background.less'	=> 'theme@style',
		'less/mixins/css3.less'			=> 'theme@style',
		'less/mixins/transform.less'		=> 'theme@style',

		// Main style
		'less/style.less' 				=> 'theme@style',

		// js core
		'jquery.js'						=> 'vendor@lib',
		'js/bootstrap.min.js'			=> 'theme@core',
		'js/application.js'				=> 'theme@app',
	)
);