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
    ), 
);