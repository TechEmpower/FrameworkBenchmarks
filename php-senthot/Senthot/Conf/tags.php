<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

// System default list of file extensions core behavior
return array(
    'app_init'      =>  array(
    ),
    'app_begin'     =>  array(
        'ReadHtmlCache', // Read static cache
    ),
    'route_check'   =>  array(
        'CheckRoute', // Routing Detection
    ), 
    'app_end'       =>  array(),
    'path_info'     =>  array(),
    'action_begin'  =>  array(),
    'action_end'    =>  array(),
    'view_begin'    =>  array(),
    'view_template' =>  array(
        'LocationTemplate', // Automatic positioning template file
    ),
    'view_parse'    =>  array(
        'ParseTemplate', // Template parsing SupportPHP, built-in template engine and third-party template engine
    ),
    'view_filter'   =>  array(
        'ContentReplace', // Replace the template output
        'TokenBuild',   // Form token
        'WriteHtmlCache', // Write static cache
        'ShowRuntime', // Running time display
    ),
    'view_end'      =>  array(
        'ShowPageTrace', // Trace display page
    ),
);