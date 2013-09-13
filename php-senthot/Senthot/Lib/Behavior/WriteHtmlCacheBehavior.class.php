<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

defined('SEN_PATH') or exit();
/**
 * System behavior extension : static cache write
 * @category	Sen
 * @package		Sen
 * @subpackage  Behavior
 * @author		ms134n <ms134n@gmail.com>
 */
class WriteHtmlCacheBehavior extends Behavior {

    // Behavior extension execution entry must be run
    public function run(&$content){
        if(C('HTML_CACHE_ON') && defined('HTML_FILE_NAME'))  {
            //Static file write
            // If you turn on HTML features Check and rewrite the HTML file
            // No stencil operation does not generate static files
            if(!is_dir(dirname(HTML_FILE_NAME)))
                mkdir(dirname(HTML_FILE_NAME),0755,true);
            if( false === file_put_contents( HTML_FILE_NAME , $content ))
                throw_exception(L('_CACHE_WRITE_ERROR_').':'.HTML_FILE_NAME);
        }
    }
}