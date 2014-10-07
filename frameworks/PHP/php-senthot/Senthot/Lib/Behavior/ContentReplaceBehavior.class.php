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
 * Behavior of the system expansion: output template content replacement
 * @category	Sen
 * @package		Sen
 * @subpackage  Behavior
 * @author		ms134n <ms134n@gmail.com>
 */
class ContentReplaceBehavior extends Behavior {
    // Parameter defines the behavior
    protected $options   =  array(
        'TMPL_PARSE_STRING' =>  array(),
    );

    // Behavior extension execution entry must be run
    public function run(&$content){
        $content = $this->templateContentReplace($content);
    }

    /**
     * Replace the contents of the template
     * @access protected
     * @param string $content Template content
     * @return string
     */
    protected function templateContentReplace($content) {
        // Special variable substitution system default
        $replace =  array(
            '__TMPL__'      =>  APP_TMPL_PATH,  // Project template directory
            '__ROOT__'      =>  __ROOT__,       // Current website address
            '__APP__'       =>  __APP__,        // Current projects address
            '__GROUP__'     =>  defined('GROUP_NAME')?__GROUP__:__APP__,
            '__ACTION__'    =>  __ACTION__,     // Address the current operation
            '__SELF__'      =>  __SELF__,       // Address of the current page
            '__URL__'       =>  __URL__,
            '../Public'     =>  APP_TMPL_PATH.'Public',// Public project template directory
            '__PUBLIC__'    =>  __ROOT__.'/Public',// Public directory sites
        );
        // Allows users to customize the template string replacement
        if(is_array(C('TMPL_PARSE_STRING')) )
            $replace =  array_merge($replace,C('TMPL_PARSE_STRING'));
        $content = str_replace(array_keys($replace),array_values($replace),$content);
        return $content;
    }

}