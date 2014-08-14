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
 * System behavior extension : Locate the template file
 * @category	Sen
 * @package		Sen
 * @subpackage  Behavior
 * @author		ms134n <ms134n@gmail.com>
 */
class LocationTemplateBehavior extends Behavior {
    // Behavior extension execution entry must be run
    public function run(&$templateFile){
        // Automatic positioning template file
        if(!file_exists_case($templateFile))
            $templateFile   = $this->parseTemplateFile($templateFile);
    }

    /**
     * Automatic positioning template file
     * @access private
     * @param string $templateFile File Name
     * @return string
     */
    private function parseTemplateFile($templateFile) {
        if(''==$templateFile) {
            // If the template file name is empty Positioned in accordance with the default rule
            $templateFile = C('TEMPLATE_NAME');
        }elseif(false === strpos($templateFile,C('TMPL_TEMPLATE_SUFFIX'))){
            // Parsing rules Template Theme:Module:Operating Not Support Cross-project and cross-grouping called
            $path   =  explode(':',$templateFile);
            $action = array_pop($path);
            $module = !empty($path)?array_pop($path):MODULE_NAME;
            if(!empty($path)) {// Set Template Theme
                $path = dirname(THEME_PATH).'/'.array_pop($path).'/';
            }else{
                $path = THEME_PATH;
            }
            $templateFile  =  $path.$module.C('TMPL_FILE_DEPR').$action.C('TMPL_TEMPLATE_SUFFIX');
        }
        if(!file_exists_case($templateFile))
            throw_exception(L('_TEMPLATE_NOT_EXIST_').'['.$templateFile.']');
        return $templateFile;
    }
}