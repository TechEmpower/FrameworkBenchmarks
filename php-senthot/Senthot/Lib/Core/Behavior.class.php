<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

/**
 * Senthot Behavior base class
 * @category	Sen
 * @package		Sen
 * @subpackage  Core
 * @author		ms134n <ms134n@gmail.com>
 */
abstract class Behavior {

    // Behavioral parameters And the same configuration parameter settings
    protected $options =  array();

   /**
     * Architecture function
     * @access public
     */
    public function __construct() {
        if(!empty($this->options)) {
            foreach ($this->options as $name=>$val){
                if(NULL !== C($name)) { // Parameters have been set Behavioral parameters overwritten
                    $this->options[$name]  =  C($name);
                }else{ // Parameter is not set The default value is passed to the configuration
                    C($name,$val);
                }
            }
            array_change_key_case($this->options);
        }
    }
    
    // Get behavioral parameters
    public function __get($name){
        return $this->options[strtolower($name)];
    }

    /**
     * Execution behavior run method is unique interface Behavior
     * @access public
     * @param mixed $params  Behavioral parameters
     * @return void
     */
    abstract public function run(&$params);

}