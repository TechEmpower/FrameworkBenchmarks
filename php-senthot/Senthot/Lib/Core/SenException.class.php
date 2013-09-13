<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

/**
 * Senthot system exception base class
 * @category	Sen
 * @package		Sen
 * @subpackage  Core
 * @author		ms134n <ms134n@gmail.com>
 */
class SenException extends Exception {

    /**
     * Exception Types
     * @var string
     * @access private
     */
    private $type;

    // The existence of extra debugging information
    private $extra;

    /**
     * Architecture function
     * @access public
     * @param string $message  Exception information
     */
    public function __construct($message,$code=0,$extra=false) {
        parent::__construct($message,$code);
        $this->type = get_class($this);
        $this->extra = $extra;
    }

    /**
     * Abnormal output All classes have passed __toString exception handling method output error
     * Each exception are written to the system log
     * This method can be overridden by child classes
     * @access public
     * @return array
     */
    public function __toString() {
        $trace = $this->getTrace();
        if($this->extra)
            // By throw_exception thrown to remove excess debug information
            array_shift($trace);
        $this->class    =   isset($trace[0]['class'])?$trace[0]['class']:'';
        $this->function =   isset($trace[0]['function'])?$trace[0]['function']:'';
        $this->file     =   $trace[0]['file'];
        $this->line     =   $trace[0]['line'];
        $file           =   file($this->file);
        $traceInfo      =   '';
        $time = date('y-m-d H:i:m');
        foreach($trace as $t) {
            $traceInfo .= '['.$time.'] '.$t['file'].' ('.$t['line'].') ';
            $traceInfo .= $t['class'].$t['type'].$t['function'].'(';
            $traceInfo .= implode(', ', $t['args']);
            $traceInfo .=")\n";
        }
        $error['message']   = $this->message;
        $error['type']      = $this->type;
        $error['detail']    = L('_MODULE_').'['.MODULE_NAME.'] '.L('_ACTION_').'['.ACTION_NAME.']'."\n";
        $error['detail']   .=   ($this->line-2).': '.$file[$this->line-3];
        $error['detail']   .=   ($this->line-1).': '.$file[$this->line-2];
        $error['detail']   .=   '<font color="#FF6600" >'.($this->line).': <strong>'.$file[$this->line-1].'</strong></font>';
        $error['detail']   .=   ($this->line+1).': '.$file[$this->line];
        $error['detail']   .=   ($this->line+2).': '.$file[$this->line+1];
        $error['class']     =   $this->class;
        $error['function']  =   $this->function;
        $error['file']      = $this->file;
        $error['line']      = $this->line;
        $error['trace']     = $traceInfo;

        // Record Exception Journal
        if(C('LOG_EXCEPTION_RECORD')) {
            Log::Write('('.$this->type.') '.$this->message);
        }
        return $error ;
    }
}