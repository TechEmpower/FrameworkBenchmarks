<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

/**
 * Log processing class
 * @category	Sen
 * @package		Sen
 * @subpackage  Core
 * @author		ms134n <ms134n@gmail.com>
 */
class Log {

    // Log Level From top to bottom, from low to high
    const EMERG     = 'EMERG';  // Fatal Error: Crash the system can not be used
    const ALERT     = 'ALERT';  // Cautionary error: Errors must be immediately modified
    const CRIT      = 'CRIT';  // Threshold Error: Error exceeds the threshold , for example, 24 hours a day and 25 hours of this input
    const ERR       = 'ERR';  // General Error: Generic error
    const WARN      = 'WARN';  // Warning Error: Need to issue a warning error
    const NOTICE    = 'NOTIC';  // Notification: Program can run but not perfect error
    const INFO      = 'INFO';  // Information: Program output
    const DEBUG     = 'DEBUG';  // Debugging: Debugging information
    const SQL       = 'SQL';  // SQL: SQL Statement Note Only valid in debug mode is on

    // Log mode
    const SYSTEM    = 0;
    const MAIL      = 1;
    const FILE      = 3;
    const SAPI      = 4;

    // Log information
    static $log     =  array();

    // Date Format
    static $format  =  '[ c ]';

    /**
     * Logging And will not set the level of filtering
     * @static
     * @access public
     * @param string $message Log information
     * @param string $level  Log Level
     * @param boolean $record  Whether to force record
     * @return void
     */
    static function record($message,$level=self::ERR,$record=false) {
        if($record || false !== strpos(C('LOG_LEVEL'),$level)) {
            self::$log[] =   "{$level}: {$message}\r\n";
        }
    }

    /**
     * Log Save
     * @static
     * @access public
     * @param integer $type Log mode
     * @param string $destination  Written to the target
     * @param string $extra Additional parameters
     * @return void
     */
    static function save($type='',$destination='',$extra='') {
        if(empty(self::$log)) return ;
        $type = $type?$type:C('LOG_TYPE');
        if(self::FILE == $type) { // Papers logging information
            if(empty($destination))
                $destination = LOG_PATH.date('y_m_d').'.log';
            //Test log file size exceeds the configured size of the backup log files to regenerate
            if(is_file($destination) && floor(C('LOG_FILE_SIZE')) <= filesize($destination) )
                  rename($destination,dirname($destination).'/'.time().'-'.basename($destination));
        }else{
            $destination   =   $destination?$destination:C('LOG_DEST');
            $extra   =  $extra?$extra:C('LOG_EXTRA');
        }
        $now = date(self::$format);
        error_log($now.' '.get_client_ip().' '.$_SERVER['REQUEST_URI']."\r\n".implode('',self::$log)."\r\n", $type,$destination ,$extra);
        // After saving the log cache emptied
        self::$log = array();
        //clearstatcache();
    }

    /**
     * Log directly into
     * @static
     * @access public
     * @param string $message Log information
     * @param string $level  Log Level
     * @param integer $type Log mode
     * @param string $destination  Written to the target
     * @param string $extra Additional parameters
     * @return void
     */
    static function write($message,$level=self::ERR,$type='',$destination='',$extra='') {
        $now = date(self::$format);
        $type = $type?$type:C('LOG_TYPE');
        if(self::FILE == $type) { // Logging papers
            if(empty($destination))
                $destination = LOG_PATH.date('y_m_d').'.log';
            //Test log file size exceeds the configured size of the backup log files to regenerate
            if(is_file($destination) && floor(C('LOG_FILE_SIZE')) <= filesize($destination) )
                  rename($destination,dirname($destination).'/'.time().'-'.basename($destination));
        }else{
            $destination   =   $destination?$destination:C('LOG_DEST');
            $extra   =  $extra?$extra:C('LOG_EXTRA');
        }
        error_log("{$now} {$level}: {$message}\r\n", $type,$destination,$extra );
        //clearstatcache();
    }
}