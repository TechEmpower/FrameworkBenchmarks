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
 * System behavior extension : Run time information display
 * @category	Sen
 * @package		Sen
 * @subpackage  Behavior
 * @author		ms134n <ms134n@gmail.com>
 */
class ShowRuntimeBehavior extends Behavior {
    // Parameter defines the behavior
    protected $options   =  array(
        'SHOW_RUN_TIME'		=> false,   // Running time display
        'SHOW_ADV_TIME'		=> false,   // Show detailed run-time
        'SHOW_DB_TIMES'		=> false,   // Show database queries and write times
        'SHOW_CACHE_TIMES'	=> false,   // Display the number of cache operation
        'SHOW_USE_MEM'		=> false,   // Display memory overhead
        'SHOW_LOAD_FILE'    => false,   // Display the number of files loaded
        'SHOW_FUN_TIMES'    => false ,  // Shows the number of function calls
    );

    // Behavior extension execution entry must be run
    public function run(&$content){
        if(C('SHOW_RUN_TIME')){
            if(false !== strpos($content,'{__NORUNTIME__}')) {
                $content   =  str_replace('{__NORUNTIME__}','',$content);
            }else{
                $runtime = $this->showTime();
                 if(strpos($content,'{__RUNTIME__}'))
                     $content   =  str_replace('{__RUNTIME__}',$runtime,$content);
                 else
                     $content   .=  $runtime;
            }
        }else{
            $content   =  str_replace(array('{__NORUNTIME__}','{__RUNTIME__}'),'',$content);
        }
    }

    /**
     * Show running time, database operations, the number of cache , memory usage information
     * @access private
     * @return string
     */
    private function showTime() {
        // Show Run Time
        G('beginTime',$GLOBALS['_beginTime']);
        G('viewEndTime');
        $showTime   =   'Process: '.G('beginTime','viewEndTime').'s ';
        if(C('SHOW_ADV_TIME')) {
            // Show Run Time
            $showTime .= '( Load:'.G('beginTime','loadTime').'s Init:'.G('loadTime','initTime').'s Exec:'.G('initTime','viewStartTime').'s Template:'.G('viewStartTime','viewEndTime').'s )';
        }
        if(C('SHOW_DB_TIMES') && class_exists('Db',false) ) {
            // Shows the number of database operations
            $showTime .= ' | DB :'.N('db_query').' queries '.N('db_write').' writes ';
        }
        if(C('SHOW_CACHE_TIMES') && class_exists('Cache',false)) {
            // Display cache read and write times
            $showTime .= ' | Cache :'.N('cache_read').' gets '.N('cache_write').' writes ';
        }
        if(MEMORY_LIMIT_ON && C('SHOW_USE_MEM')) {
            // Display memory overhead
            $showTime .= ' | UseMem:'. number_format((memory_get_usage() - $GLOBALS['_startUseMems'])/1024).' kb';
        }
        if(C('SHOW_LOAD_FILE')) {
            $showTime .= ' | LoadFile:'.count(get_included_files());
        }
        if(C('SHOW_FUN_TIMES')) {
            $fun  =  get_defined_functions();
            $showTime .= ' | CallFun:'.count($fun['user']).','.count($fun['internal']);
        }
        return $showTime;
    }
}