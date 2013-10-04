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
 * System behavior extension : Page Trace display output
 * @category	Sen
 * @package		Sen
 * @subpackage  Behavior
 * @author		ms134n <ms134n@gmail.com>
 */
class ShowPageTraceBehavior extends Behavior {
    // Parameter defines the behavior
    protected $options   =  array(
        'SHOW_PAGE_TRACE'   => false,   // Trace information display page
        'TRACE_PAGE_TABS'   => array('BASE'=>'Basic','FILE'=>'File','INFO'=>'Process','ERR|NOTIC'=>'Error','SQL'=>'SQL','DEBUG'=>'Debugging'), // Trace can be customized page tabs 
        'PAGE_TRACE_SAVE'   => false,
    );

    // Behavior extension execution entry must be run
    public function run(&$params){
        if(!IS_AJAX && C('SHOW_PAGE_TRACE')) {
            echo $this->showTrace();
        }
    }

    /**
     * Trace information display page
     * @access private
     */
    private function showTrace() {
         // The default display information
        $files  =  get_included_files();
        $info   =   array();
        foreach ($files as $key=>$file){
            $info[] = $file.' ( '.number_format(filesize($file)/1024,2).' KB )';
        }
        $trace  =   array();
        $base   =   array(
            'Request information'  =>  date('Y-m-d H:i:s',$_SERVER['REQUEST_TIME']).' '.$_SERVER['SERVER_PROTOCOL'].' '.$_SERVER['REQUEST_METHOD'].' : '.__SELF__,
            'Run time'  =>  $this->showTime(),
            'Memory overhead'  =>  MEMORY_LIMIT_ON?number_format((memory_get_usage() - $GLOBALS['_startUseMems'])/1024,2).' kb':'Not Support',
            'Query information'  =>  N('db_query').' queries '.N('db_write').' writes ',
            'File Load'  =>  count(get_included_files()),
            'Cache information'  =>  N('cache_read').' gets '.N('cache_write').' writes ',
            'Configuration is loaded'  =>  count(c()),
            'Session Information'  =>  'SESSION_ID='.session_id(),
            );
        // Reading the Trace project definition file
        $traceFile  =   CONF_PATH.'trace.php';
        if(is_file($traceFile)) {
            $base   =   array_merge($base,include $traceFile);
        }
        $debug  =   trace();
        $tabs   =   C('TRACE_PAGE_TABS');
        foreach ($tabs as $name=>$title){
            switch(strtoupper($name)) {
                case 'BASE':// Basic Information
                    $trace[$title]  =   $base;
                    break;
                case 'FILE': // File Information
                    $trace[$title]  =   $info;
                    break;
                default:// Debugging information
                    $name       =   strtoupper($name);
                    if(strpos($name,'|')) {// Multiple sets of information
                        $array  =   explode('|',$name);
                        $result =   array();
                        foreach($array as $name){
                            $result   +=   isset($debug[$name])?$debug[$name]:array();
                        }
                        $trace[$title]  =   $result;
                    }else{
                        $trace[$title]  =   isset($debug[$name])?$debug[$name]:'';
                    }
            }
        }
        if($save = C('PAGE_TRACE_SAVE')) { // Save Page Trace Logs
            if(is_array($save)) {// Select the tab to save
                $tabs   =   C('TRACE_PAGE_TABS');
                $array  =   array();
                foreach ($save as $tab){
                    $array[] =   $tabs[$tab];
                }
            }
            $content    =   date('[ c ]').' '.get_client_ip().' '.$_SERVER['REQUEST_URI']."\r\n";
            foreach ($trace as $key=>$val){
                if(!isset($array) || in_array($key,$array)) {
                    $content    .=  '[ '.$key." ]\r\n";
                    if(is_array($val)) {
                        foreach ($val as $k=>$v){
                            $content .= (!is_numeric($k)?$k.':':'').print_r($v,true)."\r\n";
                        }
                    }else{
                        $content .= print_r($val,true)."\r\n";
                    }
                    $content .= "\r\n";
                }
            }
            error_log(str_replace('<br/>',"\r\n",$content), Log::FILE,LOG_PATH.date('y_m_d').'_trace.log');
        }
        unset($files,$info,$base);
        // Call Trace page template
        ob_start();
        include C('TMPL_TRACE_FILE')?C('TMPL_TRACE_FILE'):SEN_PATH.'Tpl/page_trace.tpl';
        return ob_get_clean();
    }

    /**
     * Get running time
     */
    private function showTime() {
        // Show Run Time
        G('beginTime',$GLOBALS['_beginTime']);
        G('viewEndTime');
        // Show Run Time
        return G('beginTime','viewEndTime').'s ( Load:'.G('beginTime','loadTime').'s Init:'.G('loadTime','initTime').'s Exec:'.G('initTime','viewStartTime').'s Template:'.G('viewStartTime','viewEndTime').'s )';
    }
}