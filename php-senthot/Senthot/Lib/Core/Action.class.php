<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

/**
 * Senthot Action Controller base class Abstract class
 * @category	Sen
 * @package		Sen
 * @subpackage  Core
 * @author		ms134n <ms134n@gmail.com>
 */
abstract class Action {

    /**
     * View instance
     * @var view
     * @access protected
     */    
    protected $view     =  null;

    /**
     * Current controller name
     * @var name
     * @access protected
     */      
    private   $name     =  '';

    /**
     * Template Variables
     * @var tVar
     * @access protected
     */      
    protected $tVar     =   array();

    /**
     * Controller parameters
     * @var config
     * @access protected
     */      
    protected $config   =   array();

   /**
     * Architecture function Get a template object instance
     * @access public
     */
    public function __construct() {
        tag('action_begin',$this->config);
        //Controller initialization
        if(method_exists($this,'_initialize'))
            $this->_initialize();
    }

   /**
     * Gets the name of the current Action
     * @access protected
     */
    protected function getActionName() {
        if(empty($this->name)) {
            // Get Action Name
            $this->name     =   substr(get_class($this),0,-6);
        }
        return $this->name;
    }

    /**
     * Whether AJAX request
     * @access protected
     * @return bool
     */
    protected function isAjax() {
        if(isset($_SERVER['HTTP_X_REQUESTED_WITH']) ) {
            if('xmlhttprequest' == strtolower($_SERVER['HTTP_X_REQUESTED_WITH']))
                return true;
        }
        if(!empty($_POST[C('VAR_AJAX_SUBMIT')]) || !empty($_GET[C('VAR_AJAX_SUBMIT')]))
            // Judgment Ajax submission
            return true;
        return false;
    }

    /**
     * Template Display Invoke the built-in template engine display method
     * @access protected
     * @param string $templateFile Specifies the template file to be invoked
     * The default is empty By the system automatically locates the template file
     * @param string $charset Output Coding
     * @param string $contentType Output Type
     * @param string $content Output
     * @param string $prefix Template cache prefix
     * @return void
     */
    protected function display($templateFile='',$charset='',$contentType='',$content='',$prefix='') {
        $this->initView();
        $this->view->display($templateFile,$charset,$contentType,$content,$prefix);
    }

    /**
     * Html output text can include Parse the content and Support
     * @access protected
     * @param string $content Output
     * @param string $charset Template output character set
     * @param string $contentType Output Type
     * @param string $prefix Template cache prefix
     * @return mixed
     */
    protected function show($content,$charset='',$contentType='',$prefix='') {
        $this->initView();       
        $this->view->display('',$charset,$contentType,$content,$prefix);
    }

    /**
     * Get the output page content
     * Invoke the built-in template engine fetch method
     * @access protected
     * @param string $templateFile Specifies the template file to be invoked
     * The default is empty By the system automatically locates the template file
     * @param string $content Template output
     * @param string $prefix Template cache prefix* 
     * @return string
     */
    protected function fetch($templateFile='',$content='',$prefix='') {
        $this->initView();
        return $this->view->fetch($templateFile,$content,$prefix);
    }

    /**
     * Initialize the view
     * @access private
     * @return void
     */
    private function initView(){
        //View class is instantiated
        if(!$this->view)    $this->view     = Sen::instance('View');
        // Template variables by value
        if($this->tVar)     $this->view->assign($this->tVar);           
    }
    
    /**
     * Create static pages
     * @access protected
     * @htmlfile Generated static file names
     * @htmlpath Generated static file path
     * @param string $templateFile Specifies the template file to be invoked
     * The default is empty By the system automatically locates the template file
     * @return string
     */
    protected function buildHtml($htmlfile='',$htmlpath='',$templateFile='') {
        $content = $this->fetch($templateFile);
        $htmlpath   = !empty($htmlpath)?$htmlpath:HTML_PATH;
        $htmlfile =  $htmlpath.$htmlfile.C('HTML_FILE_SUFFIX');
        if(!is_dir(dirname($htmlfile)))
            // If the static directory does not exist Is created
            mkdir(dirname($htmlfile),0755,true);
        if(false === file_put_contents($htmlfile,$content))
            throw_exception(L('_CACHE_WRITE_ERROR_').':'.$htmlfile);
        return $content;
    }

    /**
     * Template variable assignment
     * @access protected
     * @param mixed $name To display template variables
     * @param mixed $value Variable
     * @return void
     */
    protected function assign($name,$value='') {
        if(is_array($name)) {
            $this->tVar   =  array_merge($this->tVar,$name);
        }else {
            $this->tVar[$name] = $value;
        }        
    }

    public function __set($name,$value) {
        $this->assign($name,$value);
    }

    /**
     * Get template displays the values of variables
     * @access protected
     * @param string $name Template display variable
     * @return mixed
     */
    public function get($name='') {
        if('' === $name) {
            return $this->tVar;
        }
        return isset($this->tVar[$name])?$this->tVar[$name]:false;        
    }

    public function __get($name) {
        return $this->get($name);
    }

    /**
     * Detection template variable value
     * @access public
     * @param string $name Name
     * @return boolean
     */
    public function __isset($name) {
        return isset($this->tVar[$name]);
    }

    /**
     * Magic Methods There does not exist when the operation performed
     * @access public
     * @param string $method Method name
     * @param array $args Parameter
     * @return mixed
     */
    public function __call($method,$args) {
        if( 0 === strcasecmp($method,ACTION_NAME.C('ACTION_SUFFIX'))) {
            if(method_exists($this,'_empty')) {
                // If you define _empty operation the call
                $this->_empty($method,$args);
            }elseif(file_exists_case(C('TEMPLATE_NAME'))){
                // Check if there is a default template If there is a direct output template
                $this->display();
            }elseif(function_exists('__hack_action')) {
                // hack Define the extended operation
                __hack_action();
            }else{
                _404(L('_ERROR_ACTION_').':'.ACTION_NAME);
            }
        }else{
            switch(strtolower($method)) {
                // Judgment submission
                case 'ispost'   :
                case 'isget'    :
                case 'ishead'   :
                case 'isdelete' :
                case 'isput'    :
                    return strtolower($_SERVER['REQUEST_METHOD']) == strtolower(substr($method,2));
                // Get Variables Support filtering and default values Invocation $this->_post($key,$filter,$default);
                case '_get'     :   $input =& $_GET;break;
                case '_post'    :   $input =& $_POST;break;
                case '_put'     :   parse_str(file_get_contents('php://input'), $input);break;
                case '_param'   :  
                    switch($_SERVER['REQUEST_METHOD']) {
                        case 'POST':
                            $input  =  $_POST;
                            break;
                        case 'PUT':
                            parse_str(file_get_contents('php://input'), $input);
                            break;
                        default:
                            $input  =  $_GET;
                    }
                    if(C('VAR_URL_PARAMS')){
                        $params = $_GET[C('VAR_URL_PARAMS')];
                        $input  =   array_merge($input,$params);
                    }
                    break;
                case '_request' :   $input =& $_REQUEST;   break;
                case '_session' :   $input =& $_SESSION;   break;
                case '_cookie'  :   $input =& $_COOKIE;    break;
                case '_server'  :   $input =& $_SERVER;    break;
                case '_globals' :   $input =& $GLOBALS;    break;
                default:
                    throw_exception(__CLASS__.':'.$method.L('_METHOD_NOT_EXIST_'));
            }
            if(!isset($args[0])) { // Access to global variables
                $data       =   $input; // Filtered by the VAR_FILTERS configuration
            }elseif(isset($input[$args[0]])) { // Value Operation
                $data       =	$input[$args[0]];
                $filters    =   isset($args[1])?$args[1]:C('DEFAULT_FILTER');
                if($filters) {// 2012/3/23 Increase the number of ways filtration Support
                    $filters    =   explode(',',$filters);
                    foreach($filters as $filter){
                        if(function_exists($filter)) {
                            $data   =   is_array($data)?array_map($filter,$data):$filter($data); // Parameter filter
                        }
                    }
                }
            }else{ // Variable Default
                $data       =	 isset($args[2])?$args[2]:NULL;
            }
            return $data;
        }
    }

    /**
     * A quick way to jump operator error
     * @access protected
     * @param string $message Error Messages
     * @param string $jumpUrl Page jump address
     * @param mixed $ajax Whether the Ajax way Jump designated time when the digital
     * @return void
     */
    protected function error($message,$jumpUrl='',$ajax=false) {
        $this->dispatchJump($message,0,$jumpUrl,$ajax);
    }

    /**
     * Successful operation of a quick way to jump
     * @access protected
     * @param string $message Tips
     * @param string $jumpUrl Page jump address
     * @param mixed $ajax Whether the Ajax way Jump designated time when the digital
     * @return void
     */
    protected function success($message,$jumpUrl='',$ajax=false) {
        $this->dispatchJump($message,1,$jumpUrl,$ajax);
    }

    /**
     * Ajax way to return data to the client
     * @access protected
     * @param mixed $data Data to be returned
     * @param String $type AJAX return data format
     * @return void
     */
    protected function ajaxReturn($data,$type='') {
        if(func_num_args()>2) {// Compatible 2.0 before use
            $args           =   func_get_args();
            array_shift($args);
            $info           =   array();
            $info['data']   =   $data;
            $info['info']   =   array_shift($args);
            $info['status'] =   array_shift($args);
            $data           =   $info;
            $type           =   $args?array_shift($args):'';
        }
        if(empty($type)) $type  =   C('DEFAULT_AJAX_RETURN');
        switch (strtoupper($type)){
            case 'JSON' :
                // Return JSON data format to the client Contains status information
                header('Content-Type:application/json; charset=utf-8');
                exit(json_encode($data));
            case 'XML'  :
                // Back xml format data
                header('Content-Type:text/xml; charset=utf-8');
                exit(xml_encode($data));
            case 'JSONP':
                // Return JSON data format to the client Contains status information
                header('Content-Type:application/json; charset=utf-8');
                $handler  =   isset($_GET[C('VAR_JSONP_HANDLER')]) ? $_GET[C('VAR_JSONP_HANDLER')] : C('DEFAULT_JSONP_HANDLER');
                exit($handler.'('.json_encode($data).');');  
            case 'EVAL' :
                // Back js script executable
                header('Content-Type:text/html; charset=utf-8');
                exit($data);            
            default     :
                // Back to format data for the expansion of other
                tag('ajax_return',$data);
        }
    }

    /**
     * Action Jump(URL Redirection) Support jumps specified module and delay
     * @access protected
     * @param string $url Jump to URL expression
     * @param array $params Other URL parameters
     * @param integer $delay The time delay jump seconds
     * @param string $msg Jump message
     * @return void
     */
    protected function redirect($url,$params=array(),$delay=0,$msg='') {
        $url    =   U($url,$params);
        redirect($url,$delay,$msg);
    }

    /**
     * Default jump operations Support orientation and correct errors Jump
     * Call template display The default directory for the public following success page
     * Prompt page as configurable Support Template Tags
     * @param string $message Tips
     * @param Boolean $status State
     * @param string $jumpUrl Page jump address
     * @param mixed $ajax Whether the Ajax way Jump designated time when the digital
     * @access private
     * @return void
     */
    private function dispatchJump($message,$status=1,$jumpUrl='',$ajax=false) {
        if(true === $ajax || IS_AJAX) {// AJAX submit
            $data           =   is_array($ajax)?$ajax:array();
            $data['info']   =   $message;
            $data['status'] =   $status;
            $data['url']    =   $jumpUrl;
            $this->ajaxReturn($data);
        }
        if(is_int($ajax)) $this->assign('waitSecond',$ajax);
        if(!empty($jumpUrl)) $this->assign('jumpUrl',$jumpUrl);
        // Tips title
        $this->assign('msgTitle',$status? L('_OPERATION_SUCCESS_') : L('_OPERATION_FAIL_'));
        //If you set up close the window, you are prompted to close the window automatically after
        if($this->get('closeWin'))    $this->assign('jumpUrl','javascript:window.close();');
        $this->assign('status',$status);   // State
        //Ensure that the output is not affected static cache
        C('HTML_CACHE_ON',false);
        if($status) { //Information sent successfully
            $this->assign('message',$message);// Tips
            // After the successful operation of the default stay one second
            if(!isset($this->waitSecond))    $this->assign('waitSecond','1');
            // Automatically returns to the default operation is successful before the operation page
            if(!isset($this->jumpUrl)) $this->assign("jumpUrl",$_SERVER["HTTP_REFERER"]);
            $this->display(C('TMPL_ACTION_SUCCESS'));
        }else{
            $this->assign('error',$message);// Tips
            //An error occurred when the default stay 3 seconds
            if(!isset($this->waitSecond))    $this->assign('waitSecond','3');
            // Default if an error occurs automatically Back
            if(!isset($this->jumpUrl)) $this->assign('jumpUrl',"javascript:history.back(-1);");
            $this->display(C('TMPL_ACTION_ERROR'));
            // Suspend the enforcement  Avoid mistakes continue
            exit ;
        }
    }

   /**
     * Destructor
     * @access public
     */
    public function __destruct() {
        // Save the log
        if(C('LOG_RECORD')) Log::save();
        // Subsequent operations
        tag('action_end');
    }
}