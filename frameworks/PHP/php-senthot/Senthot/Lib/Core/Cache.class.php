<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

/**
 * Cache Management
 * @category	Sen
 * @package		Sen
 * @subpackage  Core
 * @author		ms134n <ms134n@gmail.com>
 */
class Cache {

    /**
     * Operating handle
     * @var string
     * @access protected
     */
    protected $handler    ;

    /**
     * Cache connection parameters
     * @var integer
     * @access protected
     */
    protected $options = array();

    /**
     * Connection cache
     * @access public
     * @param string $type Cache type
     * @param array $options  Configuration Array
     * @return object
     */
    public function connect($type='',$options=array()) {
        if(empty($type))  $type = C('DATA_CACHE_TYPE');
        $type  = strtolower(trim($type));
        $class = 'Cache'.ucwords($type);
        if(class_exists($class))
            $cache = new $class($options);
        else
            throw_exception(L('_CACHE_TYPE_INVALID_').':'.$type);
        return $cache;
    }

    public function __get($name) {
        return $this->get($name);
    }

    public function __set($name,$value) {
        return $this->set($name,$value);
    }

    public function __unset($name) {
        $this->rm($name);
    }
    public function setOptions($name,$value) {
        $this->options[$name]   =   $value;
    }

    public function getOptions($name) {
        return $this->options[$name];
    }

    /**
     * Obtain class instance cache
     * @static
     * @access public
     * @return mixed
     */
    static function getInstance() {
       $param = func_get_args();
        return get_instance_of(__CLASS__,'connect',$param);
    }

    /**
     * Queue cache
     * @access protected
     * @param string $key Queue name
     * @return mixed
     */
    // 
    protected function queue($key) {
        static $_handler = array(
            'file'  =>  array('F','F'),
            'xcache'=>  array('xcache_get','xcache_set'),
            'apc'   =>  array('apc_fetch','apc_store'),
        );
        $queue  =  isset($this->options['queue'])?$this->options['queue']:'file';
        $fun    =  isset($_handler[$queue])?$_handler[$queue]:$_handler['file'];
        $queue_name=isset($this->options['queue_name'])?$this->options['queue_name']:'sen_queue';
        $value  =  $fun[0]($queue_name);
        if(!$value) {
            $value   =  array();
        }
        // Into the column
        if(false===array_search($key, $value))  array_push($value,$key);
        if(count($value) > $this->options['length']) {
            // Dequeuing
            $key =  array_shift($value);
            // Deleting the cache
            $this->rm($key);
             if(APP_DEUBG){
                //Debug mode, recording the number of columns
                N($queue_name.'_out_times',1,true);
            }
        }
        return $fun[1]($queue_name,$value);
    }
    
    public function __call($method,$args){
        //Own method called cache type
        if(method_exists($this->handler, $method)){
           return call_user_func_array(array($this->handler,$method), $args);
        }else{
            throw_exception(__CLASS__.':'.$method.L('_METHOD_NOT_EXIST_'));
            return;
        }
    }
}