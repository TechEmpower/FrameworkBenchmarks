<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2007 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

defined('SEN_PATH') or exit();
/**
 * Mysqli database driver class
 * @category	Sen
 * @package		Sen
 * @subpackage  Driver.Db
 * @author		ms134n <ms134n@gmail.com>
 */
class DbMysqli extends Db{

    /**
     * Architecture function Read the database configuration information
     * @access public
     * @param array $config Database configuration array
     */
    public function __construct($config=''){
        if ( !extension_loaded('mysqli') ) {
            throw_exception(L('_NOT_SUPPERT_').':mysqli');
        }
        if(!empty($config)) {
            $this->config   =   $config;
            if(empty($this->config['params'])) {
                $this->config['params'] =   '';
            }
        }
    }

    /**
     * Connection database method
     * @access public
     * @throws SenExecption
     */
    public function connect($config='',$linkNum=0) {
        if ( !isset($this->linkID[$linkNum]) ) {
            if(empty($config))  $config =   $this->config;
            $this->linkID[$linkNum] = new mysqli($config['hostname'],$config['username'],$config['password'],$config['database'],$config['hostport']?intval($config['hostport']):3306);
            if (mysqli_connect_errno()) throw_exception(mysqli_connect_error());
            $dbVersion = $this->linkID[$linkNum]->server_version;
            
            // Set the database encoding
            $this->linkID[$linkNum]->query("SET NAMES '".C('DB_CHARSET')."'");
            //Setup sql_model
            if($dbVersion >'5.0.1'){
                $this->linkID[$linkNum]->query("SET sql_mode=''");
            }
            // Mark connection successful
            $this->connected    =   true;
            //Unregister database security information
            if(1 != C('DB_DEPLOY_TYPE')) unset($this->config);
        }
        return $this->linkID[$linkNum];
    }

    /**
     * Query results released
     * @access public
     */
    public function free() {
        $this->queryID->free_result();
        $this->queryID = null;
    }

    /**
     * Execute the query Returns a DataSet
     * @access public
     * @param string $str  SQL commands
     * @return mixed
     */
    public function query($str) {
        $this->initConnect(false);
        if ( !$this->_linkID ) return false;
        $this->queryStr = $str;
        //Release the previous query results
        if ( $this->queryID ) $this->free();
        N('db_query',1);
        // Record the start time
        G('queryStartTime');
        $this->queryID = $this->_linkID->query($str);
        // Stored procedures improve
        if( $this->_linkID->more_results() ){
            while (($res = $this->_linkID->next_result()) != NULL) {
                $res->free_result();
            }
        }
        $this->debug();
        if ( false === $this->queryID ) {
            $this->error();
            return false;
        } else {
            $this->numRows  = $this->queryID->num_rows;
            $this->numCols    = $this->queryID->field_count;
            return $this->getAll();
        }
    }

    /**
     * Execute the statement
     * @access public
     * @param string $str  SQL commands
     * @return integer
     */
    public function execute($str) {
        $this->initConnect(true);
        if ( !$this->_linkID ) return false;
        $this->queryStr = $str;
        //Release the previous query results
        if ( $this->queryID ) $this->free();
        N('db_write',1);
        // Record the start time
        G('queryStartTime');
        $result =   $this->_linkID->query($str);
        $this->debug();
        if ( false === $result ) {
            $this->error();
            return false;
        } else {
            $this->numRows = $this->_linkID->affected_rows;
            $this->lastInsID = $this->_linkID->insert_id;
            return $this->numRows;
        }
    }

    /**
     * Start transaction
     * @access public
     * @return void
     */
    public function startTrans() {
        $this->initConnect(true);
        //Data rollback Support
        if ($this->transTimes == 0) {
            $this->_linkID->autocommit(false);
        }
        $this->transTimes++;
        return ;
    }

    /**
     * For non-autocommit State the following query submission
     * @access public
     * @return boolen
     */
    public function commit() {
        if ($this->transTimes > 0) {
            $result = $this->_linkID->commit();
            $this->_linkID->autocommit( true);
            $this->transTimes = 0;
            if(!$result){
                $this->error();
                return false;
            }
        }
        return true;
    }

    /**
     * Transaction rollback
     * @access public
     * @return boolen
     */
    public function rollback() {
        if ($this->transTimes > 0) {
            $result = $this->_linkID->rollback();
            $this->transTimes = 0;
            if(!$result){
                $this->error();
                return false;
            }
        }
        return true;
    }

    /**
     * Get all the query data
     * @access private
     * @param string $sql  SQL statement
     * @return array
     */
    private function getAll() {
        //Returns a DataSet
        $result = array();
        if($this->numRows>0) {
            //Returns a DataSet
            for($i=0;$i<$this->numRows ;$i++ ){
                $result[$i] = $this->queryID->fetch_assoc();
            }
            $this->queryID->data_seek(0);
        }
        return $result;
    }

    /**
     * Information obtained field data sheet
     * @access public
     * @return array
     */
    public function getFields($tableName) {
        $result =   $this->query('SHOW COLUMNS FROM '.$this->parseKey($tableName));
        $info   =   array();
        if($result) {
            foreach ($result as $key => $val) {
                $info[$val['Field']] = array(
                    'name'    => $val['Field'],
                    'type'    => $val['Type'],
                    'notnull' => (bool) ($val['Null'] === ''), // not null is empty, null is yes
                    'default' => $val['Default'],
                    'primary' => (strtolower($val['Key']) == 'pri'),
                    'autoinc' => (strtolower($val['Extra']) == 'auto_increment'),
                );
            }
        }
        return $info;
    }

    /**
     * Information obtained field data sheet
     * @access public
     * @return array
     */
    public function getTables($dbName='') {
        $sql    = !empty($dbName)?'SHOW TABLES FROM '.$dbName:'SHOW TABLES ';
        $result =   $this->query($sql);
        $info   =   array();
        if($result) {
            foreach ($result as $key => $val) {
                $info[$key] = current($val);
            }
        }
        return $info;
    }

    /**
     * Replace Record
     * @access public
     * @param mixed $data Data
     * @param array $options Parameter expression
     * @return false | integer
     */
    public function replace($data,$options=array()) {
        foreach ($data as $key=>$val){
            $value   =  $this->parseValue($val);
            if(is_scalar($value)) { // Filtering non-scalar data
                $values[]   =  $value;
                $fields[]   =  $this->parseKey($key);
            }
        }
        $sql   =  'REPLACE INTO '.$this->parseTable($options['table']).' ('.implode(',', $fields).') VALUES ('.implode(',', $values).')';
        return $this->execute($sql);
    }

    /**
     * Insert records
     * @access public
     * @param mixed $datas Data
     * @param array $options Parameter expression
     * @param boolean $replace Whether or replace
     * @return false | integer
     */
    public function insertAll($datas,$options=array(),$replace=false) {
        if(!is_array($datas[0])) return false;
        $fields = array_keys($datas[0]);
        array_walk($fields, array($this, 'parseKey'));
        $values  =  array();
        foreach ($datas as $data){
            $value   =  array();
            foreach ($data as $key=>$val){
                $val   =  $this->parseValue($val);
                if(is_scalar($val)) { // Filtering non-scalar data
                    $value[]   =  $val;
                }
            }
            $values[]    = '('.implode(',', $value).')';
        }
        $sql   =  ($replace?'REPLACE':'INSERT').' INTO '.$this->parseTable($options['table']).' ('.implode(',', $fields).') VALUES '.implode(',',$values);
        return $this->execute($sql);
    }

    /**
     * Close the database
     * @access public
     * @return volid
     */
    public function close() {
        if ($this->_linkID){
            $this->_linkID->close();
        }
        $this->_linkID = null;
    }

    /**
     * Database Error Messages
     * And displays the current SQL statement
     * @static
     * @access public
     * @return string
     */
    public function error() {
        $this->error = $this->_linkID->error;
        if('' != $this->queryStr){
            $this->error .= "\n [ SQL statement ] : ".$this->queryStr;
        }
        trace($this->error,'','ERR');
        return $this->error;
    }

    /**
     * SQL commands security filtering
     * @static
     * @access public
     * @param string $str  SQL commands
     * @return string
     */
    public function escapeString($str) {
        if($this->_linkID) {
            return  $this->_linkID->real_escape_string($str);
        }else{
            return addslashes($str);
        }
    }

    /**
     * Field and table names added processing`
     * @access protected
     * @param string $key
     * @return string
     */
    protected function parseKey(&$key) {
        $key   =  trim($key);
        if(!preg_match('/[,\'\"\*\(\)`.\s]/',$key)) {
           $key = '`'.$key.'`';
        }
        return $key;
    }
}