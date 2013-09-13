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
 * Mysql database driver class
 * @category	Sen
 * @package		Sen
 * @subpackage  Driver.Db
 * @author		ms134n <ms134n@gmail.com>
 */
class DbMysql extends Db{

    /**
     * Architecture function Read the database configuration information
     * @access public
     * @param array $config Database configuration array
     */
    public function __construct($config=''){
        if ( !extension_loaded('mysql') ) {
            throw_exception(L('_NOT_SUPPERT_').':mysql');
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
    public function connect($config='',$linkNum=0,$force=false) {
        if ( !isset($this->linkID[$linkNum]) ) {
            if(empty($config))  $config =   $this->config;
            // Deal with the port number of the socket connection
            $host = $config['hostname'].($config['hostport']?":{$config['hostport']}":'');
            // Whether long connection
            $pconnect   = !empty($config['params']['persist'])? $config['params']['persist']:$this->pconnect;
            if($pconnect) {
                $this->linkID[$linkNum] = mysql_pconnect( $host, $config['username'], $config['password'],131072);
            }else{
                $this->linkID[$linkNum] = mysql_connect( $host, $config['username'], $config['password'],true,131072);
            }
            if ( !$this->linkID[$linkNum] || (!empty($config['database']) && !mysql_select_db($config['database'], $this->linkID[$linkNum])) ) {
                throw_exception(mysql_error());
            }
            $dbVersion = mysql_get_server_info($this->linkID[$linkNum]);
            //Access database using UTF8
            mysql_query("SET NAMES '".C('DB_CHARSET')."'", $this->linkID[$linkNum]);
            //Setup sql_model
            if($dbVersion >'5.0.1'){
                mysql_query("SET sql_mode=''",$this->linkID[$linkNum]);
            }
            // Mark connection successful
            $this->connected    =   true;
            // Unregister database connection configuration information
            if(1 != C('DB_DEPLOY_TYPE')) unset($this->config);
        }
        return $this->linkID[$linkNum];
    }

    /**
     * Query results released
     * @access public
     */
    public function free() {
        mysql_free_result($this->queryID);
        $this->queryID = null;
    }

    /**
     * Execute the query Returns a DataSet
     * @access public
     * @param string $str  SQL commands
     * @return mixed
     */
    public function query($str) {
        if(0===stripos($str, 'call')){ // Stored procedure query Support
            $this->close();
        }
        $this->initConnect(false);
        if ( !$this->_linkID ) return false;
        $this->queryStr = $str;
        //Release the previous query results
        if ( $this->queryID ) {    $this->free();    }
        N('db_query',1);
        // Record the start time
        G('queryStartTime');
        $this->queryID = mysql_query($str, $this->_linkID);
        $this->debug();
        if ( false === $this->queryID ) {
            $this->error();
            return false;
        } else {
            $this->numRows = mysql_num_rows($this->queryID);
            return $this->getAll();
        }
    }

    /**
     * Execute the statement
     * @access public
     * @param string $str  SQL commands
     * @return integer|false
     */
    public function execute($str) {
        $this->initConnect(true);
        if ( !$this->_linkID ) return false;
        $this->queryStr = $str;
        //Release the previous query results
        if ( $this->queryID ) {    $this->free();    }
        N('db_write',1);
        // Record the start time
        G('queryStartTime');
        $result =   mysql_query($str, $this->_linkID) ;
        $this->debug();
        if ( false === $result) {
            $this->error();
            return false;
        } else {
            $this->numRows = mysql_affected_rows($this->_linkID);
            $this->lastInsID = mysql_insert_id($this->_linkID);
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
        if ( !$this->_linkID ) return false;
        //Data rollback Support
        if ($this->transTimes == 0) {
            mysql_query('START TRANSACTION', $this->_linkID);
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
            $result = mysql_query('COMMIT', $this->_linkID);
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
            $result = mysql_query('ROLLBACK', $this->_linkID);
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
     * @return array
     */
    private function getAll() {
        //Returns a DataSet
        $result = array();
        if($this->numRows >0) {
            while($row = mysql_fetch_assoc($this->queryID)){
                $result[]   =   $row;
            }
            mysql_data_seek($this->queryID,0);
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
     * Obtain information about the database table
     * @access public
     * @return array
     */
    public function getTables($dbName='') {
        if(!empty($dbName)) {
           $sql    = 'SHOW TABLES FROM '.$dbName;
        }else{
           $sql    = 'SHOW TABLES ';
        }
        $result =   $this->query($sql);
        $info   =   array();
        foreach ($result as $key => $val) {
            $info[$key] = current($val);
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
                $fields[]     =  $this->parseKey($key);
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
     * @return void
     */
    public function close() {
        if ($this->_linkID){
            mysql_close($this->_linkID);
        }
        $this->_linkID = null;
    }

    /**
     * Database Error Messages
     * And displays the current SQL statement
     * @access public
     * @return string
     */
    public function error() {
        $this->error = mysql_error($this->_linkID);
        if('' != $this->queryStr){
            $this->error .= "\n [ SQL statement ] : ".$this->queryStr;
        }
        trace($this->error,'','ERR');
        return $this->error;
    }

    /**
     * SQL commands security filtering
     * @access public
     * @param string $str  SQL string
     * @return string
     */
    public function escapeString($str) {
        if($this->_linkID) {
            return mysql_real_escape_string($str,$this->_linkID);
        }else{
            return mysql_escape_string($str);
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