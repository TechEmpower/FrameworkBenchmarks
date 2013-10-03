<?php
// +--------------------------------------------------------------------------
// | Senthot [ DEVELOPED BY ME ]
// +--------------------------------------------------------------------------
// | Copyright (c) 2005-2013 http://www.senthot.com All rights reserved.
// | License ( http://www.apache.org/licenses/LICENSE-2.0 )
// | Author: ms134n ( ms134n@gmail.com )
// +--------------------------------------------------------------------------

/**
 * Senthot Model class that
 * ORM and realized ActiveRecords mode
 * @category	Sen
 * @package		Sen
 * @subpackage  Core
 * @author		ms134n <ms134n@gmail.com>
 */
class Model {
    // Operation Status
    const MODEL_INSERT          =   1;      //  Insert model data
    const MODEL_UPDATE          =   2;      //  Updating model data
    const MODEL_BOTH            =   3;      //  Contains the above two methods
    const MUST_VALIDATE         =   1;// Must verify
    const EXISTS_VALIDATE       =   0;// There is a form field validation
    const VALUE_VALIDATE        =   2;// Not empty form value validation
    // Addonsed model currently in use
    private   $_extModel        =   null;
    // Current object database operations
    protected $db               =   null;
    // Primary key name
    protected $pk               =   'id';
    // Table Prefix
    protected $tablePrefix      =   '';
    // Model Name
    protected $name             =   '';
    // Database Name
    protected $dbName           =   '';
    //Database configuration
    protected $connection       =   '';
    // Data table name ( does not contain a table prefix )
    protected $tableName        =   '';
    // Actual data table name ( including table prefix )
    protected $trueTableName    =   '';
    // Recent error message
    protected $error            =   '';
    // Field Information
    protected $fields           =   array();
    // Data information
    protected $data             =   array();
    // Query expression parameter
    protected $options          =   array();
    protected $_validate        =   array();  // Automatic verification Definition
    protected $_auto            =   array();  // Auto-complete definitions
    protected $_map             =   array();  // Field mapping definition
    protected $_scope           =   array();  // Named range definition
    // Whether to automatically detect the data table field information
    protected $autoCheckFields  =   true;
    // Whether batch verification
    protected $patchValidate    =   false;
    // Chain operation method list
    protected $methods          =   array('table','order','alias','having','group','lock','distinct','auto','filter','validate');

    /**
     * Architecture function
     * Obtain DB class instance object Field inspection
     * @access public
     * @param string $name Model Name
     * @param string $tablePrefix Table Prefix
     * @param mixed $connection Database connection information
     */
    public function __construct($name='',$tablePrefix='',$connection='') {
        // Model initialization
        $this->_initialize();
        // Get the model name
        if(!empty($name)) {
            if(strpos($name,'.')) { // Support Database name. Model name Definition
                list($this->dbName,$this->name) = explode('.',$name);
            }else{
                $this->name   =  $name;
            }
        }elseif(empty($this->name)){
            $this->name =   $this->getModelName();
        }
        // Set Table Prefix
        if(is_null($tablePrefix)) {// Null means no prefix prefix
            $this->tablePrefix = '';
        }elseif('' != $tablePrefix) {
            $this->tablePrefix = $tablePrefix;
        }else{
            $this->tablePrefix = $this->tablePrefix?$this->tablePrefix:C('DB_PREFIX');
        }

        // Database initialization
        // Gets an object database operations
        // Current model has a separate database connection information
        $this->db(0,empty($this->connection)?$connection:$this->connection);
    }

    /**
     * Automatic detection of data table information
     * @access protected
     * @return void
     */
    protected function _checkTableInfo() {
        // If not Model class Automatically record the data table information
        // Only the first execution record
        if(empty($this->fields)) {
            // If the data table field is not defined is automatically acquired
            if(C('DB_FIELDS_CACHE')) {
                $db   =  $this->dbName?$this->dbName:C('DB_NAME');
                $fields = F('_fields/'.strtolower($db.'.'.$this->name));
                if($fields) {
                    $version    =   C('DB_FIELD_VERISON');
                    if(empty($version) || $fields['_version']== $version) {
                        $this->fields   =   $fields;
                        return ;
                    }
                }
            }
            // Read data every time table information
            $this->flush();
        }
    }

    /**
     * Get field information and cache
     * @access public
     * @return void
     */
    public function flush() {
        // Cache does not exist query data table information
        $this->db->setModel($this->name);
        $fields =   $this->db->getFields($this->getTableName());
        if(!$fields) { // Unable to get field information
            return false;
        }
        $this->fields   =   array_keys($fields);
        $this->fields['_autoinc'] = false;
        foreach ($fields as $key=>$val){
            // Record Field Type
            $type[$key]    =   $val['type'];
            if($val['primary']) {
                $this->fields['_pk'] = $key;
                if($val['autoinc']) $this->fields['_autoinc']   =   true;
            }
        }
        // Record field type information
        $this->fields['_type'] =  $type;
        if(C('DB_FIELD_VERISON')) $this->fields['_version'] =   C('DB_FIELD_VERISON');

        // 2008-3-7 Increase the cache switch control
        if(C('DB_FIELDS_CACHE')){
            // Persistent cache data sheet information
            $db   =  $this->dbName?$this->dbName:C('DB_NAME');
            F('_fields/'.strtolower($db.'.'.$this->name),$this->fields);
        }
    }

    /**
     * Dynamic switching extended model
     * @access public
     * @param string $type Model type name
     * @param mixed $vars To addons the model attribute variables passed
     * @return Model
     */
    public function switchModel($type,$vars=array()) {
        $class = ucwords(strtolower($type)).'Model';
        if(!class_exists($class))
            throw_exception($class.L('_MODEL_NOT_EXIST_'));
        // Addonsed model instantiation
        $this->_extModel   = new $class($this->name);
        if(!empty($vars)) {
            // Incoming current model to addons the model attributes
            foreach ($vars as $var)
                $this->_extModel->setProperty($var,$this->$var);
        }
        return $this->_extModel;
    }

    /**
     * Set the value of the data object
     * @access public
     * @param string $name Name
     * @param mixed $value Value
     * @return void
     */
    public function __set($name,$value) {
        // Setting Data object properties
        $this->data[$name]  =   $value;
    }

    /**
     * Gets the value of the data object
     * @access public
     * @param string $name Name
     * @return mixed
     */
    public function __get($name) {
        return isset($this->data[$name])?$this->data[$name]:null;
    }

    /**
     * Detecting the value of the data object
     * @access public
     * @param string $name Name
     * @return boolean
     */
    public function __isset($name) {
        return isset($this->data[$name]);
    }

    /**
     * Destroy the value of the data object
     * @access public
     * @param string $name Name
     * @return void
     */
    public function __unset($name) {
        unset($this->data[$name]);
    }

    /**
     * Use __call method to achieve some special Model Method
     * @access public
     * @param string $method Method name
     * @param array $args Call parameters
     * @return mixed
     */
    public function __call($method,$args) {
        if(in_array(strtolower($method),$this->methods,true)) {
            // Coherent implementation of the operation
            $this->options[strtolower($method)] =   $args[0];
            return $this;
        }elseif(in_array(strtolower($method),array('count','sum','min','max','avg'),true)){
            // Achieve statistical inquiry
            $field =  isset($args[0])?$args[0]:'*';
            return $this->getField(strtoupper($method).'('.$field.') AS tp_'.$method);
        }elseif(strtolower(substr($method,0,5))=='getby') {
            // According to a field to obtain records
            $field   =   parse_name(substr($method,5));
            $where[$field] =  $args[0];
            return $this->where($where)->find();
        }elseif(strtolower(substr($method,0,10))=='getfieldby') {
            // According to a field to a value obtained records
            $name   =   parse_name(substr($method,10));
            $where[$name] =$args[0];
            return $this->where($where)->getField($args[1]);
        }elseif(isset($this->_scope[$method])){// Individual named range called Support
            return $this->scope($method,$args[0]);
        }else{
            throw_exception(__CLASS__.':'.$method.L('_METHOD_NOT_EXIST_'));
            return;
        }
    }
    // Callback method Initialize model
    protected function _initialize() {}

    /**
     * The data saved to the database for processing
     * @access protected
     * @param mixed $data The data to be operated
     * @return boolean
     */
     protected function _facade($data) {
        // Check the non-data fields
        if(!empty($this->fields)) {
            foreach ($data as $key=>$val){
                if(!in_array($key,$this->fields,true)){
                    unset($data[$key]);
                }elseif(is_scalar($val)) {
                    // Field type checking
                    $this->_parseType($data,$key);
                }
            }
        }
        // Security filtering
        if(!empty($this->options['filter'])) {
            $data = array_map($this->options['filter'],$data);
            unset($this->options['filter']);
        }
        $this->_before_write($data);
        return $data;
     }

    // Callback method before writing data Including new and updated
    protected function _before_write(&$data) {}

    /**
     * Added Data
     * @access public
     * @param mixed $data Data
     * @param array $options Expression
     * @param boolean $replace Whether or replace
     * @return mixed
     */
    public function add($data='',$options=array(),$replace=false) {
        if(empty($data)) {
            // No transmission of data , access to the current value of the data object
            if(!empty($this->data)) {
                $data           =   $this->data;
                // Reset Data
                $this->data     = array();
            }else{
                $this->error    = L('_DATA_TYPE_INVALID_');
                return false;
            }
        }
        // Analysis Expressions
        $options    =   $this->_parseOptions($options);
        // Data Processing
        $data       =   $this->_facade($data);
        if(false === $this->_before_insert($data,$options)) {
            return false;
        }
        // Write data to the database
        $result = $this->db->insert($data,$options,$replace);
        if(false !== $result ) {
            $insertId   =   $this->getLastInsID();
            if($insertId) {
                // Increment primary key to return to insert ID
                $data[$this->getPk()]  = $insertId;
                $this->_after_insert($data,$options);
                return $insertId;
            }
            $this->_after_insert($data,$options);
        }
        return $result;
    }
    // Callback method before inserting data
    protected function _before_insert(&$data,$options) {}
    // Inserted after the success callback method
    protected function _after_insert($data,$options) {}

    public function addAll($dataList,$options=array(),$replace=false){
        if(empty($dataList)) {
            $this->error = L('_DATA_TYPE_INVALID_');
            return false;
        }
        // Analysis Expressions
        $options =  $this->_parseOptions($options);
        // Data Processing
        foreach ($dataList as $key=>$data){
            $dataList[$key] = $this->_facade($data);
        }
        // Write data to the database
        $result = $this->db->insertAll($dataList,$options,$replace);
        if(false !== $result ) {
            $insertId   =   $this->getLastInsID();
            if($insertId) {
                return $insertId;
            }
        }
        return $result;
    }

    /**
     * Added through Select records
     * @access public
     * @param string $fields To insert a data table field names
     * @param string $table To insert the data table name
     * @param array $options Expression
     * @return boolean
     */
    public function selectAdd($fields='',$table='',$options=array()) {
        // Analysis Expressions
        $options =  $this->_parseOptions($options);
        // Write data to the database
        if(false === $result = $this->db->selectInsert($fields?$fields:$options['field'],$table?$table:$this->getTableName(),$options)){
            // Database insert operation failed
            $this->error = L('_OPERATION_WRONG_');
            return false;
        }else {
            // Successful insertion
            return $result;
        }
    }

    /**
     * Saving Data
     * @access public
     * @param mixed $data Data
     * @param array $options Expression
     * @return boolean
     */
    public function save($data='',$options=array()) {
        if(empty($data)) {
            // No transmission of data , access to the current value of the data object
            if(!empty($this->data)) {
                $data           =   $this->data;
                // Reset Data
                $this->data     =   array();
            }else{
                $this->error    =   L('_DATA_TYPE_INVALID_');
                return false;
            }
        }
        // Data Processing
        $data       =   $this->_facade($data);
        // Analysis Expressions
        $options    =   $this->_parseOptions($options);
        if(false === $this->_before_update($data,$options)) {
            return false;
        }
        if(!isset($options['where']) ) {
            // If there is a primary key data Is automatically updated as conditions
            if(isset($data[$this->getPk()])) {
                $pk                 =   $this->getPk();
                $where[$pk]         =   $data[$pk];
                $options['where']   =   $where;
                $pkValue            =   $data[$pk];
                unset($data[$pk]);
            }else{
                // If there are any updated conditions does
                $this->error        =   L('_OPERATION_WRONG_');
                return false;
            }
        }
        $result     =   $this->db->update($data,$options);
        if(false !== $result) {
            if(isset($pkValue)) $data[$pk]   =  $pkValue;
            $this->_after_update($data,$options);
        }
        return $result;
    }
    // The callback method before the data is updated
    protected function _before_update(&$data,$options) {}
    // After a successful update callback methods
    protected function _after_update($data,$options) {}

    /**
     * Deleting data
     * @access public
     * @param mixed $options Expression
     * @return mixed
     */
    public function delete($options=array()) {
        if(empty($options) && empty($this->options['where'])) {
            // If the deletion criteria is empty Delete object corresponding to the current data record
            if(!empty($this->data) && isset($this->data[$this->getPk()]))
                return $this->delete($this->data[$this->getPk()]);
            else
                return false;
        }
        if(is_numeric($options)  || is_string($options)) {
            // Based on primary keys deleting records
            $pk   =  $this->getPk();
            if(strpos($options,',')) {
                $where[$pk]     =  array('IN', $options);
            }else{
                $where[$pk]     =  $options;
            }
            $pkValue            =  $where[$pk];
            $options            =  array();
            $options['where']   =  $where;
        }
        // Analysis Expressions
        $options =  $this->_parseOptions($options);
        $result=    $this->db->delete($options);
        if(false !== $result) {
            $data = array();
            if(isset($pkValue)) $data[$pk]   =  $pkValue;
            $this->_after_delete($data,$options);
        }
        // Returns the deleted records number
        return $result;
    }
    // Deleted after a successful callback methods
    protected function _after_delete($data,$options) {}

    /**
     * Querying datasets
     * @access public
     * @param array $options Expression arguments
     * @return mixed
     */
    public function select($options=array()) {
        if(is_string($options) || is_numeric($options)) {
            // According to the primary key query
            $pk   =  $this->getPk();
            if(strpos($options,',')) {
                $where[$pk]     =  array('IN',$options);
            }else{
                $where[$pk]     =  $options;
            }
            $options            =  array();
            $options['where']   =  $where;
        }elseif(false === $options){ // For subqueries Not only returns SQL
            $options            =  array();
            // Analysis Expressions
            $options            =  $this->_parseOptions($options);
            return  '( '.$this->db->buildSelectSql($options).' )';
        }
        // Analysis Expressions
        $options    =  $this->_parseOptions($options);
        $resultSet  = $this->db->select($options);
        if(false === $resultSet) {
            return false;
        }
        if(empty($resultSet)) { // Query result is empty
            return null;
        }
        $this->_after_select($resultSet,$options);
        return $resultSet;
    }
    // After a successful query callback methods
    protected function _after_select(&$resultSet,$options) {}

    /**
     * Generate SQL queries Can be used in a subquery
     * @access public
     * @param array $options Expression arguments
     * @return string
     */
    public function buildSql($options=array()) {
        // Analysis Expressions
        $options =  $this->_parseOptions($options);
        return  '( '.$this->db->buildSelectSql($options).' )';
    }

    /**
     * Analysis Expressions
     * @access proteced
     * @param array $options Expression arguments
     * @return array
     */
    protected function _parseOptions($options=array()) {
        if(is_array($options))
            $options =  array_merge($this->options,$options);
        // Query after emptying the SQL expression to assemble Avoid the next query
        $this->options  =   array();
        if(!isset($options['table'])){
            // Automatically get the table name
            $options['table']   =   $this->getTableName();
            $fields             =   $this->fields;
        }else{
            // Specifies the data sheet then reacquire the field list but not Support type detection
            $fields             =   $this->getDbFields();
        }

        if(!empty($options['alias'])) {
            $options['table']  .=   ' '.$options['alias'];
        }
        // Recording operation Model Name
        $options['model']       =   $this->name;

        // Field type validation
        if(isset($options['where']) && is_array($options['where']) && !empty($fields)) {
            // Query the array for the field type checking
            foreach ($options['where'] as $key=>$val){
                $key            =   trim($key);
                if(in_array($key,$fields,true)){
                    if(is_scalar($val)) {
                        $this->_parseType($options['where'],$key);
                    }
                }elseif('_' != substr($key,0,1) && false === strpos($key,'.') && false === strpos($key,'|') && false === strpos($key,'&')){
                    unset($options['where'][$key]);
                }
            }
        }

        // Expression Filter
        $this->_options_filter($options);
        return $options;
    }
    // Expression Filter callback method
    protected function _options_filter(&$options) {}

    /**
     * Data type detection
     * @access protected
     * @param mixed $data Data
     * @param string $key Field name
     * @return void
     */
    protected function _parseType(&$data,$key) {
        $fieldType = strtolower($this->fields['_type'][$key]);
        if(false === strpos($fieldType,'bigint') && false !== strpos($fieldType,'int')) {
            $data[$key]   =  intval($data[$key]);
        }elseif(false !== strpos($fieldType,'float') || false !== strpos($fieldType,'double')){
            $data[$key]   =  floatval($data[$key]);
        }elseif(false !== strpos($fieldType,'bool')){
            $data[$key]   =  (bool)$data[$key];
        }
    }

    /**
     * Query data
     * @access public
     * @param mixed $options Expression arguments
     * @return mixed
     */
    public function find($options=array()) {
        if(is_numeric($options) || is_string($options)) {
            $where[$this->getPk()]  =   $options;
            $options                =   array();
            $options['where']       =   $where;
        }
        // Always find a record
        $options['limit']   =   1;
        // Analysis Expressions
        $options            =   $this->_parseOptions($options);
        $resultSet          =   $this->db->select($options);
        if(false === $resultSet) {
            return false;
        }
        if(empty($resultSet)) {// Query result is empty
            return null;
        }
        $this->data         =   $resultSet[0];
        $this->_after_find($this->data,$options);
        return $this->data;
    }
    // Query success callback method
    protected function _after_find(&$result,$options) {}

    /**
     * Processing field mapping
     * @access public
     * @param array $data Current Data
     * @param integer $type Type 0 Write 1 Read
     * @return array
     */
    public function parseFieldsMap($data,$type=1) {
        // Check the field mapping
        if(!empty($this->_map)) {
            foreach ($this->_map as $key=>$val){
                if($type==1) { // Read
                    if(isset($data[$val])) {
                        $data[$key] =   $data[$val];
                        unset($data[$val]);
                    }
                }else{
                    if(isset($data[$key])) {
                        $data[$val] =   $data[$key];
                        unset($data[$key]);
                    }
                }
            }
        }
        return $data;
    }

    /**
     * Set the value of a field record
     * Support using the database fields and methods
     * @access public
     * @param string|array $field  Field name
     * @param string $value  Field values
     * @return boolean
     */
    public function setField($field,$value='') {
        if(is_array($field)) {
            $data           =   $field;
        }else{
            $data[$field]   =   $value;
        }
        return $this->save($data);
    }

    /**
     * Field Value Growth
     * @access public
     * @param string $field  Field name
     * @param integer $step  Growth in value
     * @return boolean
     */
    public function setInc($field,$step=1) {
        return $this->setField($field,array('exp',$field.'+'.$step));
    }

    /**
     * Field value Decrease
     * @access public
     * @param string $field  Field name
     * @param integer $step  Decrease the value
     * @return boolean
     */
    public function setDec($field,$step=1) {
        return $this->setField($field,array('exp',$field.'-'.$step));
    }

    /**
     * Gets the value of a field in a record
     * @access public
     * @param string $field  Field name
     * @param string $spea  Field data symbol interval NULL returned array
     * @return mixed
     */
    public function getField($field,$sepa=null) {
        $options['field']       =   $field;
        $options                =   $this->_parseOptions($options);
        $field                  =   trim($field);
        if(strpos($field,',')) { // Multi-Field
            if(!isset($options['limit'])){
                $options['limit']   =   is_numeric($sepa)?$sepa:'';
            }
            $resultSet          =   $this->db->select($options);
            if(!empty($resultSet)) {
                $_field         =   explode(',', $field);
                $field          =   array_keys($resultSet[0]);
                $key            =   array_shift($field);
                $key2           =   array_shift($field);
                $cols           =   array();
                $count          =   count($_field);
                foreach ($resultSet as $result){
                    $name   =  $result[$key];
                    if(2==$count) {
                        $cols[$name]   =  $result[$key2];
                    }else{
                        $cols[$name]   =  is_string($sepa)?implode($sepa,$result):$result;
                    }
                }
                return $cols;
            }
        }else{   // Find a record
            // Return the number of data
            if(true !== $sepa) {// While sepa is specified as true, returns all of the data
                $options['limit']   =   is_numeric($sepa)?$sepa:1;
            }
            $result = $this->db->select($options);
            if(!empty($result)) {
                if(true !== $sepa && 1==$options['limit']) return reset($result[0]);
                foreach ($result as $val){
                    $array[]    =   $val[$field];
                }
                return $array;
            }
        }
        return null;
    }

    /**
     * Create a data object But is not saved to the database
     * @access public
     * @param mixed $data Create a data
     * @param string $type State
     * @return mixed
     */
     public function create($data='',$type='') {
        // If there are no values default to get POST data
        if(empty($data)) {
            $data   =   $_POST;
        }elseif(is_object($data)){
            $data   =   get_object_vars($data);
        }
        // Validating data
        if(empty($data) || !is_array($data)) {
            $this->error = L('_DATA_TYPE_INVALID_');
            return false;
        }

        // Check the field mapping
        $data = $this->parseFieldsMap($data,0);

        // State
        $type = $type?$type:(!empty($data[$this->getPk()])?self::MODEL_UPDATE:self::MODEL_INSERT);

        // Detection of legitimacy of the submitted field
        if(isset($this->options['field'])) { // $this->field('field1,field2...')->create()
            $fields =   $this->options['field'];
            unset($this->options['field']);
        }elseif($type == self::MODEL_INSERT && isset($this->insertFields)) {
            $fields =   $this->insertFields;
        }elseif($type == self::MODEL_UPDATE && isset($this->updateFields)) {
            $fields =   $this->updateFields;
        }
        if(isset($fields)) {
            if(is_string($fields)) {
                $fields =   explode(',',$fields);
            }
            // Judge token validation fields
            if(C('TOKEN_ON'))   $fields[] = C('TOKEN_NAME');
            foreach ($data as $key=>$val){
                if(!in_array($key,$fields)) {
                    unset($data[$key]);
                }
            }
        }

        // Automatic data validation
        if(!$this->autoValidation($data,$type)) return false;

        // Form token validation
        if(C('TOKEN_ON') && !$this->autoCheckToken($data)) {
            $this->error = L('_TOKEN_ERROR_');
            return false;
        }

        // Validation complete generated data objects
        if($this->autoCheckFields) { // Open field test You filter the illegal field data
            $fields =   $this->getDbFields();
            foreach ($data as $key=>$val){
                if(!in_array($key,$fields)) {
                    unset($data[$key]);
                }elseif(MAGIC_QUOTES_GPC && is_string($val)){
                    $data[$key] =   stripslashes($val);
                }
            }
        }

        // Create complete automated treatment of the data
        $this->autoOperation($data,$type);
        // The current data object assignment
        $this->data =   $data;
        // Return the data created for other calls
        return $data;
     }

    // Automatic form validation token
    // TODO  ajax no refresh multiple submissions temporarily unable to meet
    public function autoCheckToken($data) {
        if(C('TOKEN_ON')){
            $name   = C('TOKEN_NAME');
            if(!isset($data[$name]) || !isset($_SESSION[$name])) { // Invalid token data
                return false;
            }

            // Token Authentication
            list($key,$value)  =  explode('_',$data[$name]);
            if($value && $_SESSION[$name][$key] === $value) { // Prevent duplicate submission
                unset($_SESSION[$name][$key]); // Verification is complete destroys the session
                return true;
            }
            // Open TOKEN reset
            if(C('TOKEN_RESET')) unset($_SESSION[$name][$key]);
            return false;
        }
        return true;
    }

    /**
     * Validate data using regular
     * @access public
     * @param string $value  To verify the data
     * @param string $rule Validation rules
     * @return boolean
     */
    public function regex($value,$rule) {
        $validate = array(
            'require'   =>  '/.+/',
            'email'     =>  '/^\w+([-+.]\w+)*@\w+([-.]\w+)*\.\w+([-.]\w+)*$/',
            'url'       =>  '/^http(s?):\/\/(?:[A-za-z0-9-]+\.)+[A-za-z]{2,4}(?:[\/\?#][\/=\?%\-&~`@[\]\':+!\.#\w]*)?$/',
            'currency'  =>  '/^\d+(\.\d+)?$/',
            'number'    =>  '/^\d+$/',
            'zip'       =>  '/^\d{6}$/',
            'integer'   =>  '/^[-\+]?\d+$/',
            'double'    =>  '/^[-\+]?\d+(\.\d+)?$/',
            'english'   =>  '/^[A-Za-z]+$/',
        );
        // Check whether there is a built-in regular expression
        if(isset($validate[strtolower($rule)]))
            $rule       =   $validate[strtolower($rule)];
        return preg_match($rule,$value)===1;
    }

    /**
     * Automatic Form Processing
     * @access public
     * @param array $data Create a data
     * @param string $type Create type
     * @return mixed
     */
    private function autoOperation(&$data,$type) {
        if(!empty($this->options['auto'])) {
            $_auto   =   $this->options['auto'];
            unset($this->options['auto']);
        }elseif(!empty($this->_auto)){
            $_auto   =   $this->_auto;
        }
        // Autofill
        if(isset($_auto)) {
            foreach ($_auto as $auto){
                // Fill Factor Definition Format
                // array('field','Fill Content','Fill Conditions','Additional rules',[Additional parameters])
                if(empty($auto[2])) $auto[2] = self::MODEL_INSERT; // The default is automatically populated when new
                if( $type == $auto[2] || $auto[2] == self::MODEL_BOTH) {
                    switch(trim($auto[3])) {
                        case 'function':    //  Use function to fill Value of the field as a parameter
                        case 'callback': // Using callback method
                            $args = isset($auto[4])?(array)$auto[4]:array();
                            if(isset($data[$auto[0]])) {
                                array_unshift($args,$data[$auto[0]]);
                            }
                            if('function'==$auto[3]) {
                                $data[$auto[0]]  = call_user_func_array($auto[1], $args);
                            }else{
                                $data[$auto[0]]  =  call_user_func_array(array(&$this,$auto[1]), $args);
                            }
                            break;
                        case 'field':    // The value used to fill other fields
                            $data[$auto[0]] = $data[$auto[1]];
                            break;
                        case 'ignore': // Ignore empty
                            if(''===$data[$auto[0]])
                                unset($data[$auto[0]]);
                            break;
                        case 'string':
                        default: // The default padding as a string
                            $data[$auto[0]] = $auto[1];
                    }
                    if(false === $data[$auto[0]] )   unset($data[$auto[0]]);
                }
            }
        }
        return $data;
    }

    /**
     * Automatic form validation
     * @access protected
     * @param array $data Create a data
     * @param string $type Create type
     * @return boolean
     */
    protected function autoValidation($data,$type) {
        if(!empty($this->options['validate'])) {
            $_validate   =   $this->options['validate'];
            unset($this->options['validate']);
        }elseif(!empty($this->_validate)){
            $_validate   =   $this->_validate;
        }
        // Property Validation
        if(isset($_validate)) { // If you set up automatic data validation data validation is carried out
            if($this->patchValidate) { // Reset validation error message
                $this->error = array();
            }
            foreach($_validate as $key=>$val) {
                // Verify factor Definition Format
                // array(field,rule,message,condition,type,when,params)
                // Determine whether validation is performed
                if(empty($val[5]) || $val[5]== self::MODEL_BOTH || $val[5]== $type ) {
                    if(0==strpos($val[2],'{%') && strpos($val[2],'}'))
                        // Support multi-language message Use {%Language definition} Mode
                        $val[2]  =  L(substr($val[2],2,-1));
                    $val[3]  =  isset($val[3])?$val[3]:self::EXISTS_VALIDATE;
                    $val[4]  =  isset($val[4])?$val[4]:'regex';
                    // Determine the validation criteria
                    switch($val[3]) {
                        case self::MUST_VALIDATE:   // Must verify Regardless of whether the form has set this field
                            if(false === $this->_validationField($data,$val))
                                return false;
                            break;
                        case self::VALUE_VALIDATE:    // Value is not empty when it is verified
                            if('' != trim($data[$val[0]]))
                                if(false === $this->_validationField($data,$val))
                                    return false;
                            break;
                        default:    // Default form field validation
                            if(isset($data[$val[0]]))
                                if(false === $this->_validationField($data,$val))
                                    return false;
                    }
                }
            }
            // Batch validation when the last error
            if(!empty($this->error)) return false;
        }
        return true;
    }

    /**
     * Validate form fields Support batch validation
     * If the batch validation returns an array of error information
     * @access protected
     * @param array $data Create a data
     * @param array $val Authentication factors
     * @return boolean
     */
    protected function _validationField($data,$val) {
        if(false === $this->_validationFieldItem($data,$val)){
            if($this->patchValidate) {
                $this->error[$val[0]]   =   $val[2];
            }else{
                $this->error            =   $val[2];
                return false;
            }
        }
        return ;
    }

    /**
     * Based on validation factor authentication field
     * @access protected
     * @param array $data Create a data
     * @param array $val Authentication factors
     * @return boolean
     */
    protected function _validationFieldItem($data,$val) {
        switch(strtolower(trim($val[4]))) {
            case 'function':// Validation using function
            case 'callback':// Calling methods of verification
                $args = isset($val[6])?(array)$val[6]:array();
                if(is_string($val[0]) && strpos($val[0], ','))
                    $val[0] = explode(',', $val[0]);
                if(is_array($val[0])){
                    // Support multiple-field validation
                    foreach($val[0] as $field)
                        $_data[$field] = $data[$field];
                    array_unshift($args, $_data);
                }else{
                    array_unshift($args, $data[$val[0]]);
                }
                if('function'==$val[4]) {
                    return call_user_func_array($val[1], $args);
                }else{
                    return call_user_func_array(array(&$this, $val[1]), $args);
                }
            case 'confirm': // Verify that the two fields is the same
                return $data[$val[0]] == $data[$val[1]];
            case 'unique': // To verify that a value is unique
                if(is_string($val[0]) && strpos($val[0],','))
                    $val[0]  =  explode(',',$val[0]);
                $map = array();
                if(is_array($val[0])) {
                    // Support multiple-field validation
                    foreach ($val[0] as $field)
                        $map[$field]   =  $data[$field];
                }else{
                    $map[$val[0]] = $data[$val[0]];
                }
                if(!empty($data[$this->getPk()])) { // Sound editing when validation only
                    $map[$this->getPk()] = array('neq',$data[$this->getPk()]);
                }
                if($this->where($map)->find())   return false;
                return true;
            default:  // Check additional rule
                return $this->check($data[$val[0]],$val[1],$val[4]);
        }
    }

    /**
     * Validating data Support in between equal length regex expire ip_allow ip_deny
     * @access public
     * @param string $value Validating data
     * @param mixed $rule Validation expression
     * @param string $type Authentication methods supported Defaults to verify
     * @return boolean
     */
    public function check($value,$rule,$type='regex'){
        $type   =   strtolower(trim($type));
        switch($type) {
            case 'in': // Verifies that a specified range A comma-delimited string or array
            case 'notin':
                $range   = is_array($rule)? $rule : explode(',',$rule);
                return $type == 'in' ? in_array($value ,$range) : !in_array($value ,$range);
            case 'between': // Verify a range
            case 'notbetween': // Verify that is not in a range
                if (is_array($rule)){
                    $min    =    $rule[0];
                    $max    =    $rule[1];
                }else{
                    list($min,$max)   =  explode(',',$rule);
                }
                return $type == 'between' ? $value>=$min && $value<=$max : $value<$min || $value>$max;
            case 'equal': // Verify that equal to a value
            case 'notequal': // Verify that equal to a value
                return $type == 'equal' ? $value == $rule : $value != $rule;
            case 'length': // Verify the length
                $length  =  mb_strlen($value,'utf-8'); // Current data length
                if(strpos($rule,',')) { // Length range
                    list($min,$max)   =  explode(',',$rule);
                    return $length >= $min && $length <= $max;
                }else{// Specified length
                    return $length == $rule;
                }
            case 'expire':
                list($start,$end)   =  explode(',',$rule);
                if(!is_numeric($start)) $start   =  strtotime($start);
                if(!is_numeric($end)) $end   =  strtotime($end);
                return NOW_TIME >= $start && NOW_TIME <= $end;
            case 'ip_allow': // IP Operating license verified
                return in_array(get_client_ip(),explode(',',$rule));
            case 'ip_deny': // IP Action to suppress validation
                return !in_array(get_client_ip(),explode(',',$rule));
            case 'regex':
            default:    // Default use regular verification You can use validation class defined in the Authentication Name
                // Check additional rule
                return $this->regex($value,$rule);
        }
    }

    /**
     * SQL Query
     * @access public
     * @param string $sql  SQL commands
     * @param mixed $parse  The need for parsing the SQL
     * @return mixed
     */
    public function query($sql,$parse=false) {
        if(!is_bool($parse) && !is_array($parse)) {
            $parse = func_get_args();
            array_shift($parse);
        }
        $sql  =   $this->parseSql($sql,$parse);
        return $this->db->query($sql);
    }

    /**
     * Executing SQL statements
     * @access public
     * @param string $sql  SQL commands
     * @param mixed $parse  The need for parsing the SQL
     * @return false | integer
     */
    public function execute($sql,$parse=false) {
        if(!is_bool($parse) && !is_array($parse)) {
            $parse = func_get_args();
            array_shift($parse);
        }
        $sql  =   $this->parseSql($sql,$parse);
        return $this->db->execute($sql);
    }

    /**
     * Parsing the SQL statement
     * @access public
     * @param string $sql  SQL commands
     * @param boolean $parse  The need for parsing the SQL
     * @return string
     */
    protected function parseSql($sql,$parse) {
        // Analysis Expressions
        if(true === $parse) {
            $options =  $this->_parseOptions();
            $sql  =   $this->db->parseSql($sql,$options);
        }elseif(is_array($parse)){ // SQL preprocessing
            $sql  = vsprintf($sql,$parse);
        }else{
            $sql    =   strtr($sql,array('__TABLE__'=>$this->getTableName(),'__PREFIX__'=>C('DB_PREFIX')));
        }
        $this->db->setModel($this->name);
        return $sql;
    }

    /**
     * Switch the current database connection
     * @access public
     * @param integer $linkNum  Serial Connection
     * @param mixed $config  Database connection information
     * @param array $params  Model parameters
     * @return Model
     */
    public function db($linkNum='',$config='',$params=array()){
        if(''===$linkNum && $this->db) {
            return $this->db;
        }
        static $_linkNum    =   array();
        static $_db = array();
        if(!isset($_db[$linkNum]) || (isset($_db[$linkNum]) && $config && $_linkNum[$linkNum]!=$config) ) {
            // Create a new instance
            if(!empty($config) && is_string($config) && false === strpos($config,'/')) { // Support read configuration parameters
                $config  =  C($config);
            }
            $_db[$linkNum]            =    Db::getInstance($config);
        }elseif(NULL === $config){
            $_db[$linkNum]->close(); // Close the database connection
            unset($_db[$linkNum]);
            return ;
        }
        if(!empty($params)) {
            if(is_string($params))    parse_str($params,$params);
            foreach ($params as $name=>$value){
                $this->setProperty($name,$value);
            }
        }
        // Record Connection Information
        $_linkNum[$linkNum] =   $config;
        // Switching the database connection
        $this->db   =    $_db[$linkNum];
        $this->_after_db();
        // Field testing
        if(!empty($this->name) && $this->autoCheckFields)    $this->_checkTableInfo();
        return $this;
    }
    // Database callback method after switching
    protected function _after_db() {}

    /**
     * Get current data object name
     * @access public
     * @return string
     */
    public function getModelName() {
        if(empty($this->name))
            $this->name =   substr(get_class($this),0,-5);
        return $this->name;
    }

    /**
     * Get complete data table name
     * @access public
     * @return string
     */
    public function getTableName() {
        if(empty($this->trueTableName)) {
            $tableName  = !empty($this->tablePrefix) ? $this->tablePrefix : '';
            if(!empty($this->tableName)) {
                $tableName .= $this->tableName;
            }else{
                $tableName .= ($this->name);
            }
            $this->trueTableName    =   ($tableName);
        }
        return (!empty($this->dbName)?$this->dbName.'.':'').$this->trueTableName;
    }

    /**
     * Start transaction
     * @access public
     * @return void
     */
    public function startTrans() {
        $this->commit();
        $this->db->startTrans();
        return ;
    }

    /**
     * Commit the transaction
     * @access public
     * @return boolean
     */
    public function commit() {
        return $this->db->commit();
    }

    /**
     * Transaction rollback
     * @access public
     * @return boolean
     */
    public function rollback() {
        return $this->db->rollback();
    }

    /**
     * Returns an error message model
     * @access public
     * @return string
     */
    public function getError(){
        return $this->error;
    }

    /**
     * Returns the database error message
     * @access public
     * @return string
     */
    public function getDbError() {
        return $this->db->getError();
    }

    /**
     * Returns the last inserted ID
     * @access public
     * @return string
     */
    public function getLastInsID() {
        return $this->db->getLastInsID();
    }

    /**
     * Returns the last executed sql statement
     * @access public
     * @return string
     */
    public function getLastSql() {
        return $this->db->getLastSql($this->name);
    }
    // Given getLastSql more common Increase _sql Aliases
    public function _sql(){
        return $this->getLastSql();
    }

    /**
     * Get the name of the primary key
     * @access public
     * @return string
     */
    public function getPk() {
        return isset($this->fields['_pk'])?$this->fields['_pk']:$this->pk;
    }

    /**
     * Field information for a data sheet
     * @access public
     * @return array
     */
    public function getDbFields(){
        if(isset($this->options['table'])) {// Dynamically specify the table name
            $fields     =   $this->db->getFields($this->options['table']);
            return  $fields?array_keys($fields):false;
        }
        if($this->fields) {
            $fields     =  $this->fields;
            unset($fields['_autoinc'],$fields['_pk'],$fields['_type'],$fields['_version']);
            return $fields;
        }
        return false;
    }

    /**
     * Set the value of the data object
     * @access public
     * @param mixed $data Data
     * @return Model
     */
    public function data($data=''){
        if('' === $data && !empty($this->data)) {
            return $this->data;
        }
        if(is_object($data)){
            $data   =   get_object_vars($data);
        }elseif(is_string($data)){
            parse_str($data,$data);
        }elseif(!is_array($data)){
            throw_exception(L('_DATA_TYPE_INVALID_'));
        }
        $this->data = $data;
        return $this;
    }

    /**
     * Querying SQL assembly join
     * @access public
     * @param mixed $join
     * @return Model
     */
    public function join($join) {
        if(is_array($join)) {
            $this->options['join']      =   $join;
        }elseif(!empty($join)) {
            $this->options['join'][]    =   $join;
        }
        return $this;
    }

    /**
     * Querying SQL assembly union
     * @access public
     * @param mixed $union
     * @param boolean $all
     * @return Model
     */
    public function union($union,$all=false) {
        if(empty($union)) return $this;
        if($all) {
            $this->options['union']['_all']  =   true;
        }
        if(is_object($union)) {
            $union   =  get_object_vars($union);
        }
        // Conversion union expression
        if(is_string($union) ) {
            $options =  $union;
        }elseif(is_array($union)){
            if(isset($union[0])) {
                $this->options['union']  =  array_merge($this->options['union'],$union);
                return $this;
            }else{
                $options =  $union;
            }
        }else{
            throw_exception(L('_DATA_TYPE_INVALID_'));
        }
        $this->options['union'][]  =   $options;
        return $this;
    }

    /**
     * Query Cache
     * @access public
     * @param mixed $key
     * @param integer $expire
     * @param string $type
     * @return Model
     */
    public function cache($key=true,$expire=null,$type=''){
        if(false !== $key)
            $this->options['cache']  =  array('key'=>$key,'expire'=>$expire,'type'=>$type);
        return $this;
    }

    /**
     * Specifies the query field Support field exclusion
     * @access public
     * @param mixed $field
     * @param boolean $except Whether the exclusion
     * @return Model
     */
    public function field($field,$except=false){
        if(true === $field) {// Get all the fields
            $fields     =  $this->getDbFields();
            $field      =  $fields?$fields:'*';
        }elseif($except) {// Fields excluded
            if(is_string($field)) {
                $field  =  explode(',',$field);
            }
            $fields     =  $this->getDbFields();
            $field      =  $fields?array_diff($fields,$field):$field;
        }
        $this->options['field']   =   $field;
        return $this;
    }

    /**
     * Calling a named range
     * @access public
     * @param mixed $scope Named Range Name Support multiple and direct definition
     * @param array $args Parameter
     * @return Model
     */
    public function scope($scope='',$args=NULL){
        if('' === $scope) {
            if(isset($this->_scope['default'])) {
                // Default naming scope
                $options    =   $this->_scope['default'];
            }else{
                return $this;
            }
        }elseif(is_string($scope)){ // Support multiple named ranges called separated by commas
            $scopes         =   explode(',',$scope);
            $options        =   array();
            foreach ($scopes as $name){
                if(!isset($this->_scope[$name])) continue;
                $options    =   array_merge($options,$this->_scope[$name]);
            }
            if(!empty($args) && is_array($args)) {
                $options    =   array_merge($options,$args);
            }
        }elseif(is_array($scope)){ // Directly into a named range defined
            $options        =   $scope;
        }

        if(is_array($options) && !empty($options)){
            $this->options  =   array_merge($this->options,array_change_key_case($options));
        }
        return $this;
    }

    /**
     * Specify the query criteria Support security filtering
     * @access public
     * @param mixed $where Conditional Expressions
     * @param mixed $parse Pretreatment parameters
     * @return Model
     */
    public function where($where,$parse=null){
        if(!is_null($parse) && is_string($where)) {
            if(!is_array($parse)) {
                $parse = func_get_args();
                array_shift($parse);
            }
            $parse = array_map(array($this->db,'escapeString'),$parse);
            $where =   vsprintf($where,$parse);
        }elseif(is_object($where)){
            $where  =   get_object_vars($where);
        }
        if(is_string($where) && '' != $where){
            $map    =   array();
            $map['_string']   =   $where;
            $where  =   $map;
        }
        if(isset($this->options['where'])){
            $this->options['where'] =   array_merge($this->options['where'],$where);
        }else{
            $this->options['where'] =   $where;
        }

        return $this;
    }

    /**
     * Specifies the number of queries
     * @access public
     * @param mixed $offset Starting position
     * @param mixed $length Number of queries
     * @return Model
     */
    public function limit($offset,$length=null){
        $this->options['limit'] =   is_null($length)?$offset:$offset.','.$length;
        return $this;
    }

    /**
     * Specifies the paging
     * @access public
     * @param mixed $page Pages
     * @param mixed $listRows Number per page
     * @return Model
     */
    public function page($page,$listRows=null){
        $this->options['page'] =   is_null($listRows)?$page:$page.','.$listRows;
        return $this;
    }

    /**
     * Query Comments
     * @access public
     * @param string $comment Note
     * @return Model
     */
    public function comment($comment){
        $this->options['comment'] =   $comment;
        return $this;
    }

    /**
     * Set model attribute values
     * @access public
     * @param string $name Name
     * @param mixed $value Value
     * @return Model
     */
    public function setProperty($name,$value) {
        if(property_exists($this,$name))
            $this->$name = $value;
        return $this;
    }

}