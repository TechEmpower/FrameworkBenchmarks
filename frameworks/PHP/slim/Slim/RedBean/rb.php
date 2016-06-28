<?php 
/**
REDBEANPHP 3.4
--------------
RedBeanPHP Database Objects -
Written by Gabor de Mooij (c) copyright 2009-2013 and the RedBeanPHP community 
RedBeanPHP is DUAL Licensed BSD and GPLv2. You may choose the license that fits
best for your project.
*/

interface RedBean_Driver {
	/**
	 * Runs a query and fetches results as a multi dimensional array.
	 *
	 * @param  string $sql SQL to be executed
	 *
	 * @return array $results result
	 */
	public function GetAll($sql, $aValues = array());
	/**
	 * Runs a query and fetches results as a column.
	 *
	 * @param  string $sql SQL Code to execute
	 *
	 * @return array	$results Resultset
	 */
	public function GetCol($sql, $aValues = array());
	/**
	 * Runs a query and returns results as a single cell.
	 *
	 * @param string $sql SQL to execute
	 *
	 * @return mixed $cellvalue result cell
	 */
	public function GetCell($sql, $aValues = array());
	/**
	 * Runs a query and returns a flat array containing the values of
	 * one row.
	 *
	 * @param string $sql SQL to execute
	 *
	 * @return array $row result row
	 */
	public function GetRow($sql, $aValues = array());
	/**
	 * Executes SQL code and allows key-value binding.
	 * This function allows you to provide an array with values to bind
	 * to query parameters. For instance you can bind values to question
	 * marks in the query. Each value in the array corresponds to the
	 * question mark in the query that matches the position of the value in the
	 * array. You can also bind values using explicit keys, for instance
	 * array(":key"=>123) will bind the integer 123 to the key :key in the
	 * SQL. This method has no return value.
	 *
	 * @param string $sql	  SQL Code to execute
	 * @param array  $aValues Values to bind to SQL query
	 *
	 * @return void
	 */
	public function Execute($sql, $aValues = array());
	/**
	 * Returns the latest insert ID if driver does support this
	 * feature.
	 *
	 * @return integer $id primary key ID
	 */
	public function GetInsertID();
	/**
	 * Returns the number of rows affected by the most recent query
	 * if the currently selected driver driver supports this feature.
	 *
	 * @return integer $numOfRows number of rows affected
	 */
	public function Affected_Rows();
	/**
	 * Toggles debug mode. In debug mode the driver will print all
	 * SQL to the screen together with some information about the
	 * results. All SQL code that passes through the driver will be
	 * passes on to the screen for inspection.
	 * This method has no return value.
	 *
	 * @param boolean $trueFalse turn on/off
	 *
	 * @return void
	 */
	public function setDebugMode($tf);
	/**
	 * Starts a transaction.
	 * @return void
	 */
	public function CommitTrans();
	/**
	 * Commits a transaction.
	 * @return void
	 */
	public function StartTrans();
	/**
	 * Rolls back a transaction.
	 * @return void
	 */
	public function FailTrans();
}

class RedBean_Driver_PDO implements RedBean_Driver {
	/**
	 * @var string
	 */
	protected $dsn;
	/**
	 * @var boolean
	 */
	protected $debug = false;
	/**
	 * @var RedBean_Logger
	 */
	protected $logger = NULL;
	/**
	 * @var PDO
	 */
	protected $pdo;
	/**
	 * @var integer
	 */
	protected $affected_rows;
	/**
	 * @var integer
	 */
	protected $rs;
	/**
	 * @var array
	 */
	protected $connectInfo = array();
	/**
	 * @var bool
	 */
	public $flagUseStringOnlyBinding = false;
	/**
	 * @var boolean
	 */
	protected $isConnected = false;
	/**
	 * Constructor. You may either specify dsn, user and password or
	 * just give an existing PDO connection.
	 * Examples:
	 *    $driver = new RedBean_Driver_PDO($dsn, $user, $password);
	 *    $driver = new RedBean_Driver_PDO($existingConnection);
	 *
	 * @param string|PDO  $dsn	database connection string
	 * @param string      $user optional, usename to sign in 
	 * @param string      $pass optional, password for connection login
	 *
	 * @return void
	 */
	public function __construct($dsn, $user = null, $pass = null) {
		if ($dsn instanceof PDO) {
			$this->pdo = $dsn;
			$this->isConnected = true;
			$this->pdo->setAttribute(1002, 'SET NAMES utf8');
			$this->pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
			$this->pdo->setAttribute(PDO::ATTR_DEFAULT_FETCH_MODE, PDO::FETCH_ASSOC);
			// make sure that the dsn at least contains the type
			$this->dsn = $this->getDatabaseType();
		} else {
			$this->dsn = $dsn;
			$this->connectInfo = array('pass' => $pass, 'user' => $user);
		}
	}
	/**
	 * Establishes a connection to the database using PHP PDO
	 * functionality. If a connection has already been established this
	 * method will simply return directly. This method also turns on
	 * UTF8 for the database and PDO-ERRMODE-EXCEPTION as well as
	 * PDO-FETCH-ASSOC.
	 *
	 * @return void
	 */
	public function connect() {
		if ($this->isConnected) return;
		try {
			$user = $this->connectInfo['user'];
			$pass = $this->connectInfo['pass'];
			$this->pdo = new PDO(
					  $this->dsn,
					  $user,
					  $pass,
					  array(1002 => 'SET NAMES utf8',
								 PDO::ATTR_ERRMODE => PDO::ERRMODE_EXCEPTION,
								 PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC,
					  )
			);
			$this->pdo->setAttribute(PDO::ATTR_STRINGIFY_FETCHES, true);
			$this->isConnected = true;
		} catch(PDOException $e) {
			throw new PDOException('Could not connect to database.');
		}
	}
	/**
	 * Binds parameters. This method binds parameters to a PDOStatement for
	 * Query Execution. This method binds parameters as NULL, INTEGER or STRING
	 * and supports both named keys and question mark keys.
	 *
	 * @param  PDOStatement $s       PDO Statement instance
	 * @param  array        $aValues values that need to get bound to the statement
	 *
	 * @return void
	 */
	protected function bindParams($s, $aValues) {
		foreach($aValues as $key => &$value) {
			if (is_integer($key)) {
				if (is_null($value)){
					$s->bindValue($key+1, null, PDO::PARAM_NULL);
				} elseif (!$this->flagUseStringOnlyBinding && RedBean_QueryWriter_AQueryWriter::canBeTreatedAsInt($value) && $value < 2147483648) {
					$s->bindParam($key+1, $value, PDO::PARAM_INT);
				} else {
					$s->bindParam($key+1, $value, PDO::PARAM_STR);
				}
			} else {
				if (is_null($value)){
					$s->bindValue($key, null, PDO::PARAM_NULL);
				} elseif (!$this->flagUseStringOnlyBinding && RedBean_QueryWriter_AQueryWriter::canBeTreatedAsInt($value) &&  $value < 2147483648) {
					$s->bindParam($key, $value, PDO::PARAM_INT);
				} else {
					$s->bindParam($key, $value, PDO::PARAM_STR);
				}
			}
		}
	}
	/**
	 * This method runs the actual SQL query and binds a list of parameters to the query.
	 * slots. The result of the query will be stored in the protected property
	 * $rs (always array). The number of rows affected (result of rowcount, if supported by database)
	 * is stored in protected property $affected_rows. If the debug flag is set
	 * this function will send debugging output to screen buffer.
	 * 
	 * @throws RedBean_Exception_SQL 
	 * 
	 * @param string $sql     the SQL string to be send to database server
	 * @param array  $aValues the values that need to get bound to the query slots
	 */
	protected function runQuery($sql, $aValues) {
		$this->connect();
		if ($this->debug && $this->logger) {
			$this->logger->log($sql, $aValues);
		}
		try {
			if (strpos('pgsql', $this->dsn) === 0) {
				$s = $this->pdo->prepare($sql, array(PDO::PGSQL_ATTR_DISABLE_NATIVE_PREPARED_STATEMENT => true));
			} else {
				$s = $this->pdo->prepare($sql);
			}
			$this->bindParams($s, $aValues);
			$s->execute();
			$this->affected_rows = $s->rowCount();
			if ($s->columnCount()) {
		    	$this->rs = $s->fetchAll();
		    	if ($this->debug && $this->logger) $this->logger->log('resultset: '.count($this->rs).' rows');
	    	} else {
		    	$this->rs = array();
		  	}
		} catch(PDOException $e) {
			//Unfortunately the code field is supposed to be int by default (php)
			//So we need a property to convey the SQL State code.
			$err = $e->getMessage();
			if ($this->debug && $this->logger) $this->logger->log('An error occurred: '.$err);
			$x = new RedBean_Exception_SQL($err, 0);
			$x->setSQLState($e->getCode());
			throw $x;
		}
	}
	/**
	 * @see RedBean_Driver::GetAll
	 */
	public function GetAll($sql, $aValues = array()) {
		$this->runQuery($sql, $aValues);
		return $this->rs;
	}
	 /**
	 * @see RedBean_Driver::GetCol
	 */
	public function GetCol($sql, $aValues = array()) {
		$rows = $this->GetAll($sql, $aValues);
		$cols = array();
		if ($rows && is_array($rows) && count($rows)>0) {
			foreach ($rows as $row) {
				$cols[] = array_shift($row);
			}
		}
		return $cols;
	}
	/**
	 * @see RedBean_Driver::GetCell
	 */
	public function GetCell($sql, $aValues = array()) {
		$arr = $this->GetAll($sql, $aValues);
		$row1 = array_shift($arr);
		$col1 = array_shift($row1);
		return $col1;
	}
	/**
	 * @see RedBean_Driver::GetRow
	 */
	public function GetRow($sql, $aValues = array()) {
		$arr = $this->GetAll($sql, $aValues);
		return array_shift($arr);
	}
	/**
	 * @see RedBean_Driver::Excecute
	 */
	public function Execute($sql, $aValues = array()) {
		$this->runQuery($sql, $aValues);
		return $this->affected_rows;
	}
	/**
	 * @see RedBean_Driver::GetInsertID
	 */
	public function GetInsertID() {
		$this->connect();
		return (int) $this->pdo->lastInsertId();
	}
	/**
	 * @see RedBean_Driver::Affected_Rows
	 */
	public function Affected_Rows() {
		$this->connect();
		return (int) $this->affected_rows;
	}
	/**
	 * Toggles debug mode. In debug mode the driver will print all
	 * SQL to the screen together with some information about the
	 * results. 
	 *
	 * @param boolean $trueFalse turn on/off
	 * @param RedBean_Logger $logger 
	 *
	 * @return void
	 */
	public function setDebugMode($tf, $logger = NULL) {
		$this->connect();
		$this->debug = (bool) $tf;
		if ($this->debug and !$logger) $logger = new RedBean_Logger_Default();
		$this->setLogger($logger);
	}
	/**
	 * Injects RedBean_Logger object.
	 *
	 * @param RedBean_Logger $logger
	 */
	public function setLogger(RedBean_Logger $logger) {
		$this->logger = $logger;
	}
	/**
	 * Gets RedBean_Logger object.
	 *
	 * @return RedBean_Logger
	 */
	public function getLogger() {
		return $this->logger;
	}
	/**
	 * @see RedBean_Driver::StartTrans
	 */
	public function StartTrans() {
		$this->connect();
		$this->pdo->beginTransaction();
	}
	/**
	 * @see RedBean_Driver::CommitTrans
	 */
	public function CommitTrans() {
		$this->connect();
		$this->pdo->commit();
	}
	/**
	 * @see RedBean_Driver::FailTrans
	 */
	public function FailTrans() {
		$this->connect();
		$this->pdo->rollback();
	}
	/**
	 * Returns the name of the database type/brand: i.e. mysql, db2 etc.
	 *
	 * @return string $typeName database identification
	 */
	public function getDatabaseType() {
		$this->connect();
		return $this->pdo->getAttribute(PDO::ATTR_DRIVER_NAME);
	}
	/**
	 * Returns the version number of the database.
	 *
	 * @return mixed $version version number of the database
	 */
	public function getDatabaseVersion() {
		$this->connect();
		return $this->pdo->getAttribute(PDO::ATTR_CLIENT_VERSION);
	}
	/**
	 * Returns the underlying PHP PDO instance.
	 *
	 * @return PDO $pdo PDO instance used by PDO wrapper
	 */
	public function getPDO() {
		$this->connect();
		return $this->pdo;
	}
	/**
	 * Closes database connection by destructing PDO.
	 */
	public function close() {
		$this->pdo = null;
		$this->isConnected = false;
	}
	/**
	 * Returns TRUE if the current PDO instance is connected.
	 * 
	 * @return boolean $yesNO 
	 */
	public function isConnected() {
		if (!$this->isConnected && !$this->pdo) return false;
		return true;
	}
}

class RedBean_OODBBean implements IteratorAggregate, ArrayAccess, Countable {
	/**
	 * @var boolean
	 */
	private static $flagUseBeautyfulColumnnames = true;
	/**
	 * @var array
	 */
	private static $beautifulColumns = array();
	/**
	 * @var boolean  
	 */
	private static $flagKeyedExport = false;
	/**
	* @var boolean
	*/
	private $flagSkipBeau = false;
	/**
	 * @var array $properties
	 */
	private $properties = array();
	/**
	 * @var array
	 */
	private $__info = array();
	/**
	 * @var RedBean_BeanHelper
	 */
	private $beanHelper = NULL;
	/**
	 * @var null
	 */
	private $fetchType = NULL;
	/**
	 * @var string 
	 */
	private $withSql = '';
	/**
	 * @var array 
	 */
	private $withParams = array();
	/**
	 * @var string 
	 */
	private $aliasName = NULL;
	/**
	 * By default own-lists and shared-lists no longer have IDs as keys (3.3+),
	 * this is because exportAll also does not offer this feature and we want the
	 * ORM to be more consistent. Also, exporting without keys makes it easier to
	 * export lists to Javascript because unlike in PHP in JS arrays will fill up gaps.
	 * 
	 * @var boolean $yesNo 
	 */
	public static function setFlagKeyedExport($flag) {
		self::$flagKeyedExport = (boolean) $flag;
	}
	/**
	 * Flag indicates whether column names with CamelCase are supported and automatically
	 * converted; example: isForSale -> is_for_sale
	 * 
	 * @param boolean
	 */
	public static function setFlagBeautifulColumnNames($flag) {
		self::$flagUseBeautyfulColumnnames = (boolean) $flag;
	}
	/** Returns the alias for a type
	 *
	 * @param  $type aliased type
	 *
	 * @return string $type type
	 */
	private function getAlias($type) {
		if ($this->fetchType) {
			$type = $this->fetchType;
			$this->fetchType = null;
		}
		return $type;
	}
	/**
	 * Sets the Bean Helper. Normally the Bean Helper is set by OODB.
	 * Here you can change the Bean Helper. The Bean Helper is an object
	 * providing access to a toolbox for the bean necessary to retrieve
	 * nested beans (bean lists: ownBean, sharedBean) without the need to
	 * rely on static calls to the facade (or make this class dep. on OODB).
	 *
	 * @param RedBean_IBeanHelper $helper
	 * 
	 * @return void
	 */
	public function setBeanHelper(RedBean_BeanHelper $helper) {
		$this->beanHelper = $helper;
	}
	/**
	 * Returns an ArrayIterator so you can treat the bean like
	 * an array with the properties container as its contents.
	 *
	 * @return ArrayIterator $arrayIt an array iterator instance with $properties
	 */
	public function getIterator() {
		return new ArrayIterator($this->properties);
	}
	/**
	 * Imports all values from an associative array $array. Chainable.
	 *
	 * @param array        $array     what you want to import
	 * @param string|array $selection selection of values
	 * @param boolean      $notrim    if TRUE values will not be trimmed
	 *
	 * @return RedBean_OODBBean $this
	 */
	public function import($arr, $selection = false, $notrim = false) {
		if (is_string($selection)) {
			$selection = explode(',', $selection);
		}
		if (!$notrim && is_array($selection)) foreach($selection as $k => $s){ $selection[$k] = trim($s); }
		foreach($arr as $k => $v) {
			if ($k != '__info') {
				if (!$selection || ($selection && in_array($k, $selection))) {
					$this->$k = $v;
				}
			}
		}
		return $this;
	}
	/**
	* Imports data from another bean. Chainable.
	* 
	* @param RedBean_OODBBean $sourceBean the source bean to take properties from
	*
	* @return RedBean_OODBBean $self
	*/
	public function importFrom(RedBean_OODBBean $sourceBean) {
		$this->__info['tainted'] = true;
		$array = $sourceBean->properties;
		$this->properties = $array;
		return $this;
	}
	/**
	 * Injects the properties of another bean but keeps the original ID.
	 * Just like import() but keeps the original ID.
	 * Chainable.
	 * 
	 * @param RedBean_OODBBean $otherBean the bean whose properties you would like to copy
	 * 
	 * @return RedBean_OODBBean $self
	 */
	public function inject(RedBean_OODBBean $otherBean) {
		$myID = $this->id;
		$array = $otherBean->export();
		$this->import($array);
		$this->id = $myID;
		return $this;
	}
	/**
	 * Exports the bean as an array.
	 * This function exports the contents of a bean to an array and returns
	 * the resulting array. 
	 * 
	 * @param boolean $meta    set to TRUE if you want to export meta data as well 
	 * @param boolean $parents set to TRUE if you want to export parents as well
	 * @param boolean $onlyMe  set to TRUE if you want to export only this bean
	 * @param array   $filters optional whitelist for export
	 * 
	 * @return array $arr
	 */
	public function export($meta = false, $parents = false, $onlyMe = false, $filters = array()) {
		$arr = array();
		if ($parents) {
			foreach($this as $k => $v) {
				if (substr($k, -3) == '_id') {
					$prop = substr($k, 0, strlen($k)-3);
					$this->$prop;
				}
			}
		}
		foreach($this as $k => $v) {
			if (!$onlyMe && is_array($v)) {
				$vn = array();
				foreach($v as $i => $b) {
					if (is_numeric($i) && !self::$flagKeyedExport) {
						$vn[] = $b->export($meta, false, false, $filters);
					} else {
						$vn[$i] = $b->export($meta, false, false, $filters);
					}
					$v = $vn;
				}
			} elseif ($v instanceof RedBean_OODBBean) {
				if (is_array($filters) && count($filters) && !in_array(strtolower($v->getMeta('type')), $filters)) {
					continue;
				}
				$v = $v->export($meta, $parents, false, $filters);
			}
			$arr[$k] = $v;
		}
		if ($meta) $arr['__info'] = $this->__info;
		return $arr;
	}
	/**
	 * Exports the bean to an object.
	 * 
	 * @param object $obj target object
	 * 
	 * @return array $arr
	 */
	public function exportToObj($obj) {
		foreach($this->properties as $k => $v) {
			if (!is_array($v) && !is_object($v))
			$obj->$k = $v;
		}
	}
	/**
	 * Implements isset() function for use as an array.
	 * 
	 * @param string $property name of the property you want to check
	 * 
	 * @return boolean
	 */
	public function __isset($property) {
		return (isset($this->properties[$property]));
	}
	/**
	 * Returns the ID of the bean no matter what the ID field is.
	 *
	 * @return string $id record Identifier for bean
	 */
	public function getID() {
		return (string) $this->id;
	}
	/**
	 * Unsets a property. This method will load the property first using
	 * __get.
	 *
	 * @param  string $property property
	 *
	 * @return void
	 */
	public function __unset($property) {
		$this->__get($property);
		$fieldLink = $property.'_id';
		if (isset($this->$fieldLink)) {
			//wanna unset a bean reference?
			$this->$fieldLink = null;
		}
		if ((isset($this->properties[$property]))) {
			unset($this->properties[$property]);
		}
	}
	/**
	 * Removes a property from the properties list without invoking
	 * an __unset on the bean.
	 *
	 * @param  string $property property that needs to be unset
	 *
	 * @return void
	 */
	public function removeProperty($property) {
		unset($this->properties[$property]);
	}
	/**
	 * Adds WHERE clause conditions to ownList retrieval.
	 * For instance to get the pages that belong to a book you would
	 * issue the following command: $book->ownPage
	 * However, to order these pages by number use:
	 * 
	 * $book->with(' ORDER BY `number` ASC ')->ownPage
	 * 
	 * the additional SQL snippet will be merged into the final
	 * query.
	 * 
	 * @param string|RedBean_SQLHelper $sql SQL to be added to retrieval query.
	 * @param array                    $params array with parameters to bind to SQL snippet
	 * 
	 * @return RedBean_OODBBean $self
	 */
	public function with($sql, $params = array()) {
		if ($sql instanceof RedBean_SQLHelper) {
			list($this->withSql, $this->withParams) = $sql->getQuery();
		} else {
			$this->withSql = $sql;
			$this->withParams = $params;
		}
		return $this;
	}
	/**
	 * Just like with(). Except that this method prepends the SQL query snippet 
	 * with AND which makes it slightly more comfortable to use a conditional
	 * SQL snippet. For instance to filter an own-list with pages (belonging to
	 * a book) on specific chapters you can use:
	 * 
	 * $book->withCondition(' chapter = 3 ')->ownPage
	 * 
	 * This will return in the own list only the pages having 'chapter == 3'. 
	 * 
	 * @param string|RedBean_SQLHelper $sql    SQL to be added to retrieval query (prefixed by AND)
	 * @param array                    $params array with parameters to bind to SQL snippet
	 * 
	 * @return RedBean_OODBBean $self
	 */
	public function withCondition($sql, $params = array()) {
		if ($sql instanceof RedBean_SQLHelper) {
			list($sql, $params) = $sql->getQuery();
		} 
		$this->withSql = ' AND '.$sql;
		$this->withParams = $params;
		return $this;
	}
	/**
	 * Prepares an own-list to use an alias. This is best explained using
	 * an example. Imagine a project and a person. The project always involves
	 * two persons: a teacher and a student. The person beans have been aliased in this
	 * case, so to the project has a teacher_id pointing to a person, and a student_id
	 * also pointing to a person. Given a project, we obtain the teacher like this:
	 * 
	 * $project->fetchAs('person')->teacher;
	 * 
	 * Now, if we want all projects of a teacher we cant say:
	 * 
	 * $teacher->ownProject
	 * 
	 * because the $teacher is a bean of type 'person' and no project has been
	 * assigned to a person. Instead we use the alias() method like this:
	 * 
	 * $teacher->alias('teacher')->ownProject
	 * 
	 * now we get the projects associated with the person bean aliased as
	 * a teacher.
	 * 
	 * @param string $aliasName the alias name to use
	 * 
	 * @return RedBean_OODBBean 
	 */
	public function alias($aliasName) {
		$this->aliasName = $aliasName;
		return $this;
	}
	/**
	* Turns a camelcase property name into an underscored property name.
	* Examples:
	*	oneACLRoute -> one_acl_route
	*	camelCase -> camel_case
	*
	* Also caches the result to improve performance.
	*
	* @param string $property
	*
	* @return string	
	*/
	public function beau($property) {
		if (strpos($property, 'own') !== 0 && strpos($property, 'shared') !== 0) {
			if (isset(self::$beautifulColumns[$property])) {
				$propertyBeau = self::$beautifulColumns[$property];
			} else {
				$propertyBeau = strtolower(preg_replace('/(?<=[a-z])([A-Z])|([A-Z])(?=[a-z])/', '_$1$2', $property));
				self::$beautifulColumns[$property] = $propertyBeau;
			}
			return $propertyBeau;
		} else {
			return $property;
		}
	}
	/**
	 * Magic Getter. Gets the value for a specific property in the bean.
	 * If the property does not exist this getter will make sure no error
	 * occurs. This is because RedBean allows you to query (probe) for
	 * properties. If the property can not be found this method will
	 * return NULL instead.
	 * @param string $property
	 * @return mixed $value
	 */
	public function &__get($property) {
		if (self::$flagUseBeautyfulColumnnames && !$this->flagSkipBeau) {
			$property = $this->beau($property);	
		}
		if ($this->beanHelper) {
			$toolbox = $this->beanHelper->getToolbox();
			$redbean = $toolbox->getRedBean();
		}
		if ($this->withSql !== '') {
			if (strpos($property, 'own') === 0) {
				unset($this->properties[$property]);
			}
		}	
		if (!isset($this->properties[$property])) { 
			$fieldLink = $property.'_id'; 
			if (isset($this->$fieldLink) && $fieldLink !== $this->getMeta('sys.idfield')) {
				$this->__info['tainted'] = true; 
				$bean = $this->getMeta('sys.parentcache.'.$property);
				if (!$bean) { 
					$type =  $this->getAlias($property);
					$targetType = $this->properties[$fieldLink];
					$bean =  $redbean->load($type, $targetType);
				}
				$this->properties[$property] = $bean;
				return $this->properties[$property];
			}
			elseif (strpos($property, 'own') === 0 && ctype_upper(substr($property, 3, 1))) {
				$type = lcfirst(substr($property, 3));
				if (self::$flagUseBeautyfulColumnnames) {
					$type = $this->beau($type);
				}
				if ($this->aliasName) {
					$parentField = $this->aliasName;
					$myFieldLink = $this->aliasName.'_id';
					$this->__info['sys.alias.'.$type] = $this->aliasName;
					$this->aliasName = null;
				} else {
					$myFieldLink = $this->__info['type'].'_id';
					$parentField = $this->__info['type'];
				}
				$beans = array();
				if ($this->getID()>0) {
					$params = array_merge(array($this->getID()), $this->withParams);
					$beans = $redbean->find($type, array(), array(" $myFieldLink = ? ".$this->withSql, $params));
				}
				$this->withSql = '';
				$this->withParams = array();
				foreach($beans as $b) {
					$b->__info['sys.parentcache.'.$parentField] = $this;
				}
				$this->properties[$property] = $beans;
				$this->__info['sys.shadow.'.$property] = $beans;
				$this->__info['tainted'] = true;
				return $this->properties[$property];
			}
			elseif (strpos($property, 'shared') === 0 && ctype_upper(substr($property, 6, 1))) {
				$type = lcfirst(substr($property, 6));
				if (self::$flagUseBeautyfulColumnnames ) {
					$type = $this->beau($type);
				}	
				$keys = $redbean->getAssociationManager()->related($this, $type);
				if (!count($keys)) $beans = array(); else
				if (trim($this->withSql) !== '') {
					$beans = $redbean->find($type, array('id' => $keys), array($this->withSql, $this->withParams), true);
				} else {
					$beans = $redbean->batch($type, $keys);
				}
				$this->withSql = '';
				$this->withParams = array();
				$this->properties[$property] = $beans;
				$this->__info['sys.shadow.'.$property] = $beans;
				$this->__info['tainted'] = true;
				return $this->properties[$property];
			} else {
				$null = null;
				return $null;
			}
		} else {
			return $this->properties[$property];
		}
	}
	/**
	 * Magic Setter. Sets the value for a specific property.
	 * This setter acts as a hook for OODB to mark beans as tainted.
	 * The tainted meta property can be retrieved using getMeta("tainted").
	 * The tainted meta property indicates whether a bean has been modified and
	 * can be used in various caching mechanisms.
	 * @param string $property
	 * @param  mixed $value
	 */
	public function __set($property, $value) {
		if (self::$flagUseBeautyfulColumnnames) {
			$property = $this->beau($property);
		}
		$this->flagSkipBeau = true;
		$this->__get($property);
		$this->flagSkipBeau = false;
		$this->setMeta('tainted', true);
		$linkField = $property.'_id';
		if (isset($this->properties[$linkField]) && !($value instanceof RedBean_OODBBean)) {
			if (is_null($value) || $value === false) {
				return $this->__unset($property);
			} else {
				throw new RedBean_Exception_Security('Cannot cast to bean.');
			}
		}
		if ($value === false) {
			$value = '0';
		} elseif ($value === true) {
			$value = '1';
		} elseif ($value instanceof DateTime) {
			$value = $value->format('Y-m-d H:i:s');
		}
		$this->properties[$property] = $value;
	}
	/**
	 * Sets a property directly, for internal use only.
	 * 
	 * @param string $property property
	 * @param mixed  $value    value
	 */
	public function setProperty($property, $value) {
		$this->properties[$property] = $value;
	}
	/**
	 * Returns the value of a meta property. A meta property
	 * contains extra information about the bean object that will not
	 * get stored in the database. Meta information is used to instruct
	 * RedBean as well as other systems how to deal with the bean.
	 * For instance: $bean->setMeta("buildcommand.unique", array(
	 * array("column1", "column2", "column3") ) );
	 * Will add a UNIQUE constaint for the bean on columns: column1, column2 and
	 * column 3.
	 * To access a Meta property we use a dot separated notation.
	 * If the property cannot be found this getter will return NULL instead.
	 * 
	 * @param string $path    path
	 * @param mixed  $default default value
	 * 
	 * @return mixed $value
	 */
	public function getMeta($path, $default = NULL) {
		return (isset($this->__info[$path])) ? $this->__info[$path] : $default;
	}
	/**
	 * Stores a value in the specified Meta information property. $value contains
	 * the value you want to store in the Meta section of the bean and $path
	 * specifies the dot separated path to the property. For instance "my.meta.property".
	 * If "my" and "meta" do not exist they will be created automatically.
	 * 
	 * @param string $path  path
	 * @param mixed  $value value
	 */
	public function setMeta($path, $value) {
		$this->__info[$path] = $value;
	}
	/**
	 * Copies the meta information of the specified bean
	 * This is a convenience method to enable you to
	 * exchange meta information easily.
	 * 
	 * @param RedBean_OODBBean $bean
	 * 
	 * @return RedBean_OODBBean
	 */
	public function copyMetaFrom(RedBean_OODBBean $bean) {
		$this->__info = $bean->__info;
		return $this;
	}
	/**
	 * Reroutes a call to Model if exists. (new fuse)
	 * 
	 * @param string $method
	 * @param array $args
	 * 
	 * @return mixed $mixed
	 */
	public function __call($method, $args) {
		if (!isset($this->__info['model'])) {
			$model = $this->beanHelper->getModelForBean($this);
			if (!$model) return;
			$this->__info['model'] = $model;
		}
		if (!method_exists($this->__info['model'], $method)) return null;
		return call_user_func_array(array($this->__info['model'], $method), $args);
	}
	/**
	 * Implementation of __toString Method
	 * Routes call to Model.
	 * 
	 * @return string $string
	 */
	public function __toString() {
		$string = $this->__call('__toString', array());
		if ($string === null) {
			return json_encode($this->properties);
		} else {
			return $string;
		}
	}
	/**
	 * Implementation of Array Access Interface, you can access bean objects
	 * like an array.
	 * Call gets routed to __set.
	 *
	 * @param  mixed $offset offset string
	 * @param  mixed $value value
	 *
	 * @return void
	 */
	public function offsetSet($offset, $value) {
		$this->__set($offset, $value);
	}
	/**
	 * Implementation of Array Access Interface, you can access bean objects
	 * like an array.
	 *
	 * @param  mixed $offset property
	 *
	 * @return
	 */
	public function offsetExists($offset) {
		return isset($this->properties[$offset]);
	}
	/**
	 * Implementation of Array Access Interface, you can access bean objects
	 * like an array.
	 * Unsets a value from the array/bean.
	 *
	 * @param  mixed $offset property
	 *
	 * @return
	 */
	public function offsetUnset($offset) {
		unset($this->properties[$offset]);
	}
	/**
	 * Implementation of Array Access Interface, you can access bean objects
	 * like an array.
	 * Returns value of a property.
	 *
	 * @param  mixed $offset property
	 *
	 * @return
	 */
	public function offsetGet($offset) {
		return $this->__get($offset);
	}
	/**
	 * Chainable method to cast a certain ID to a bean; for instance:
	 * $person = $club->fetchAs('person')->member;
	 * This will load a bean of type person using member_id as ID.
	 *
	 * @param  string $type preferred fetch type
	 *
	 * @return RedBean_OODBBean
	 */
	public function fetchAs($type) {
		$this->fetchType = $type;
		return $this;
	}
	/**
	* For polymorphic bean relations.
	* Same as fetchAs but uses a column instead of a direct value.
	*
	* @param string $column
	*
	* @return RedBean_OODBean
	*
	*/
	public function poly($field) {
		return $this->fetchAs($this->$field);
	}	
	/**
	 * Implementation of Countable interface. Makes it possible to use
	 * count() function on a bean.
	 * 
	 * @return integer $numberOfProperties number of properties in the bean. 
	 */
	public function count() {
		return count($this->properties);
	}
	/**
	 * Checks wether a bean is empty or not.
	 * A bean is empty if it has no other properties than the id field OR
	 * if all the other property are empty().
	 * 
	 * @return boolean 
	 */
	public function isEmpty() {
		$empty = true;
		foreach($this->properties as $key => $value) {
			if ($key == 'id') continue;
			if (!empty($value)) { 
				$empty = false;
			}	
		}
		return $empty;
	}
	/**
	 * Chainable setter.
	 * 
	 * @param string $property the property of the bean
	 * @param mixed  $value    the value you want to set 
	 * 
	 * @return RedBean_OODBBean the bean 
	 */
	public function setAttr($property, $value) {
		$this->$property = $value;
		return $this;
	}
	/**
	 * Comfort method.
	 * Unsets all properties in array.
	 * 
	 * @param array $properties properties you want to unset.
	 * 
	 * @return RedBean_OODBBean 
	 */
	public function unsetAll($properties) {
		foreach($properties as $prop) {
			if (isset($this->properties[$prop])) {
				unset($this->properties[$prop]);
			}
		}
		return $this;
	}
	/**
	 * Returns original (old) value of a property. 
	 * You can use this method to see what has changed in a
	 * bean.
	 * 
	 * @param string $property name of the property you want the old value of
	 * 
	 * @return mixed
	 */
	public function old($property) {
		$old = $this->getMeta('sys.orig', array());
		if (isset($old[$property])) {
			return $old[$property];
		}
	}
	/**
	 * Convenience method.
	 * Returns true if the bean has been changed, or false otherwise.
	 * Same as $bean->getMeta('tainted');
	 * Note that a bean becomes tainted as soon as you retrieve a list from
	 * the bean. This is because the bean lists are arrays and the bean cannot 
	 * determine whether you have made modifications to a list so RedBeanPHP
	 * will mark the whole bean as tainted.
	 * 
	 * @return boolean 
	 */
	public function isTainted() {
		return $this->getMeta('tainted');
	}
	/**
	 * Returns TRUE if the value of a certain property of the bean has been changed and
	 * FALSE otherwise.
	 * 
	 * @param string $property name of the property you want the change-status of
	 * 
	 * @return boolean 
	 */
	public function hasChanged($property) {
		if (!isset($this->properties[$property])) return false;
		return ($this->old($property) != $this->properties[$property]);
	}
	/**
	 * Creates a N-M relation by linking an intermediate bean.
	 * This method can be used to quickly connect beans using indirect
	 * relations. For instance, given an album and a song you can connect the two
	 * using a track with a number like this:
	 * 
	 * Usage:
	 * 
	 * $album->link('track', array('number'=>1))->song = $song;
	 * 
	 * or:
	 * 
	 * $album->link($trackBean)->song = $song;
	 * 	
	 * What this method does is adding the link bean to the own-list, in this case
	 * ownTrack. If the first argument is a string and the second is an array or
	 * a JSON string then the linking bean gets dispensed on-the-fly as seen in
	 * example #1. After preparing the linking bean, the bean is returned thus
	 * allowing the chained setter: ->song = $song.
	 * 
	 * @param string|RedBean_OODBBean $type          type of bean to dispense or the full bean
	 * @param string|array            $qualification JSON string or array (optional)
	 */
	public function link($typeOrBean, $qualification = array()) {
		if (is_string($typeOrBean)) {
			$bean = $this->beanHelper->getToolBox()->getRedBean()->dispense($typeOrBean);
			if (is_string($qualification)) {
				$data = json_decode($qualification, true);
			} else {
				$data = $qualification;
			}
			foreach($data as $key => $value) {
				$bean->$key = $value;
			}
		} else {
			$bean = $typeOrBean;
		}		
		$list = 'own'.ucfirst($bean->getMeta('type'));
		array_push($this->$list, $bean);
		return $bean;
	}
}

abstract class RedBean_Observable {
	/**
	 * @var array
	 */
	private $observers = array();
	/**
	 * Implementation of the Observer Pattern.
	 *
	 * @param string           $eventname event
	 * @param RedBean_Observer $observer observer
	 *
	 * @return void
	 */
	public function addEventListener($eventname, RedBean_Observer $observer) {
		if (!isset($this->observers[$eventname])) {
			$this->observers[$eventname] = array();
		}
		foreach($this->observers[$eventname] as $o) if ($o == $observer) return;
		$this->observers[$eventname][] = $observer;
	}
	/**
	 * Notifies listeners.
	 *
	 * @param string $eventname eventname
	 * @param mixed  $info      info
	 * @return unknown_ty
	 */
	public function signal($eventname, $info) {
		if (!isset($this->observers[ $eventname ])) $this->observers[$eventname] = array();
		foreach($this->observers[$eventname] as $observer) $observer->onEvent($eventname, $info);
	}
}

interface RedBean_Observer {
	/**
	 * @param string $eventname
	 * @param RedBean_OODBBean mixed $info
	 */
	public function onEvent($eventname, $bean);
}

interface RedBean_Adapter {
	/**
	 * Returns the latest SQL statement
	 *
	 * @return string $SQLString SQLString
	 */
	public function getSQL();
	/**
	 * Executes an SQL Statement using an array of values to bind
	 * If $noevent is TRUE then this function will not signal its
	 * observers to notify about the SQL execution; this to prevent
	 * infinite recursion when using observers.
	 *
	 * @param string  $sql     SQL
	 * @param array   $aValues values
	 * @param boolean $noevent no event firing
	 */
	public function exec($sql , $aValues = array(), $noevent = false);
	/**
	 * Executes an SQL Query and returns a resultset.
	 * This method returns a multi dimensional resultset similar to getAll
	 * The values array can be used to bind values to the place holders in the
	 * SQL query.
	 *
	 * @param string $sql     SQL
	 * @param array  $aValues values
	 */
	public function get($sql, $aValues = array());
	/**
	 * Executes an SQL Query and returns a resultset.
	 * This method returns a single row (one array) resultset.
	 * The values array can be used to bind values to the place holders in the
	 * SQL query.
	 *
	 * @param string $sql	  SQL
	 * @param array  $aValues values to bind
	 *
	 * @return array $aMultiDimArray row
	 */
	public function getRow($sql, $aValues = array());
	/**
	 * Executes an SQL Query and returns a resultset.
	 * This method returns a single column (one array) resultset.
	 * The values array can be used to bind values to the place holders in the
	 * SQL query.
	 *
	 * @param string $sql	  SQL
	 * @param array  $aValues values to bind
	 *
	 * @return array $aSingleDimArray column
	 */
	public function getCol($sql, $aValues = array());
	/**
	 * Executes an SQL Query and returns a resultset.
	 * This method returns a single cell, a scalar value as the resultset.
	 * The values array can be used to bind values to the place holders in the
	 * SQL query.
	 *
	 * @param string $sql     SQL
	 * @param array  $aValues values to bind
	 *
	 * @return string $sSingleValue value from cell
	 */
	public function getCell($sql, $aValues = array());
	/**
	 * Executes the SQL query specified in $sql and takes
	 * the first two columns of the resultset. This function transforms the
	 * resultset into an associative array. Values from the the first column will
	 * serve as keys while the values of the second column will be used as values.
	 * The values array can be used to bind values to the place holders in the
	 * SQL query.
	 *
	 * @param string $sql    SQL
	 * @param array  $values values to bind
	 *
	 * @return array $associativeArray associative array result set
	 */
	public function getAssoc($sql, $values = array());
	/**
	 * Returns the latest insert ID.
	 *
	 * @return integer $id primary key ID
	 */
	public function getInsertID();
	/**
	 * Returns the number of rows that have been
	 * affected by the last update statement.
	 *
	 * @return integer $count number of rows affected
	 */
	public function getAffectedRows();
	/**
	 * Returns the original database resource. This is useful if you want to
	 * perform operations on the driver directly instead of working with the
	 * adapter. RedBean will only access the adapter and never to talk
	 * directly to the driver though.
	 *
	 * @return object $driver driver
	 */
	public function getDatabase();
	/**
	 * This method is part of the RedBean Transaction Management
	 * mechanisms.
	 * Starts a transaction.
	 */
	public function startTransaction();
	/**
	 * This method is part of the RedBean Transaction Management
	 * mechanisms.
	 * Commits the transaction.
	 */
	public function commit();
	/**
	 * This method is part of the RedBean Transaction Management
	 * mechanisms.
	 * Rolls back the transaction.
	 */
	public function rollback();
	/**
	 * Closes database connection.
	 */
	public function close();
}

class RedBean_Adapter_DBAdapter extends RedBean_Observable implements RedBean_Adapter {
	private $db = null;
	private $sql = '';
	/**
	 * Constructor.
	 * Creates an instance of the RedBean Adapter Class.
	 * This class provides an interface for RedBean to work
	 * with ADO compatible DB instances.
	 *
	 * @param RedBean_Driver $database ADO Compatible DB Instance
	 */
	public function __construct($database) {
		$this->db = $database;
	}
	/**
	 * @see RedBean_Adapter::getSQL
	 */
	public function getSQL() {
		return $this->sql;
	}
	/**
	 * @see RedBean_Adapter::exec
	 */
	public function exec($sql, $aValues = array(), $noevent = false) {
		if (!$noevent) {
			$this->sql = $sql;
			$this->signal('sql_exec', $this);
		}
		return $this->db->Execute($sql, $aValues);
	}
	/**
	 * @see RedBean_Adapter::get
	 */
	public function get($sql, $aValues = array()) {
		$this->sql = $sql;
		$this->signal('sql_exec', $this);
		return $this->db->GetAll($sql, $aValues);
	}
	/**
	 * @see RedBean_Adapter::getRow
	 */
	public function getRow($sql, $aValues = array()) {
		$this->sql = $sql;
		$this->signal('sql_exec', $this);
		return $this->db->GetRow($sql, $aValues);
	}
	/**
	 * @see RedBean_Adapter::getCol
	 */
	public function getCol($sql, $aValues = array()) {
		$this->sql = $sql;
		$this->signal('sql_exec', $this);
		return $this->db->GetCol($sql, $aValues);
	}
	/**
	 * @see RedBean_Adapter::getAssoc
	 */
	public function getAssoc($sql, $aValues = array()) {
		$this->sql = $sql;
		$this->signal('sql_exec', $this);
		$rows = $this->db->GetAll($sql, $aValues);
		$assoc = array();
		if ($rows) {
			foreach($rows as $row) {
				if (is_array($row) && count($row)>0) {
					if (count($row)>1) {
						$key = array_shift($row);
						$value = array_shift($row);
					}
					elseif (count($row) == 1) {
						$key = array_shift($row);
						$value = $key;
					}
					$assoc[$key] = $value;
				}
			}
		}
		return $assoc;
	}
	/**
	 * @see RedBean_Adapter::getCell
	 */
	public function getCell($sql, $aValues = array(), $noSignal = null) {
		$this->sql = $sql;
		if (!$noSignal) $this->signal('sql_exec', $this);
		$arr = $this->db->getCol($sql, $aValues);
		if ($arr && is_array($arr))	return ($arr[0]); else return false;
	}
	/**
	 * @see RedBean_Adapter::getInsertID
	 */
	public function getInsertID() {
		return $this->db->getInsertID();
	}
	/**
	 * @see RedBean_Adapter::getAffectedRows
	 */
	public function getAffectedRows() {
		return $this->db->Affected_Rows();
	}
	/**
	 * @see RedBean_Adapter::getDatabase
	 */
	public function getDatabase() {
		return $this->db;
	}
	/**
	 * @see RedBean_Adapter::startTransaction
	 */
	public function startTransaction() {
		return $this->db->StartTrans();
	}
	/**
	 * @see RedBean_Adapter::commit
	 */
	public function commit() {
		return $this->db->CommitTrans();
	}
	/**
	 * @see RedBean_Adapter::rollback
	 */
	public function rollback() {
		return $this->db->FailTrans();
	}
	/**
	 * @see RedBean_Adapter::close.
	 */
	public function close() {
		$this->db->close();
	}
}

interface RedBean_QueryWriter {
	/**
	 * Query Writer constants.
	 */
	const C_SQLSTATE_NO_SUCH_TABLE = 1;
	const C_SQLSTATE_NO_SUCH_COLUMN = 2;
	const C_SQLSTATE_INTEGRITY_CONSTRAINT_VIOLATION = 3;

	/**
	 * Returns the tables that are in the database.
	 *
	 * @return array $arrayOfTables list of tables
	 */
	public function getTables();
	/**
	 * This method will create a table for the bean.
	 * This methods accepts a type and infers the corresponding table name.
	 *
	 * @param string $type type of bean you want to create a table for
	 *
	 * @return void
	 */
	public function createTable($type);
	/**
	 * Returns an array containing all the columns of the specified type.
	 * The format of the return array looks like this:
	 * $field => $type where $field is the name of the column and $type
	 * is a database specific description of the datatype.
	 *
	 * This methods accepts a type and infers the corresponding table name.
	 *
	 * @param string $type type of bean you want to obtain a column list of
	 *
	 * @return array $listOfColumns list of columns ($field=>$type)
	 */
	public function getColumns($type);
	/**
	 * Returns the Column Type Code (integer) that corresponds
	 * to the given value type. This method is used to determine the minimum
	 * column type required to represent the given value.
	 *
	 * @param string $value value
	 *
	 * @return integer $type type
	 */
	public function scanType($value, $alsoScanSpecialForTypes = false);
	/**
	 * This method will add a column to a table.
	 * This methods accepts a type and infers the corresponding table name.
	 *
	 * @param string  $type   name of the table
	 * @param string  $column name of the column
	 * @param integer $field  data type for field
	 *
	 * @return void
	 *
	 */
	public function addColumn($type, $column, $field);
	/**
	 * Returns the Type Code for a Column Description.
	 * Given an SQL column description this method will return the corresponding
	 * code for the writer. If the include specials flag is set it will also
	 * return codes for special columns. Otherwise special columns will be identified
	 * as specified columns.
	 *
	 * @param string  $typedescription description
	 * @param boolean $includeSpecials whether you want to get codes for special columns as well
	 *
	 * @return integer $typecode code
	 */
	public function code($typedescription, $includeSpecials = false);
	/**
	 * This method will widen the column to the specified data type.
	 * This methods accepts a type and infers the corresponding table name.
	 *
	 * @param string  $type       type / table that needs to be adjusted
	 * @param string  $column     column that needs to be altered
	 * @param integer $datatype   target data type
	 *
	 * @return void
	 */
	public function widenColumn($type, $column, $datatype);
	/**
	 * This method should update (or insert a record), it takes
	 * a table name, a list of update values ( $field => $value ) and an
	 * primary key ID (optional). If no primary key ID is provided, an
	 * INSERT will take place.
	 * Returns the new ID.
	 * This methods accepts a type and infers the corresponding table name.
	 *
	 * @param string  $type         name of the table to update
	 * @param array   $updatevalues list of update values
	 * @param integer $id			optional primary key ID value
	 *
	 * @return integer $id the primary key ID value of the new record
	 */
	public function updateRecord($type, $updatevalues, $id = null);
	/**
	 * This method selects a record. You can provide a
	 * collection of conditions using the following format:
	 * array( $field1 => array($possibleValue1, $possibleValue2,... $possibleValueN ),
	 * ...$fieldN=>array(...));
	 * Also, additional SQL can be provided. This SQL snippet will be appended to the
	 * query string. If the $delete parameter is set to TRUE instead of selecting the
	 * records they will be deleted.
	 * This methods accepts a type and infers the corresponding table name.
	 *
	 * @throws Exception
	 * @param string  $type    type of bean to select records from
	 * @param array   $cond    conditions using the specified format
	 * @param string  $asql    additional sql
	 * @param boolean $delete  IF TRUE delete records (optional)
	 * @param boolean $inverse IF TRUE inverse the selection (optional)
	 * @param boolean $all     IF TRUE suppress WHERE keyword, omitting WHERE clause
	 *
	 * @return array $records selected records
	 */
	public function selectRecord($type, $conditions, $addSql = null, $delete = false, $inverse = false);
	/**
	 * This method will add a UNIQUE constraint index to a table on columns $columns.
	 * This methods accepts a type and infers the corresponding table name.
	 *
	 * @param string $type               type
	 * @param array  $columnsPartOfIndex columns to include in index
	 *
	 * @return void
	 */
	public function addUniqueIndex($type, $columns);
	/**
	 * This method will check whether the SQL state is in the list of specified states
	 * and returns true if it does appear in this list or false if it
	 * does not. The purpose of this method is to translate the database specific state to
	 * a one of the constants defined in this class and then check whether it is in the list
	 * of standard states provided.
	 *
	 * @param string $state sql state
	 * @param array  $list  list
	 *
	 * @return boolean $isInList
	 */
	public function sqlStateIn($state, $list);
	/**
	 * This method will remove all beans of a certain type.
	 * This methods accepts a type and infers the corresponding table name.
	 *
	 * @param  string $type bean type
	 *
	 * @return void
	 */
	public function wipe($type);
	/**
	 * This method will count the number of beans of the given type.
	 * This methods accepts a type and infers the corresponding table name.
	 *
	 * @param  string $type type of bean to count
	 *
	 * @return integer $numOfBeans number of beans found
	 */
	public function count($type);
	/**
	 * This method will add a constraint. If one of the beans gets trashed
	 * the other, related bean will be removed as well.
	 *
	 * @param RedBean_OODBBean $bean1      first bean
	 * @param RedBean_OODBBean $bean2      second bean
	 *
	 * @return void
	 */
	public function addConstraint(RedBean_OODBBean $bean1, RedBean_OODBBean $bean2);
	/**
	 * This method will add a foreign key from type and field to
	 * target type and target field.
	 * The foreign key is created without an action. On delete/update
	 * no action will be triggered. The FK is only used to allow database
	 * tools to generate pretty diagrams and to make it easy to add actions
	 * later on.
	 * This methods accepts a type and infers the corresponding table name.
	 *
	 *
	 * @param  string $type	       type that will have a foreign key field
	 * @param  string $targetType  points to this type
	 * @param  string $field       field that contains the foreign key value
	 * @param  string $targetField field where the fk points to
	 *
	 * @return void
	 */
	public function addFK($type, $targetType, $field, $targetField);
	/**
	 * This method will add an index to a type and field with name
	 * $name.
	 * This methods accepts a type and infers the corresponding table name.
	 *
	 * @param  $type   type to add index to
	 * @param  $name   name of the new index
	 * @param  $column field to index
	 *
	 * @return void
	 */
	public function addIndex($type, $name, $column);
	
	/**
	 * Checks and filters a database structure element like a table of column
	 * for safe use in a query. A database structure has to conform to the
	 * RedBeanPHP DB security policy which basically means only alphanumeric
	 * symbols are allowed. This security policy is more strict than conventional
	 * SQL policies and does therefore not require database specific escaping rules.
	 * 
	 * @param string  $databaseStructure name of the column/table to check
	 * @param boolean $noQuotes          TRUE to NOT put backticks or quotes around the string
	 * 
	 * @return string 
	 */
	public function esc($databaseStructure, $dontQuote = false);
	
	/**
	 * Removes all tables and views from the database. 
	 */
	public function wipeAll();
}

abstract class RedBean_QueryWriter_AQueryWriter {
	/**
	 * @var array
	 */
	public $typeno_sqltype = array();
	/**
	 * @var RedBean_Adapter_DBAdapter
	 */
	protected $adapter;
	/**
	 * @var string
	 */
	protected $defaultValue = 'NULL';
	/**
	 * @var string
	 */
	protected $quoteCharacter = '';
	/**
	 * @var boolean
	 */
	protected $flagUseCache = false;
	/**
	 * @var array 
	 */
	protected $cache = array();
	/**
	 * @var array
	 */
	protected static $renames = array();
	/**
	 * Returns the sql that should follow an insert statement.
	 *
	 * @param string $table name
	 *
	 * @return string sql
	 */
  	protected function getInsertSuffix ($table) {
    	return '';
  	}
	/**
	 * @see RedBean_QueryWriter::esc
	 */
	public function esc($dbStructure, $dontQuote = false) {
		$this->check($dbStructure);
		return ($dontQuote) ? $dbStructure : $this->quoteCharacter.$dbStructure.$this->quoteCharacter;
	}
	/**
	 * Checks table name or column name.
	 *
	 * @param string $table table string
	 *
	 * @return string $table escaped string
	 */
	protected function check($struct) {
		if (!preg_match('/^[a-zA-Z0-9_]+$/', $struct)) {
		  throw new RedBean_Exception_Security('Identifier does not conform to RedBeanPHP security policies.');
	    }
		return $struct;
	}
	/**
	 * @see RedBean_QueryWriter::addColumn
	 */
	public function addColumn($type, $column, $field) {
		$table = $type;
		$type = $field;
		$table = $this->esc($table);
		$column = $this->esc($column);
		$type = (isset($this->typeno_sqltype[$type])) ? $this->typeno_sqltype[$type] : '';
		$sql = "ALTER TABLE $table ADD $column $type ";
		$this->adapter->exec($sql);
	}
	/**
	 * @see RedBean_QueryWriter::updateRecord
	 */
	public function updateRecord($type, $updatevalues, $id = null) {
		$table = $type;
		if (!$id) {
			$insertcolumns =  $insertvalues = array();
			foreach($updatevalues as $pair) {
				$insertcolumns[] = $pair['property'];
				$insertvalues[] = $pair['value'];
			}
			return $this->insertRecord($table, $insertcolumns, array($insertvalues));
		}
		if ($id && !count($updatevalues)) return $id;	
		$table = $this->esc($table);
		$sql = "UPDATE $table SET ";
		$p = $v = array();
		foreach($updatevalues as $uv) {
			$p[] = " {$this->esc($uv["property"])} = ? ";
			$v[] = $uv['value'];
		}
		$sql .= implode(',', $p).' WHERE id = '.intval($id);
		$this->adapter->exec($sql, $v);
		return $id;
	}
	/**
	 * Inserts a record into the database using a series of insert columns
	 * and corresponding insertvalues. Returns the insert id.
	 *
	 * @param string $table			  table to perform query on
	 * @param array  $insertcolumns columns to be inserted
	 * @param array  $insertvalues  values to be inserted
	 *
	 * @return integer $insertid	  insert id from driver, new record id
	 */
	protected function insertRecord($table, $insertcolumns, $insertvalues) {
		$default = $this->defaultValue;
		$suffix = $this->getInsertSuffix($table);
		$table = $this->esc($table);
		if (count($insertvalues)>0 && is_array($insertvalues[0]) && count($insertvalues[0])>0) {
			foreach($insertcolumns as $k => $v) {
				$insertcolumns[$k] = $this->esc($v);
			}
			$insertSQL = "INSERT INTO $table ( id, ".implode(',', $insertcolumns)." ) VALUES 
			( $default, ". implode(',', array_fill(0, count($insertcolumns), ' ? '))." ) $suffix";

			foreach($insertvalues as $i => $insertvalue) {
				$ids[] = $this->adapter->getCell($insertSQL, $insertvalue, $i);
			}
			$result = count($ids) === 1 ? array_pop($ids) : $ids;
		} else {
			$result = $this->adapter->getCell("INSERT INTO $table (id) VALUES($default) $suffix");
		}
		if ($suffix) return $result;
		$last_id = $this->adapter->getInsertID();
		return $last_id;
	}
	/**
	 * @see RedBean_QueryWriter::selectRecord
	 */
	public function selectRecord($type, $conditions, $addSql = null, $delete = null, $inverse = false, $all = false) { 
		if (!is_array($conditions)) throw new Exception('Conditions must be an array');
		if (!$delete && $this->flagUseCache) {
			$key = serialize(array($type, $conditions, $addSql, $inverse, $all));
			$sql = $this->adapter->getSQL();
			if (strpos($sql, '-- keep-cache') !== strlen($sql)-13) {
				//If SQL has been taken place outside of this method then something else then
				//a select query might have happened! (or instruct to keep cache)
				$this->cache = array();
			} else {
				if (isset($this->cache[$key])) return $this->cache[$key];
			}
		}
		$table = $this->esc($type);
		$sqlConditions = array();
		$bindings = array();
		foreach($conditions as $column => $values) {
			if (!count($values)) continue;
			$sql = $this->esc($column);
			$sql .= ' '.($inverse ? ' NOT ':'').' IN ( ';
			//If its safe to not use bindings please do... (fixes SQLite PDO issue limit 256 bindings)
			if (is_array($conditions)
				&& count($conditions) === 1 
				&& isset($conditions['id']) 
				&& is_array($values) 
				&& preg_match('/^\d+$/', implode('', $values))) {
				$sql .= implode(',', $values).') ';
				$sqlConditions[] = $sql;
			} else {
				$sql .= implode(',', array_fill(0, count($values), '?')).') ';
				$sqlConditions[] = $sql;
				if (!is_array($values)) $values = array($values);
				foreach($values as $k => $v) {
					$values[$k] = strval($v);
				}
				$bindings = array_merge($bindings, $values);
			}
		}
		if (is_array($addSql)) {
			if (count($addSql)>1) {
				$bindings = array_merge($bindings, $addSql[1]);
			} else {
				$bindings = array();
			}
			$addSql = $addSql[0];
		}
		$sql = '';
		if (is_array($sqlConditions) && count($sqlConditions)>0) {
			$sql = implode(' AND ', $sqlConditions);
			$sql = " WHERE ( $sql ) ";
			if ($addSql) $sql .= ($all ? '': ' AND ') . " $addSql ";
		}
		elseif ($addSql) {
			if ($all) {
				$sql = " $addSql ";
			} else {
				$sql = " WHERE $addSql ";
			}
		}
		$sql = (($delete) ? 'DELETE FROM ' : 'SELECT * FROM ').$table.$sql;
		$rows = $this->adapter->get($sql.(($delete) ? '' : ' -- keep-cache'), $bindings);
		if (!$delete && $this->flagUseCache) {
			$this->cache[$key] = $rows;
		}
		return $rows;
	}
	/**
	 * @see RedBean_QueryWriter::wipe
	 */
	public function wipe($type) {
		$table = $this->esc($type);
		$this->adapter->exec("TRUNCATE $table ");
	}
	/**
	 * @see RedBean_QueryWriter::count
	 */
	public function count($beanType, $addSQL = '', $params = array()) {
		$sql = "SELECT count(*) FROM {$this->esc($beanType)} ";
		if ($addSQL != '') $addSQL = ' WHERE '.$addSQL; 
		return (int) $this->adapter->getCell($sql.$addSQL, $params);
	}
	/**
	 * Checks whether a number can be treated like an int.
	 *
	 * @param  string $value string representation of a certain value
	 *
	 * @return boolean $value boolean result of analysis
	 */
	public static function canBeTreatedAsInt($value) {
		return (boolean) (ctype_digit(strval($value)) && strval($value) === strval(intval($value)));
	}
	/**
	 * @see RedBean_QueryWriter::addFK
	 */
	public function addFK($type, $targetType, $field, $targetField, $isDependent = false) {
		$table = $this->esc($type);
		$tableNoQ = $this->esc($type, true);
		$targetTable = $this->esc($targetType);
		$column = $this->esc($field);
		$columnNoQ = $this->esc($field, true);
		$targetColumn  = $this->esc($targetField);
		$targetColumnNoQ  = $this->esc($targetField, true);
		$db = $this->adapter->getCell('select database()');
		$fkName = 'fk_'.$tableNoQ.'_'.$columnNoQ.'_'.$targetColumnNoQ.($isDependent ? '_casc':'');
		$cName = 'cons_'.$fkName;
		$cfks =  $this->adapter->getCell("
			SELECT CONSTRAINT_NAME
			FROM information_schema.KEY_COLUMN_USAGE
			WHERE TABLE_SCHEMA ='$db' AND TABLE_NAME = '$tableNoQ'  AND COLUMN_NAME = '$columnNoQ' AND
			CONSTRAINT_NAME <>'PRIMARY' AND REFERENCED_TABLE_NAME is not null
		");
		$flagAddKey = false;
		try{
			//No keys
			if (!$cfks) {
				$flagAddKey = true; //go get a new key
			}
			//has fk, but different setting, --remove
			if ($cfks && $cfks != $cName) {
				$this->adapter->exec("ALTER TABLE $table DROP FOREIGN KEY $cfks ");
				$flagAddKey = true; //go get a new key.
			}
			if ($flagAddKey) { 
				$this->adapter->exec("ALTER TABLE  $table
				ADD CONSTRAINT $cName FOREIGN KEY $fkName (  $column ) REFERENCES  $targetTable (
				$targetColumn) ON DELETE ".($isDependent ? 'CASCADE':'SET NULL').' ON UPDATE SET NULL ;');
			}
		} catch(Exception $e) {} //Failure of fk-constraints is not a problem
	}
	/**
	 * Renames an association. For instance if you would like to refer to
	 * album_song as: track you can specify this by calling this method like:
	 * 
	 * renameAssociation('album_song','track')
	 * 
	 * This allows:
	 * 
	 * $album->sharedSong 
	 * 
	 * to add/retrieve beans from track instead of album_song.
	 * Also works for exportAll().
	 * 
	 * This method also accepts a single associative array as
	 * its first argument.
	 * 
	 * @param string|array $from
	 * @param string $to (optional)
	 * 
	 * @return void 
	 */
	public static function renameAssociation($from, $to = null) {
		if (is_array($from)) {
			foreach($from as $key => $value) self::$renames[$key] = $value;
			return;
		}
		self::$renames[$from] = $to;
	}
	/**
	 * Returns the format for link tables.
	 * Given an array containing two type names this method returns the
	 * name of the link table to be used to store and retrieve
	 * association records.
	 *
	 * @param  array $types two types array($type1, $type2)
	 *
	 * @return string $linktable name of the link table
	 */
	public static function getAssocTableFormat($types) {
		sort($types);
		$assoc = (implode('_', $types));
		return (isset(self::$renames[$assoc])) ? self::$renames[$assoc] : $assoc;
	}
	/**
	 * @see RedBean_QueryWriter::addConstraint
	 */
	public function addConstraint(RedBean_OODBBean $bean1, RedBean_OODBBean $bean2) {
		$table1 = $bean1->getMeta('type');
		$table2 = $bean2->getMeta('type');
		$writer = $this;
		$adapter = $this->adapter;
		$table = RedBean_QueryWriter_AQueryWriter::getAssocTableFormat(array($table1, $table2));
		$property1 = $bean1->getMeta('type') . '_id';
		$property2 = $bean2->getMeta('type') . '_id';
		if ($property1 == $property2) $property2 = $bean2->getMeta('type').'2_id';
		$table = $this->esc($table, true);
		$table1 = $this->esc($table1, true);
		$table2 = $this->esc($table2, true);
		$property1 = $this->esc($property1, true);
		$property2 = $this->esc($property2, true);
		//Dispatch to right method
		return $this->constrain($table, $table1, $table2, $property1, $property2);
	}
	/**
	 * Checks whether a value starts with zeros. In this case
	 * the value should probably be stored using a text datatype instead of a
	 * numerical type in order to preserve the zeros.
	 * 
	 * @param string $value value to be checked.
	 */
	protected function startsWithZeros($value) {
		$value = strval($value);
		if (strlen($value)>1 && strpos($value, '0') === 0 && strpos($value, '0.') !==0) {
			return true;
		} else {
			return false;
		}
	}
	/**
	 * Turns caching on or off. Default: off.
	 * If caching is turned on retrieval queries fired after eachother will
	 * use a result row cache.
	 * 
	 * @param boolean $yesNo 
	 */
	public function setUseCache($yesNo) {
		$this->flushCache();
		$this->flagUseCache = (boolean) $yesNo;
	}
	/**
	 * Flushes the Query Writer Cache.
	 */
	public function flushCache() {
		$this->cache = array();
	}
	/**
	 * @deprecated Use esc() instead.
	 */
	public function safeColumn($a, $b = false) { return $this->esc($a, $b); }
	public function safeTable($a, $b = false) { return $this->esc($a, $b); }
}

class RedBean_QueryWriter_MySQL extends RedBean_QueryWriter_AQueryWriter implements RedBean_QueryWriter {
	/**
	 * Data types
	 */
	const C_DATATYPE_BOOL = 0;
	const C_DATATYPE_UINT8 = 1;
	const C_DATATYPE_UINT32 = 2;
	const C_DATATYPE_DOUBLE = 3;
	const C_DATATYPE_TEXT8 = 4;
	const C_DATATYPE_TEXT16 = 5;
	const C_DATATYPE_TEXT32 = 6;
	const C_DATATYPE_SPECIAL_DATE = 80;
	const C_DATATYPE_SPECIAL_DATETIME = 81;
	const C_DATATYPE_SPECIAL_POINT = 90;
	const C_DATATYPE_SPECIFIED = 99;
	/**
	 * @var RedBean_Adapter_DBAdapter
	 */
	protected $adapter;
	/**
	 * @var string
	 */
  	protected $quoteCharacter = '`';
	/**
	 * Constructor
	 * 
	 * @param RedBean_Adapter $adapter Database Adapter
	 */
	public function __construct(RedBean_Adapter $adapter) {
		$this->typeno_sqltype = array(
			  RedBean_QueryWriter_MySQL::C_DATATYPE_BOOL => ' TINYINT(1) UNSIGNED ',
			  RedBean_QueryWriter_MySQL::C_DATATYPE_UINT8 => ' TINYINT(3) UNSIGNED ',
			  RedBean_QueryWriter_MySQL::C_DATATYPE_UINT32 => ' INT(11) UNSIGNED ',
			  RedBean_QueryWriter_MySQL::C_DATATYPE_DOUBLE => ' DOUBLE ',
			  RedBean_QueryWriter_MySQL::C_DATATYPE_TEXT8 => ' VARCHAR(255) ',
			  RedBean_QueryWriter_MySQL::C_DATATYPE_TEXT16 => ' TEXT ',
			  RedBean_QueryWriter_MySQL::C_DATATYPE_TEXT32 => ' LONGTEXT ',
			  RedBean_QueryWriter_MySQL::C_DATATYPE_SPECIAL_DATE => ' DATE ',
			  RedBean_QueryWriter_MySQL::C_DATATYPE_SPECIAL_DATETIME => ' DATETIME ',
			  RedBean_QueryWriter_MySQL::C_DATATYPE_SPECIAL_POINT => ' POINT ',
			);
		$this->sqltype_typeno = array();
		foreach($this->typeno_sqltype as $k => $v)
		$this->sqltype_typeno[trim(strtolower($v))] = $k;
		$this->adapter = $adapter;
	}
	/**
	 * This method returns the datatype to be used for primary key IDS and
	 * foreign keys. Returns one if the data type constants.
	 *
	 * @return integer $const data type to be used for IDS.
	 */
	public function getTypeForID() {
		return self::C_DATATYPE_UINT32;
	}
	/**
	 * @see RedBean_QueryWriter::getTables
	 */
	public function getTables() {
		return $this->adapter->getCol('show tables');
	}
	/**
	 * @see RedBean_QueryWriter::createTable
	 */
	public function createTable($table) {
		$table = $this->esc($table);
		$sql = "CREATE TABLE $table (
                     id INT( 11 ) UNSIGNED NOT NULL AUTO_INCREMENT ,
                     PRIMARY KEY ( id )
                     ) ENGINE = InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci ";
		$this->adapter->exec($sql);
	}
	/**
	 * @see RedBean_QueryWriter::getColumns
	 */
	public function getColumns($table) {
		$table = $this->esc($table);
		$columnsRaw = $this->adapter->get("DESCRIBE $table");
		foreach($columnsRaw as $r) $columns[$r['Field']] = $r['Type'];
		return $columns;
	}
	/**
	 * @see RedBean_QueryWriter::scanType
	 */
	public function scanType($value, $flagSpecial = false) {
		$this->svalue = $value;
		if (is_null($value)) return RedBean_QueryWriter_MySQL::C_DATATYPE_BOOL;
		if ($flagSpecial) {
			if (preg_match('/^\d{4}\-\d\d-\d\d$/', $value)) {
				return RedBean_QueryWriter_MySQL::C_DATATYPE_SPECIAL_DATE;
			}
			if (preg_match('/^\d{4}\-\d\d-\d\d\s\d\d:\d\d:\d\d$/', $value)) {
				return RedBean_QueryWriter_MySQL::C_DATATYPE_SPECIAL_DATETIME;
			}
		}
		$value = strval($value);
		if (!$this->startsWithZeros($value)) {

			if ($value === true || $value === false || $value === '1' || $value === '') {
				return RedBean_QueryWriter_MySQL::C_DATATYPE_BOOL;
			}
			if (is_numeric($value) && (floor($value) == $value) && $value >= 0 && $value <= 255 ) {
				return RedBean_QueryWriter_MySQL::C_DATATYPE_UINT8;
			}
			if (is_numeric($value) && (floor($value) == $value) && $value >= 0  && $value <= 4294967295 ) {
				return RedBean_QueryWriter_MySQL::C_DATATYPE_UINT32;
			}
			if (is_numeric($value)) {
				return RedBean_QueryWriter_MySQL::C_DATATYPE_DOUBLE;
			}
		}
		if (mb_strlen($value, 'UTF-8') <= 255) {
			return RedBean_QueryWriter_MySQL::C_DATATYPE_TEXT8;
		}
		if (mb_strlen($value, 'UTF-8') <= 65535) {
			return RedBean_QueryWriter_MySQL::C_DATATYPE_TEXT16;
		}
		return RedBean_QueryWriter_MySQL::C_DATATYPE_TEXT32;
	}
	/**
	 * @see RedBean_QueryWriter::code
	 */
	public function code($typedescription, $includeSpecials = false) {
		$r = ((isset($this->sqltype_typeno[$typedescription])) ? $this->sqltype_typeno[$typedescription] : self::C_DATATYPE_SPECIFIED);
		if ($includeSpecials) return $r;
		if ($r > self::C_DATATYPE_SPECIFIED) return self::C_DATATYPE_SPECIFIED;
		return $r;
	}
	/**
	  * @see RedBean_QueryWriter::wideColumn
	 */
	public function widenColumn($type, $column, $datatype) {
		$table = $type;
		$type = $datatype;
		$table = $this->esc($table);
		$column = $this->esc($column);
		$newtype = array_key_exists($type, $this->typeno_sqltype) ? $this->typeno_sqltype[$type] : '';
		$changecolumnSQL = "ALTER TABLE $table CHANGE $column $column $newtype ";
		$this->adapter->exec($changecolumnSQL);
	}
	/**
	 * @see RedBean_QueryWriter::addUniqueIndex
	 */
	public function addUniqueIndex($table, $columns) {
		$table = $this->esc($table);
		sort($columns); //else we get multiple indexes due to order-effects
		foreach($columns as $k => $v) {
			$columns[$k]= $this->esc($v);
		}
		$r = $this->adapter->get("SHOW INDEX FROM $table");
		$name = 'UQ_'.sha1(implode(',', $columns));
		if ($r) {
			foreach($r as $i) {
				if ($i['Key_name'] == $name) {
					return;
				}
			}
		}
		$sql = "ALTER IGNORE TABLE $table
                ADD UNIQUE INDEX $name (".implode(',', $columns).")";
		$this->adapter->exec($sql);
	}
	/**
	 * @see RedBean_QueryWriter::addIndex
	 */
	public function addIndex($type, $name, $column) {
		$table = $type;
		$table = $this->esc($table);
		$name = preg_replace('/\W/', '', $name);
		$column = $this->esc($column);
		foreach($this->adapter->get("SHOW INDEX FROM $table ") as $ind) if ($ind['Key_name'] === $name) return;
		try{ $this->adapter->exec("CREATE INDEX $name ON $table ($column) "); }catch(Exception $e){}
	}
	
	/**
	 * @see RedBean_QueryWriter::sqlStateIn
	 */
	public function sqlStateIn($state, $list) {
		$stateMap = array(
			'42S02' => RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_TABLE,
			'42S22' => RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_COLUMN,
			'23000' => RedBean_QueryWriter::C_SQLSTATE_INTEGRITY_CONSTRAINT_VIOLATION
		);
		return in_array((isset($stateMap[$state]) ? $stateMap[$state] : '0'), $list); 
	}
	/**
	 * Add the constraints for a specific database driver: MySQL.
	 * @todo Too many arguments; find a way to solve this in a neater way.
	 *
	 * @param string			  $table     table
	 * @param string			  $table1    table1
	 * @param string			  $table2    table2
	 * @param string			  $property1 property1
	 * @param string			  $property2 property2
	 *
	 * @return boolean $succes whether the constraint has been applied
	 */
	protected function constrain($table, $table1, $table2, $property1, $property2) {
		try{
			$db = $this->adapter->getCell('SELECT database()');
			$fks =  $this->adapter->getCell("
				SELECT count(*)
				FROM information_schema.KEY_COLUMN_USAGE
				WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? AND
				CONSTRAINT_NAME <>'PRIMARY' AND REFERENCED_TABLE_NAME IS NOT NULL
					  ", array($db, $table));
			//already foreign keys added in this association table
			if ($fks>0) return false;
			$columns = $this->getColumns($table);
			if ($this->code($columns[$property1]) !== RedBean_QueryWriter_MySQL::C_DATATYPE_UINT32) {
				$this->widenColumn($table, $property1, RedBean_QueryWriter_MySQL::C_DATATYPE_UINT32);
			}
			if ($this->code($columns[$property2]) !== RedBean_QueryWriter_MySQL::C_DATATYPE_UINT32) {
				$this->widenColumn($table, $property2, RedBean_QueryWriter_MySQL::C_DATATYPE_UINT32);
			}
			$sql = "
				ALTER TABLE ".$this->esc($table)."
				ADD FOREIGN KEY($property1) references `$table1`(id) ON DELETE CASCADE;
			";
			$this->adapter->exec($sql);
			$sql = "
				ALTER TABLE ".$this->esc($table)."
				ADD FOREIGN KEY($property2) references `$table2`(id) ON DELETE CASCADE
			";
			$this->adapter->exec($sql);
			return true;
		} catch(Exception $e){ return false; }
	}
	/**
	 * @see RedBean_QueryWriter::wipeAll
	 */
	public function wipeAll() {
		$this->adapter->exec('SET FOREIGN_KEY_CHECKS = 0;');
		foreach($this->getTables() as $t) {
	 		try{
	 			$this->adapter->exec("DROP TABLE IF EXISTS `$t`");
	 		}
	 		catch(Exception $e){}
	 		try{
	 			$this->adapter->exec("DROP VIEW IF EXISTS `$t`");
	 		}
	 		catch(Exception $e){}
		}
		$this->adapter->exec('SET FOREIGN_KEY_CHECKS = 1;');
	}
}

class RedBean_QueryWriter_SQLiteT extends RedBean_QueryWriter_AQueryWriter implements RedBean_QueryWriter {
	/**
	 * @var RedBean_Adapter_DBAdapter
	 */
	protected $adapter;
	/**
	 * @var string
	 */
  	protected $quoteCharacter = '`';
	/**
	 * Data types
	 */
	const C_DATATYPE_INTEGER = 0;
	const C_DATATYPE_NUMERIC = 1;
	const C_DATATYPE_TEXT = 2;
	const C_DATATYPE_SPECIFIED = 99;
	/**
	 * Constructor
	 * 
	 * @param RedBean_Adapter $adapter Database Adapter
	 */
	public function __construct(RedBean_Adapter $adapter) {
		$this->typeno_sqltype = array(
			  RedBean_QueryWriter_SQLiteT::C_DATATYPE_INTEGER => 'INTEGER',
			  RedBean_QueryWriter_SQLiteT::C_DATATYPE_NUMERIC => 'NUMERIC',
			  RedBean_QueryWriter_SQLiteT::C_DATATYPE_TEXT => 'TEXT',
		);
		$this->sqltype_typeno = array();
		foreach($this->typeno_sqltype as $k => $v)
		$this->sqltype_typeno[$v] = $k;
		$this->adapter = $adapter;
	}
	/**
	 * This method returns the datatype to be used for primary key IDS and
	 * foreign keys. Returns one if the data type constants.
	 *
	 * @return integer $const data type to be used for IDS.
	 */
	public function getTypeForID() {
		return self::C_DATATYPE_INTEGER;
	}
	/**
	 * @see RedBean_QueryWriter::scanType
	 */
	public function scanType($value, $flagSpecial = false) {
		$this->svalue = $value;
		if ($value === false) return self::C_DATATYPE_INTEGER;
		if ($value === null) return self::C_DATATYPE_INTEGER;
		if ($this->startsWithZeros($value)) return self::C_DATATYPE_TEXT;
		if (is_numeric($value) && (intval($value) == $value) && $value<2147483648) return self::C_DATATYPE_INTEGER;
		if ((is_numeric($value) && $value < 2147483648)
				  || preg_match('/\d{4}\-\d\d\-\d\d/', $value)
				  || preg_match('/\d{4}\-\d\d\-\d\d\s\d\d:\d\d:\d\d/', $value)
		) {
			return self::C_DATATYPE_NUMERIC;
		}
		return self::C_DATATYPE_TEXT;
	}
	/**
	 * @see RedBean_QueryWriter::addColumn
	 */
	public function addColumn($table, $column, $type) {
		$column = $this->check($column);
		$table = $this->check($table);
		$type = $this->typeno_sqltype[$type];
		$sql = "ALTER TABLE `$table` ADD `$column` $type ";
		$this->adapter->exec($sql);
	}
	/**
	 * @see RedBean_QueryWriter::code
	 */
	public function code($typedescription, $includeSpecials = false) {
		$r =  ((isset($this->sqltype_typeno[$typedescription])) ? $this->sqltype_typeno[$typedescription] : 99);
		if ($includeSpecials) return $r;
		if ($r > self::C_DATATYPE_SPECIFIED) return self::C_DATATYPE_SPECIFIED;
		return $r;
	}
	/**
	 * Gets all information about a table (from a type).
	 * 
	 * Format:
	 * array(
	 *		name => name of the table
	 *		columns => array( name => datatype )
	 *		indexes => array() raw index information rows from PRAGMA query
	 *		keys => array() raw key information rows from PRAGMA query
	 * )
	 * 
	 * @param string $type type you want to get info of
	 * 
	 * @return array $info 
	 */
	protected function getTable($type) {
		$tableName = $this->esc($type, true);
		$columns = $this->getColumns($type);
		$indexes = $this->getIndexes($type);
		$keys = $this->getKeys($type);
		$table = array('columns' => $columns, 'indexes' => $indexes, 'keys' => $keys, 'name' => $tableName);
		$this->tableArchive[$tableName] = $table;
		return $table;
	}
	/**
	 * Puts a table. Updates the table structure.
	 * In SQLite we can't change columns, drop columns, change or add foreign keys so we
	 * have a table-rebuild function. You simply load your table with getTable(), modify it and
	 * then store it with putTable()...
	 * 
	 * @param array $tableMap information array 
	 */
	protected function putTable($tableMap) {
		$table = $tableMap['name'];
		$q = array();
		$q[] = "DROP TABLE IF EXISTS tmp_backup;";
		$oldColumnNames = array_keys($this->getColumns($table));
		foreach($oldColumnNames as $k => $v) $oldColumnNames[$k] = "`$v`";
		$q[] = "CREATE TEMPORARY TABLE tmp_backup(".implode(",", $oldColumnNames).");";
		$q[] = "INSERT INTO tmp_backup SELECT * FROM `$table`;";
		$q[] = "PRAGMA foreign_keys = 0 ";
		$q[] = "DROP TABLE `$table`;";
		$newTableDefStr = '';
		foreach($tableMap['columns'] as $column => $type) {
			if ($column != 'id') {
				$newTableDefStr .= ",`$column` $type";
			}
		}
		$fkDef = '';
		foreach($tableMap['keys'] as $key) {
			$fkDef .= ", FOREIGN KEY(`{$key['from']}`) 
						 REFERENCES `{$key['table']}`(`{$key['to']}`) 
						 ON DELETE {$key['on_delete']} ON UPDATE {$key['on_update']}";
		}
		$q[] = "CREATE TABLE `$table` ( `id` INTEGER PRIMARY KEY AUTOINCREMENT  $newTableDefStr  $fkDef );";
		foreach($tableMap['indexes'] as $name => $index)  {
			if (strpos($name, 'UQ_') === 0) {
				$cols = explode('__', substr($name, strlen('UQ_'.$table)));
				foreach($cols as $k => $v) $cols[$k] = "`$v`";
				$q[] = "CREATE UNIQUE INDEX $name ON `$table` (".implode(',', $cols).")";
			}
			else $q[] = "CREATE INDEX $name ON `$table` ({$index['name']}) ";
		}
		$q[] = "INSERT INTO `$table` SELECT * FROM tmp_backup;";
		$q[] = "DROP TABLE tmp_backup;";
		$q[] = "PRAGMA foreign_keys = 1 ";
		foreach($q as $sq) $this->adapter->exec($sq);
	}
	/**
	 * @see RedBean_QueryWriter::widenColumn
	 */
	public function widenColumn($type, $column, $datatype) {
		$t = $this->getTable($type);
		$t['columns'][$column] = $this->typeno_sqltype[$datatype];
		$this->putTable($t);
	}
	/**
	 * @see RedBean_QueryWriter::getTables();
	 */
	public function getTables() {
		return $this->adapter->getCol("SELECT name FROM sqlite_master
			WHERE type='table' AND name!='sqlite_sequence';");
	}
	/**
	 * @see RedBean_QueryWriter::createTable
	 */
	public function createTable($table) {
		$table = $this->esc($table);
		$sql = "CREATE TABLE $table ( id INTEGER PRIMARY KEY AUTOINCREMENT ) ";
		$this->adapter->exec($sql);
	}
	/**
	 * @see RedBean_QueryWriter::getColumns
	 */
	public function getColumns($table) {
		$table = $this->esc($table, true);
		$columnsRaw = $this->adapter->get("PRAGMA table_info('$table')");
		$columns = array();
		foreach($columnsRaw as $r) $columns[$r['name']] = $r['type'];
		return $columns;
	}
	/**
	 * Returns the indexes for type $type.
	 * 
	 * @param string $type
	 * 
	 * @return array $indexInfo index information
	 */
	protected function getIndexes($type) {
		$table = $this->esc($type, true);
		$indexes = $this->adapter->get("PRAGMA index_list('$table')");
		$indexInfoList = array();
		foreach($indexes as $i) {
			$indexInfoList[$i['name']] = $this->adapter->getRow("PRAGMA index_info('{$i['name']}') ");
			$indexInfoList[$i['name']]['unique'] = $i['unique'];
		}
		return $indexInfoList;
	}
	/**
	 * Returns the keys for type $type.
	 * 
	 * @param string $type
	 * 
	 * @return array $keysInfo keys information
	 */
	protected function getKeys($type) {
		$table = $this->esc($type, true);
		$keys = $this->adapter->get("PRAGMA foreign_key_list('$table')");
		$keyInfoList = array();
		foreach($keys as $k) {
			$keyInfoList['from_'.$k['from'].'_to_table_'.$k['table'].'_col_'.$k['to']] = $k;
		}
		return $keyInfoList;
	}
	/**
	 * @see RedBean_QueryWriter::addUniqueIndex
	 */
	public function addUniqueIndex($type, $columns) {
		$table = $this->esc($type, true);
		$name = 'UQ_'.$table.implode('__', $columns);
		$t = $this->getTable($type);
		if (isset($t['indexes'][$name])) return;
		$t['indexes'][$name] = array('name' => $name);
		$this->putTable($t);
	}
	/**
	 * @see RedBean_QueryWriter::sqlStateIn
	 */
	public function sqlStateIn($state, $list) {
		$stateMap = array(
			'HY000' => RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_TABLE,
			'23000' => RedBean_QueryWriter::C_SQLSTATE_INTEGRITY_CONSTRAINT_VIOLATION
		);
		return in_array((isset($stateMap[$state]) ? $stateMap[$state] : '0'), $list);
	}
	/**
	 * @see RedBean_QueryWriter::addIndex
	 */
	public function addIndex($type, $name, $column) {
		$table = $type;
		$table = $this->esc($table);
		$name = preg_replace('/\W/', '', $name);
		$column = $this->esc($column, true);
		foreach($this->adapter->get("PRAGMA INDEX_LIST($table) ") as $ind) if ($ind['name'] === $name) return;
		$t = $this->getTable($type);
		$t['indexes'][$name] = array('name' => $column);
		return $this->putTable($t);
	}
	/**
	 * @see RedBean_QueryWriter::wipe
	 */
	public function wipe($type) {
		$table = $this->esc($type);
		$this->adapter->exec("DELETE FROM $table");
	}
	/**
	 * @see RedBean_QueryWriter::addFK
	 */
	public function addFK($type, $targetType, $field, $targetField, $isDep = false) {
		return $this->buildFK($type, $targetType, $field, $targetField, $isDep);
	}
	/**
	 * Adds a foreign key to a type
	 *
	 * @param  string $type        type you want to modify table of
	 * @param  string $targetType  target type
	 * @param  string $field       field of the type that needs to get the fk
	 * @param  string $targetField field where the fk needs to point to
	 * @param  integer $buildopt   0 = NO ACTION, 1 = ON DELETE CASCADE
	 *
	 * @return boolean $didIt
	 * 
	 * @note: cant put this in try-catch because that can hide the fact
	 * that database has been damaged. 
	 */
	protected function buildFK($type, $targetType, $field, $targetField, $constraint = false) {
		$consSQL = ($constraint ? 'CASCADE' : 'SET NULL');
		$t = $this->getTable($type);
		$label = 'from_'.$field.'_to_table_'.$targetType.'_col_'.$targetField;
		if (isset($t['keys'][$label]) 
				&& $t['keys'][$label]['table'] === $targetType 
				&& $t['keys'][$label]['from'] === $field
				&& $t['keys'][$label]['to'] === $targetField
				&& $t['keys'][$label]['on_delete'] === $consSQL
		) return false;
		$t['keys'][$label] = array(
			'table' => $targetType,
			'from' => $field,
			'to' => $targetField,
			'on_update' => 'SET NULL',
			'on_delete' => $consSQL
		);
		$this->putTable($t);
		return true;
	}
	/**
	 * Add the constraints for a specific database driver: SQLite.
	 * @todo Too many arguments; find a way to solve this in a neater way.
	 *
	 * @param string			  $table     table
	 * @param string			  $table1    table1
	 * @param string			  $table2    table2
	 * @param string			  $property1 property1
	 * @param string			  $property2 property2
	 *
	 * @return boolean $succes whether the constraint has been applied
	 */
	protected  function constrain($table, $table1, $table2, $property1, $property2) {
		$writer = $this;
		$adapter = $this->adapter;
		$firstState = $this->buildFK($table, $table1, $property1, 'id', true);
		$secondState = $this->buildFK($table, $table2, $property2, 'id', true);
		return ($firstState && $secondState);
	}
	/**
	 * @see RedBean_QueryWriter::wipeAll
	 */
	public function wipeAll() {
		$this->adapter->exec('PRAGMA foreign_keys = 0 ');
		foreach($this->getTables() as $t) {
	 		try{
	 			$this->adapter->exec("drop table if exists`$t`");
	 		} catch(Exception $e){}
	 		try{
	 			$this->adapter->exec("drop view if exists`$t`");
	 		} catch(Exception $e){}
		}
		$this->adapter->exec('PRAGMA foreign_keys = 1 ');
	}
}

class RedBean_QueryWriter_PostgreSQL extends RedBean_QueryWriter_AQueryWriter implements RedBean_QueryWriter {
	/**
	 * Data types
	 */
	const C_DATATYPE_INTEGER = 0;
	const C_DATATYPE_DOUBLE = 1;
	const C_DATATYPE_TEXT = 3;
	const C_DATATYPE_SPECIAL_DATE = 80;
	const C_DATATYPE_SPECIAL_DATETIME = 81;
	const C_DATATYPE_SPECIAL_POINT = 90;
	const C_DATATYPE_SPECIAL_LSEG = 91;
	const C_DATATYPE_SPECIAL_CIRCLE = 92;
	const C_DATATYPE_SPECIAL_MONEY = 93;
	const C_DATATYPE_SPECIFIED = 99;
	/**
	 * @var RedBean_DBAdapter
	 */
	protected $adapter;
	/**
	 * @var string
	 */
	protected $quoteCharacter = '"';
	/**
	 * @var string
	 */
	protected $defaultValue = 'DEFAULT';
	/**
	* This method returns the datatype to be used for primary key IDS and
	* foreign keys. Returns one if the data type constants.
	*
	* @return integer $const data type to be used for IDS.
	*/
	public function getTypeForID() {
		return self::C_DATATYPE_INTEGER;
	}
	/**
	 * Returns the insert suffix SQL Snippet
	 *
	 * @param string $table table
	 *
	 * @return  string $sql SQL Snippet
	 */
	protected function getInsertSuffix($table) {
		return 'RETURNING id ';
	}
	/**
	 * Constructor
	 * 
	 * @param RedBean_Adapter $adapter Database Adapter
	 */
	public function __construct(RedBean_Adapter $adapter) {	
		$this->typeno_sqltype = array(
				  self::C_DATATYPE_INTEGER => ' integer ',
				  self::C_DATATYPE_DOUBLE => ' double precision ',
				  self::C_DATATYPE_TEXT => ' text ',
				  self::C_DATATYPE_SPECIAL_DATE => ' date ',
				  self::C_DATATYPE_SPECIAL_DATETIME => ' timestamp without time zone ',
				  self::C_DATATYPE_SPECIAL_POINT => ' point ',
				  self::C_DATATYPE_SPECIAL_LSEG => ' lseg ',
				  self::C_DATATYPE_SPECIAL_CIRCLE => ' circle ',
				  self::C_DATATYPE_SPECIAL_MONEY => ' money ',
		);
		$this->sqltype_typeno = array();
		foreach($this->typeno_sqltype as $k => $v)
		$this->sqltype_typeno[trim(strtolower($v))] = $k;	
		$this->adapter = $adapter;
	}
	/**
	 * @see RedBean_QueryWriter::getTables
	 */
	public function getTables() {
		return $this->adapter->getCol("select table_name from information_schema.tables
		where table_schema = 'public'");
	}
	/**
	 * @see RedBean_QueryWriter::createTable
	 */
	public function createTable($table) {
		$table = $this->esc($table);
		$sql = " CREATE TABLE $table (id SERIAL PRIMARY KEY); ";
		$this->adapter->exec($sql);
	}
	/**
	 * @see RedBean_QueryWriter::getColumns
	 */
	public function getColumns($table) {
		$table = $this->esc($table, true);
		$columnsRaw = $this->adapter->get("select column_name, data_type from information_schema.columns where table_name='$table'");
		foreach($columnsRaw as $r) {
			$columns[$r['column_name']] = $r['data_type'];
		}
		return $columns;
	}
	/**
	 * @see RedBean_QueryWriter::scanType
	 */
	public function scanType($value, $flagSpecial = false) {
		$this->svalue = $value;
		if ($flagSpecial && $value) {
			if (preg_match('/^\d{4}\-\d\d-\d\d$/', $value)) {
				return RedBean_QueryWriter_PostgreSQL::C_DATATYPE_SPECIAL_DATE;
			}
			if (preg_match('/^\d{4}\-\d\d-\d\d\s\d\d:\d\d:\d\d(\.\d{1,6})?$/', $value)) {
				return RedBean_QueryWriter_PostgreSQL::C_DATATYPE_SPECIAL_DATETIME;
			}
			if (preg_match('/^\([\d\.]+,[\d\.]+\)$/', $value)) {
				return RedBean_QueryWriter_PostgreSQL::C_DATATYPE_SPECIAL_POINT;
			}
			if (preg_match('/^\[\([\d\.]+,[\d\.]+\),\([\d\.]+,[\d\.]+\)\]$/', $value)) {
				return RedBean_QueryWriter_PostgreSQL::C_DATATYPE_SPECIAL_LSEG;
			}
			if (preg_match('/^\<\([\d\.]+,[\d\.]+\),[\d\.]+\>$/', $value)) {
				return RedBean_QueryWriter_PostgreSQL::C_DATATYPE_SPECIAL_CIRCLE;
			}
			if (preg_match('/^\-?\$\d+/', $value)) {
				return RedBean_QueryWriter_PostgreSQL::C_DATATYPE_SPECIAL_MONEY;
			}
		}
		$sz = ($this->startsWithZeros($value));
		if ($sz) return self::C_DATATYPE_TEXT;
		if ($value === null || ($value instanceof RedBean_Driver_PDO_NULL) ||(is_numeric($value)
				  && floor($value) == $value
				  && $value < 2147483648
				  && $value > -2147483648)) {
			return self::C_DATATYPE_INTEGER;
		}
		elseif(is_numeric($value)) {
			return self::C_DATATYPE_DOUBLE;
		} else {
			return self::C_DATATYPE_TEXT;
		}
	}
	/**
	 * @see RedBean_QueryWriter::code
	 */
	public function code($typedescription, $includeSpecials = false) {
		$r = ((isset($this->sqltype_typeno[$typedescription])) ? $this->sqltype_typeno[$typedescription] : 99);
		if ($includeSpecials) return $r;
		if ($r > self::C_DATATYPE_SPECIFIED) return self::C_DATATYPE_SPECIFIED;
		return $r;
	}
	/**
	 * @see RedBean_QueryWriter::widenColumn
	 */
	public function widenColumn($type, $column, $datatype) {
		$table = $type;
		$type = $datatype;
		$table = $this->esc($table);
		$column = $this->esc($column);
		$newtype = $this->typeno_sqltype[$type];
		$changecolumnSQL = "ALTER TABLE $table \n\t ALTER COLUMN $column TYPE $newtype ";
		$this->adapter->exec($changecolumnSQL);
	}
	/**
	 * @see RedBean_QueryWriter::addUniqueIndex
	 */
	public function addUniqueIndex($table, $columns) {
		$table = $this->esc($table, true);
		sort($columns); //else we get multiple indexes due to order-effects
		foreach($columns as $k => $v) {
			$columns[$k] = $this->esc($v);
		}
		$r = $this->adapter->get("SELECT
									i.relname as index_name
								FROM
									pg_class t,
									pg_class i,
									pg_index ix,
									pg_attribute a
								WHERE
									t.oid = ix.indrelid
									AND i.oid = ix.indexrelid
									AND a.attrelid = t.oid
									AND a.attnum = ANY(ix.indkey)
									AND t.relkind = 'r'
									AND t.relname = '$table'
								ORDER BY  t.relname,  i.relname;");

		$name = "UQ_".sha1($table.implode(',', $columns));
		if ($r) {
			foreach($r as $i) {
				if (strtolower($i['index_name']) == strtolower($name)) {
					return;
				}
			}
		}
		$sql = "ALTER TABLE \"$table\"
                ADD CONSTRAINT $name UNIQUE (".implode(',', $columns).")";
		$this->adapter->exec($sql);
	}
	/**
	 * @see RedBean_QueryWriter::sqlStateIn
	 */
	public function sqlStateIn($state, $list) {
		$stateMap = array(
			'42P01' => RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_TABLE,
			'42703' => RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_COLUMN,
			'23505' => RedBean_QueryWriter::C_SQLSTATE_INTEGRITY_CONSTRAINT_VIOLATION
		);
		return in_array((isset($stateMap[$state]) ? $stateMap[$state] : '0'), $list);
	}
	/**
	 * @see RedBean_QueryWriter::addIndex
	 */
	public function addIndex($type, $name, $column) {
		$table = $type;
		$table = $this->esc($table);
		$name = preg_replace('/\W/', '', $name);
		$column = $this->esc($column);
		if ($this->adapter->getCell("SELECT COUNT(*) FROM pg_class WHERE relname = '$name'")) return;
		try{ $this->adapter->exec("CREATE INDEX $name ON $table ($column) "); }catch(Exception $e){}
	}
	/**
	 * @see RedBean_QueryWriter::addFK
	 */
	public function addFK($type, $targetType, $field, $targetField, $isDep = false) {
		try{
			$table = $this->esc($type);
			$column = $this->esc($field);
			$tableNoQ = $this->esc($type, true);
			$columnNoQ = $this->esc($field, true);
			$targetTable = $this->esc($targetType);
			$targetTableNoQ = $this->esc($targetType, true);
			$targetColumn  = $this->esc($targetField);
			$targetColumnNoQ  = $this->esc($targetField, true);
			$sql = "SELECT
					tc.constraint_name, 
					tc.table_name, 
					kcu.column_name, 
					ccu.table_name AS foreign_table_name,
					ccu.column_name AS foreign_column_name,
					rc.delete_rule
					FROM 
					information_schema.table_constraints AS tc 
					JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name
					JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name
					JOIN information_schema.referential_constraints AS rc ON ccu.constraint_name = rc.constraint_name
					WHERE constraint_type = 'FOREIGN KEY' AND tc.table_catalog=current_database()
					AND tc.table_name = '$tableNoQ' 
					AND ccu.table_name = '$targetTableNoQ'
					AND kcu.column_name = '$columnNoQ'
					AND ccu.column_name = '$targetColumnNoQ'
					";
	
			
			$row = $this->adapter->getRow($sql);
			$flagAddKey = false;
			if (!$row) $flagAddKey = true;
			if ($row) { 
				if (($row['delete_rule'] == 'SET NULL' && $isDep) || 
					($row['delete_rule'] != 'SET NULL' && !$isDep)) {
					//delete old key
					$flagAddKey = true; //and order a new one
					$cName = $row['constraint_name'];
					$sql = "ALTER TABLE $table DROP CONSTRAINT $cName ";
					$this->adapter->exec($sql);
				} 
			}
			if ($flagAddKey) {
			$delRule = ($isDep ? 'CASCADE' : 'SET NULL');	
			$this->adapter->exec("ALTER TABLE  $table
					ADD FOREIGN KEY (  $column ) REFERENCES  $targetTable (
					$targetColumn) ON DELETE $delRule ON UPDATE SET NULL DEFERRABLE ;");
					return true;
			}
			return false;
		} catch(Exception $e){ return false; }
	}
	/**
	 * Add the constraints for a specific database driver: PostgreSQL.
	 * @todo Too many arguments; find a way to solve this in a neater way.
	 *
	 * @param string			  $table     table
	 * @param string			  $table1    table1
	 * @param string			  $table2    table2
	 * @param string			  $property1 property1
	 * @param string			  $property2 property2
	 *
	 * @return boolean $succes whether the constraint has been applied
	 */
	protected function constrain($table, $table1, $table2, $property1, $property2) {
		try{
			$writer = $this;
			$adapter = $this->adapter;
			$fkCode = 'fk'.md5($table.$property1.$property2);
			$sql = "
						SELECT
								c.oid,
								n.nspname,
								c.relname,
								n2.nspname,
								c2.relname,
								cons.conname
						FROM pg_class c
						JOIN pg_namespace n ON n.oid = c.relnamespace
						LEFT OUTER JOIN pg_constraint cons ON cons.conrelid = c.oid
						LEFT OUTER JOIN pg_class c2 ON cons.confrelid = c2.oid
						LEFT OUTER JOIN pg_namespace n2 ON n2.oid = c2.relnamespace
						WHERE c.relkind = 'r'
						AND n.nspname IN ('public')
						AND (cons.contype = 'f' OR cons.contype IS NULL)
						AND
						(  cons.conname = '{$fkCode}a'	OR  cons.conname = '{$fkCode}b' )

					  ";

			$rows = $adapter->get($sql);
			if (!count($rows)) {
				$sql1 = "ALTER TABLE \"$table\" ADD CONSTRAINT
						  {$fkCode}a FOREIGN KEY ($property1)
							REFERENCES \"$table1\" (id) ON DELETE CASCADE ";
				$sql2 = "ALTER TABLE \"$table\" ADD CONSTRAINT
						  {$fkCode}b FOREIGN KEY ($property2)
							REFERENCES \"$table2\" (id) ON DELETE CASCADE ";
				$adapter->exec($sql1);
				$adapter->exec($sql2);
			}
			return true;
		}
		catch(Exception $e){ return false; }
	}
	/**
	 * @see RedBean_QueryWriter::wipeAll
	 */
	public function wipeAll() {
      	$this->adapter->exec('SET CONSTRAINTS ALL DEFERRED');
      	foreach($this->getTables() as $t) {
      		$t = $this->esc($t);
	 		try{
	 			$this->adapter->exec("drop table if exists $t CASCADE ");
	 		} catch(Exception $e){}
	 		try{
	 			$this->adapter->exec("drop view if exists $t CASCADE ");
	 		} catch(Exception $e){  throw $e; }
		}
		$this->adapter->exec('SET CONSTRAINTS ALL IMMEDIATE');
	}
	/**
	 * @see RedBean_QueryWriter::wipe
	 */
	public function wipe($type) {
		$table = $type;
		$table = $this->esc($table);
		$sql = "TRUNCATE $table CASCADE";
		$this->adapter->exec($sql);
	}
}

class RedBean_QueryWriter_CUBRID extends RedBean_QueryWriter_AQueryWriter implements RedBean_QueryWriter {
	/**
	 * Data types
	 */
	const C_DATATYPE_INTEGER = 0;
	const C_DATATYPE_DOUBLE = 1;
	const C_DATATYPE_STRING = 2;
	const C_DATATYPE_SPECIAL_DATE = 80;
	const C_DATATYPE_SPECIAL_DATETIME = 81;
	const C_DATATYPE_SPECIFIED = 99;
	/**
	 * @var RedBean_Adapter_DBAdapter
	 */
	protected $adapter;
	/**
	 * @var string
	 */
  	protected $quoteCharacter = '`';
	/**
	 * Constructor
	 * 
	 * @param RedBean_Adapter $adapter Database Adapter
	 */
	public function __construct(RedBean_Adapter $adapter) {
		$this->typeno_sqltype = array(
			RedBean_QueryWriter_CUBRID::C_DATATYPE_INTEGER => ' INTEGER ',
			RedBean_QueryWriter_CUBRID::C_DATATYPE_DOUBLE => ' DOUBLE ',
			RedBean_QueryWriter_CUBRID::C_DATATYPE_STRING => ' STRING ',
			RedBean_QueryWriter_CUBRID::C_DATATYPE_SPECIAL_DATE => ' DATE ',
			RedBean_QueryWriter_CUBRID::C_DATATYPE_SPECIAL_DATETIME => ' DATETIME ',
		);
		$this->sqltype_typeno = array();
		foreach($this->typeno_sqltype as $k => $v)
		$this->sqltype_typeno[trim(($v))] = $k;
		$this->sqltype_typeno['STRING(1073741823)'] = self::C_DATATYPE_STRING;
		$this->adapter = $adapter;
	}
	/**
	 * This method returns the datatype to be used for primary key IDS and
	 * foreign keys. Returns one if the data type constants.
	 *
	 * @return integer $const data type to be used for IDS.
	 */
	public function getTypeForID() {
		return self::C_DATATYPE_INTEGER;
	}
	/**
	 * @see RedBean_QueryWriter::getTables
	 */
	public function getTables() { 
		$rows = $this->adapter->getCol("SELECT class_name FROM db_class WHERE is_system_class = 'NO';");
		return $rows;
	}
	/**
	 * @see RedBean_QueryWriter::createTable
	 */
	public function createTable($table) {
		$rawTable = $this->esc($table, true);
		$table = $this->esc($table);
		$sql = 'CREATE TABLE '.$table.' (
                   "id" integer AUTO_INCREMENT,
					CONSTRAINT "pk_'.$rawTable.'_id" PRIMARY KEY("id")
		            )';
		$this->adapter->exec($sql);
	}
	/**
	 * @see RedBean_QueryWriter::getColumns
	 */
	public function getColumns($table) {
		$columns = array();
		$table = $this->esc($table);
		$columnsRaw = $this->adapter->get("SHOW COLUMNS FROM $table");
		foreach($columnsRaw as $r) {
			$columns[$r['Field']] = $r['Type'];
		}
		return $columns;
	}
	/**
	 * @see RedBean_QueryWriter::scanType
	 */
	public function scanType($value, $flagSpecial = false) {
		$this->svalue = $value;		
		if (is_null($value)) {
			return self::C_DATATYPE_INTEGER;
		}
		if ($flagSpecial) {
			if (preg_match('/^\d{4}\-\d\d-\d\d$/', $value)) {
				return self::C_DATATYPE_SPECIAL_DATE;
			}
			if (preg_match('/^\d{4}\-\d\d-\d\d\s\d\d:\d\d:\d\d$/', $value)) {
				return self::C_DATATYPE_SPECIAL_DATETIME;
			}
		}
		$value = strval($value);
		if (!$this->startsWithZeros($value)) {
			if (is_numeric($value) && (floor($value) == $value) && $value >= -2147483647  && $value <= 2147483647 ) {
				return self::C_DATATYPE_INTEGER;
			}
			if (is_numeric($value)) {
				return self::C_DATATYPE_DOUBLE;
			}
		}
		return self::C_DATATYPE_STRING;
	}
	/**
	 * @see RedBean_QueryWriter::code
	 */
	public function code($typedescription, $includeSpecials = false) {
		$r = ((isset($this->sqltype_typeno[$typedescription])) ? $this->sqltype_typeno[$typedescription] : self::C_DATATYPE_SPECIFIED);
		if ($includeSpecials) return $r;
		if ($r > self::C_DATATYPE_SPECIFIED) return self::C_DATATYPE_SPECIFIED;
		return $r;
	}
	/**
	 * @see RedBean_QueryWriter::addColumn
	 *
	 */
	public function addColumn($type, $column, $field) {
		$table = $type;
		$type = $field;
		$table = $this->esc($table);
		$column = $this->esc($column);
		$type = array_key_exists($type, $this->typeno_sqltype) ? $this->typeno_sqltype[$type] : '';
		$sql = "ALTER TABLE $table ADD COLUMN $column $type ";
		$this->adapter->exec($sql);
	}
	/**
	 * @see RedBean_QueryWriter::widenColumn
	 */
	public function widenColumn($type, $column, $datatype) {
		$table = $type;
		$type = $datatype;
		$table = $this->esc($table);
		$column = $this->esc($column);
		$newtype = array_key_exists($type, $this->typeno_sqltype) ? $this->typeno_sqltype[$type] : '';
		$changecolumnSQL = "ALTER TABLE $table CHANGE $column $column $newtype ";
		$this->adapter->exec($changecolumnSQL);
	}
	/**
	 * @see RedBean_QueryWriter::addUniqueIndex
	 */
	public function addUniqueIndex($table, $columns) {
		$table = $this->esc($table);
		sort($columns); //else we get multiple indexes due to order-effects
		foreach($columns as $k => $v) {
			$columns[$k] = $this->esc($v);
		}
		$r = $this->adapter->get("SHOW INDEX FROM $table");
		$name = 'UQ_'.sha1(implode(',', $columns));
		if ($r) {
			foreach($r as $i) { 
				if (strtoupper($i['Key_name']) == strtoupper($name)) {
					return;
				}
			}
		}
		$sql = "ALTER TABLE $table
                ADD CONSTRAINT UNIQUE $name (".implode(',', $columns).")";
		$this->adapter->exec($sql);
	}
	/**
	 * @see RedBean_QueryWriter::sqlStateIn
	 */
	public function sqlStateIn($state, $list) {
		return ($state == 'HY000') ? (count(array_diff(array(
				RedBean_QueryWriter::C_SQLSTATE_INTEGRITY_CONSTRAINT_VIOLATION,
				RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_COLUMN,
				RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_TABLE
				), $list)) !== 3) : false;
	}
	/**
	 * @see RedBean_QueryWriter::addConstraint
	 */
	public function addConstraint(RedBean_OODBBean $bean1, RedBean_OODBBean $bean2) {
		$table1 = $bean1->getMeta('type');
		$table2 = $bean2->getMeta('type');
		$writer = $this;
		$adapter = $this->adapter;
		$table = RedBean_QueryWriter_AQueryWriter::getAssocTableFormat(array($table1, $table2));
		$property1 = $bean1->getMeta('type') . '_id';
		$property2 = $bean2->getMeta('type') . '_id';
		if ($property1 == $property2) $property2 = $bean2->getMeta('type').'2_id';
		//Dispatch to right method
		return $this->constrain($table, $table1, $table2, $property1, $property2);
	}
	/**
	 * Add the constraints for a specific database driver: CUBRID
	 * @todo Too many arguments; find a way to solve this in a neater way.
	 *
	 * @param string			  $table     table
	 * @param string			  $table1    table1
	 * @param string			  $table2    table2
	 * @param string			  $property1 property1
	 * @param string			  $property2 property2
	 *
	 * @return boolean $succes whether the constraint has been applied
	 */
	protected function constrain($table, $table1, $table2, $property1, $property2) {
		$writer = $this;
		$adapter = $this->adapter;
		$firstState = $this->buildFK($table, $table1, $property1, 'id', true);
		$secondState = $this->buildFK($table, $table2, $property2, 'id', true);
		return ($firstState && $secondState);
	}
	/**
	 * @see RedBean_QueryWriter::addIndex
	 */
	public function addIndex($type, $name, $column) {
		$table = $type;
		$table = $this->esc($table);
		$name = preg_replace('/\W/', '', $name);
		$column = $this->esc($column);
		$index = $this->adapter->getRow("SELECT 1 as `exists` FROM db_index WHERE index_name = ? ", array($name));
		if ($index && $index['exists']) return;   // positive number will return, 0 will continue.
		try{ $this->adapter->exec("CREATE INDEX $name ON $table ($column) "); } catch(Exception $e){}
	}
	/**
	 * @see RedBean_QueryWriter::addFK
	 */
	public function addFK($type, $targetType, $field, $targetField, $isDependent = false) {
		return $this->buildFK($type, $targetType, $field, $targetField, $isDependent);
	}
	/**
	 * This method adds a foreign key from type and field to
	 * target type and target field.
	 * The foreign key is created without an action. On delete/update
	 * no action will be triggered. The FK is only used to allow database
	 * tools to generate pretty diagrams and to make it easy to add actions
	 * later on.
	 * This methods accepts a type and infers the corresponding table name.
	 *
	 *
	 * @param  string $type	       type that will have a foreign key field
	 * @param  string $targetType  points to this type
	 * @param  string $field       field that contains the foreign key value
	 * @param  string $targetField field where the fk points to
	 *
	 * @return void
	 */
	protected function buildFK($type, $targetType, $field, $targetField, $isDep = false) {
		$table = $this->esc($type);
		$tableNoQ = $this->esc($type, true);
		$targetTable = $this->esc($targetType);
		$targetTableNoQ = $this->esc($targetType, true);
		$column = $this->esc($field);
		$columnNoQ = $this->esc($field, true);
		$targetColumn  = $this->esc($targetField);
		$targetColumnNoQ  = $this->esc($targetField, true);
		$keys = $this->getKeys($targetTableNoQ, $tableNoQ);
		$needsToAddFK = true;
		$needsToDropFK = false;
		foreach($keys as $key) {
			if ($key['FKTABLE_NAME'] == $tableNoQ && $key['FKCOLUMN_NAME'] == $columnNoQ) { 
				//already has an FK
				$needsToDropFK = true;
				if ((($isDep && $key['DELETE_RULE'] == 0) || (!$isDep && $key['DELETE_RULE'] == 3))) {
					return false;
				}
				break;
			}
		}	
		if ($needsToDropFK) {
			$sql = "ALTER TABLE $table DROP FOREIGN KEY {$key['FK_NAME']} ";
			$this->adapter->exec($sql);
		}
		$casc = ($isDep ? 'CASCADE' : 'SET NULL');
		$sql = "ALTER TABLE $table ADD CONSTRAINT FOREIGN KEY($column) REFERENCES $targetTable($targetColumn) ON DELETE $casc ";
		$this->adapter->exec($sql);
	}
	/**
	 * @see RedBean_QueryWriter::wipeAll
	 */
	public function wipeAll() {
		foreach($this->getTables() as $t) {
			foreach($this->getKeys($t) as $k) {
				$this->adapter->exec("ALTER TABLE \"{$k['FKTABLE_NAME']}\" DROP FOREIGN KEY \"{$k['FK_NAME']}\"");
			}
			$this->adapter->exec("DROP TABLE \"$t\"");
		}
	}
	/**
	 * Obtains the keys of a table using the PDO schema function.
	 * 
	 * @param type $table
	 * @return type 
	 */
	protected function getKeys($table, $table2 = null) {
		$pdo = $this->adapter->getDatabase()->getPDO();
		$keys = $pdo->cubrid_schema(PDO::CUBRID_SCH_EXPORTED_KEYS, $table);
		if ($table2) $keys = array_merge($keys, $pdo->cubrid_schema(PDO::CUBRID_SCH_IMPORTED_KEYS, $table2));
		return $keys;
	}
	/**
	 * @see RedBean_QueryWriter::esc
	 */
	public function esc($dbStructure, $noQuotes = false) {
		return parent::esc(strtolower($dbStructure), $noQuotes);
	}
}

class RedBean_Exception extends LogicException {}

class RedBean_Exception_SQL extends RuntimeException {
	/**
	 * @var string
	 */
	private $sqlState;
	/**
	 * Returns an ANSI-92 compliant SQL state.
	 *
	 * @return string $state ANSI state code
	 */
	public function getSQLState() {
		return $this->sqlState;
	}
	/**
	 * @todo parse state to verify valid ANSI92!
	 * Stores ANSI-92 compliant SQL state.
	 *
	 * @param string $sqlState code
	 * 
	 * @return void
	 */
	public function setSQLState($sqlState) {
		$this->sqlState = $sqlState;
	}
	/**
	 * To String prints both code and SQL state.
	 *
	 * @return string $message prints this exception instance as a string
	 */
	public function __toString() {
		return '['.$this->getSQLState().'] - '.$this->getMessage();
	}
}

class RedBean_Exception_Security extends RedBean_Exception {}

class RedBean_OODB extends RedBean_Observable {
	/**
	 * @var array 
	 */
	protected $chillList = array();
	/**
	 * @var array
	 */
	protected $dep = array();
	/**
	 * @var array
	 */
	protected $stash = NULL;
	/**
	 * @var RedBean_Adapter_DBAdapter
	 */
	protected $writer;
	/**
	 * @var boolean
	 */
	protected $isFrozen = false;
	/**
	 * @var null|\RedBean_BeanHelperFacade
	 */
	protected $beanhelper = null;
	/**
	 * @var RedBean_AssociationManager
	 */
	protected $assocManager = null;
	/**
	 * Constructor, requires a query writer.
	 *
	 * @param RedBean_QueryWriter $writer
	 */
	public function __construct(RedBean_QueryWriter $writer) {
		if ($writer instanceof RedBean_QueryWriter) $this->writer = $writer;
		$this->beanhelper = new RedBean_BeanHelper_Facade();
	}
	/**
	 * Toggles fluid or frozen mode. In fluid mode the database
	 * structure is adjusted to accomodate your objects. In frozen mode
	 * this is not the case.
	 * 
	 * You can also pass an array containing a selection of frozen types.
	 * Let's call this chilly mode, it's just like fluid mode except that
	 * certain types (i.e. tables) aren't touched.
	 * 
	 * @param boolean|array $trueFalse
	 */
	public function freeze($tf) {
		if (is_array($tf)) {
			$this->chillList = $tf;
			$this->isFrozen = false;
		} else $this->isFrozen = (boolean) $tf;
	}
	/**
	 * Returns the current mode of operation of RedBean.
	 * In fluid mode the database
	 * structure is adjusted to accomodate your objects.
	 * In frozen mode
	 * this is not the case.
	 * 
	 * @return boolean $yesNo TRUE if frozen, FALSE otherwise
	 */
	public function isFrozen() {
		return (bool) $this->isFrozen;
	}
	/**
	 * Dispenses a new bean (a RedBean_OODBBean Bean Object)
	 * of the specified type. Always
	 * use this function to get an empty bean object. Never
	 * instantiate a RedBean_OODBBean yourself because it needs
	 * to be configured before you can use it with RedBean. This
	 * function applies the appropriate initialization /
	 * configuration for you.
	 * 
	 * @param string $type   type of bean you want to dispense
	 * @param string $number number of beans you would like to get
	 * 
	 * @return RedBean_OODBBean $bean the new bean instance
	 */
	public function dispense($type, $number = 1) {
		$beans = array();
		for($i = 0; $i < $number; $i++){
			$bean = new RedBean_OODBBean;
			$bean->setBeanHelper($this->beanhelper);
			$bean->setMeta('type', $type );
			$bean->setMeta('sys.id', 'id');
			$bean->id = 0;
			if (!$this->isFrozen) $this->check($bean);
			$bean->setMeta('tainted', true);
			$bean->setMeta('sys.orig', array('id' => 0));
			$this->signal('dispense', $bean );
			$beans[] = $bean;
		}
		return (count($beans) === 1) ? array_pop($beans) : $beans; 
	}
	/**
	 * Sets bean helper to be given to beans.
	 * Bean helpers assist beans in getting a reference to a toolbox.
	 *
	 * @param RedBean_IBeanHelper $beanhelper helper
	 *
	 * @return void
	 */
	public function setBeanHelper(RedBean_BeanHelper $beanhelper) {
		$this->beanhelper = $beanhelper;
	}
	/**
	 * Checks whether a RedBean_OODBBean bean is valid.
	 * If the type is not valid or the ID is not valid it will
	 * throw an exception: RedBean_Exception_Security.
	 * @throws RedBean_Exception_Security $exception
	 * 
	 * @param RedBean_OODBBean $bean the bean that needs to be checked
	 * 
	 * @return void
	 */
	public function check(RedBean_OODBBean $bean) {
		//Is all meta information present?
		if (!isset($bean->id) ) {
			throw new RedBean_Exception_Security('Bean has incomplete Meta Information id ');
		}
		if (!is_numeric($bean->id)) {
			throw new RedBean_Exception_Security('ID must be numeric.');
		}
		if (!($bean->getMeta('type'))) {
			throw new RedBean_Exception_Security('Bean has incomplete Meta Information II');
		}
		//Pattern of allowed characters
		$pattern = '/[^a-z0-9_]/i';
		//Does the type contain invalid characters?
		if (preg_match($pattern, $bean->getMeta('type'))) {
			throw new RedBean_Exception_Security('Bean Type is invalid');
		}
		//Are the properties and values valid?
		foreach($bean as $prop => $value) {
			if (
					  is_array($value) ||
					  (is_object($value)) ||
					  strlen($prop) < 1 ||
					  preg_match($pattern, $prop)
			) {
				throw new RedBean_Exception_Security("Invalid Bean: property $prop  ");
			}
		}
	}
	/**
	 * Searches the database for a bean that matches conditions $conditions and sql $addSQL
	 * and returns an array containing all the beans that have been found.
	 * 
	 * Conditions need to take form:
	 * 
	 * array(
	 * 		'PROPERTY' => array( POSSIBLE VALUES... 'John', 'Steve' )
	 * 		'PROPERTY' => array( POSSIBLE VALUES... )
	 * );
	 * 
	 * All conditions are glued together using the AND-operator, while all value lists
	 * are glued using IN-operators thus acting as OR-conditions.
	 * 
	 * Note that you can use property names; the columns will be extracted using the
	 * appropriate bean formatter.
	 * 
	 * @throws RedBean_Exception_SQL 
	 * 
	 * @param string  $type       type of beans you are looking for
	 * @param array   $conditions list of conditions
	 * @param string  $addSQL	  SQL to be used in query
	 * @param boolean $all        whether you prefer to use a WHERE clause or not (TRUE = not)
	 * 
	 * @return array $beans		  resulting beans
	 */
	public function find($type, $conditions = array(), $addSQL = null, $all = false) {
		try {
			$beans = $this->convertToBeans($type, $this->writer->selectRecord($type, $conditions, $addSQL, false, false, $all));
			return $beans;
		} catch(RedBean_Exception_SQL $e) {
			if (!$this->writer->sqlStateIn($e->getSQLState(),
			array(
				RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_TABLE,
				RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_COLUMN)
			)) throw $e;
		}
		return array();
	}
	/**
	 * Checks whether the specified table already exists in the database.
	 * Not part of the Object Database interface!
	 * 
	 * @param string $table table name (not type!)
	 * 
	 * @return boolean $exists whether the given table exists within this database.
	 */
	public function tableExists($table) {
		$tables = $this->writer->getTables();
		return in_array(($table), $tables);
	}
	/**
	 * Processes all column based build commands.
	 * A build command is an additional instruction for the Query Writer. It is processed only when
	 * a column gets created. The build command is often used to instruct the writer to write some
	 * extra SQL to create indexes or constraints. Build commands are stored in meta data of the bean.
	 * They are only for internal use, try to refrain from using them in your code directly.
	 *
	 * @param  string           $table    name of the table to process build commands for
	 * @param  string           $property name of the property to process build commands for
	 * @param  RedBean_OODBBean $bean     bean that contains the build commands
	 *
	 * @return void
	 */
	protected function processBuildCommands($table, $property, RedBean_OODBBean $bean) {
		if ($inx = ($bean->getMeta('buildcommand.indexes'))) {
			if (isset($inx[$property])) $this->writer->addIndex($table, $inx[$property], $property);
		}
	}
	/**
	 * Process groups. Internal function. Processes different kind of groups for
	 * storage function. Given a list of original beans and a list of current beans,
	 * this function calculates which beans remain in the list (residue), which
	 * have been deleted (are in the trashcan) and which beans have been added
	 * (additions). 
	 *
	 * @param  array $originals originals
	 * @param  array $current   the current beans
	 * @param  array $additions beans that have been added
	 * @param  array $trashcan  beans that have been deleted
	 * @param  array $residue   beans that have been left untouched
	 *
	 * @return array $result 	new relational state
	 */
	private function processGroups($originals, $current, $additions, $trashcan, $residue) {
		return array(
			array_merge($additions, array_diff($current, $originals)),
			array_merge($trashcan, array_diff($originals, $current)),
			array_merge($residue, array_intersect($current, $originals))
		);
	}
	/**
	 * Figures out the desired type given the cast string ID.
	 * 
	 * @param string $cast cast identifier
	 * 
	 * @return integer $typeno 
	 */
	private function getTypeFromCast($cast) {
		if ($cast == 'string') {
			$typeno = $this->writer->scanType('STRING');
		} elseif ($cast == 'id') {
			$typeno = $this->writer->getTypeForID();
		} elseif(isset($this->writer->sqltype_typeno[$cast])) {
			$typeno = $this->writer->sqltype_typeno[$cast];
		} else {
			throw new RedBean_Exception('Invalid Cast');
		}
		return $typeno;
	}
	/**
	 * Processes an embedded bean. First the bean gets unboxed if possible.
	 * Then, the bean is stored if needed and finally the ID of the bean
	 * will be returned.
	 * 
	 * @param RedBean_OODBBean|Model $v the bean or model
	 * 
	 * @return  integer $id
	 */
	private function prepareEmbeddedBean($v) {
		if (!$v->id || $v->getMeta('tainted')) {
			$this->store($v);
		}
		return $v->id;
	}
	/**
	 * Stores a bean in the database. This function takes a
	 * RedBean_OODBBean Bean Object $bean and stores it
	 * in the database. If the database schema is not compatible
	 * with this bean and RedBean runs in fluid mode the schema
	 * will be altered to store the bean correctly.
	 * If the database schema is not compatible with this bean and
	 * RedBean runs in frozen mode it will throw an exception.
	 * This function returns the primary key ID of the inserted
	 * bean.
	 *
	 * @throws RedBean_Exception_Security $exception
	 * 
	 * @param RedBean_OODBBean | RedBean_SimpleModel $bean bean to store
	 *
	 * @return integer $newid resulting ID of the new bean
	 */
	public function store($bean) { 
		if ($bean instanceof RedBean_SimpleModel) $bean = $bean->unbox();
		if (!($bean instanceof RedBean_OODBBean)) throw new RedBean_Exception_Security('OODB Store requires a bean, got: '.gettype($bean));
		$processLists = false;
		foreach($bean as $k => $v) if (is_array($v) || is_object($v)) { $processLists = true; break; }
		if (!$processLists && !$bean->getMeta('tainted')) return $bean->getID();
		$this->signal('update', $bean );
		foreach($bean as $k => $v) if (is_array($v) || is_object($v)) { $processLists = true; break; }
		if ($processLists) {
			//Define groups
			$sharedAdditions = $sharedTrashcan = $sharedresidue = $sharedItems = array();
			$ownAdditions = $ownTrashcan = $ownresidue = array();
			$tmpCollectionStore = array();
			$embeddedBeans = array();
			foreach($bean as $p => $v) {
				if ($v instanceof RedBean_SimpleModel) $v = $v->unbox(); 
				if ($v instanceof RedBean_OODBBean) {
					$linkField = $p.'_id';
					$bean->$linkField = $this->prepareEmbeddedBean($v);
					$bean->setMeta('cast.'.$linkField, 'id');
					$embeddedBeans[$linkField] = $v;
					$tmpCollectionStore[$p] = $bean->$p;
					$bean->removeProperty($p);
				}
				if (is_array($v)) {
					$originals = $bean->getMeta('sys.shadow.'.$p);
					if (!$originals) $originals = array();
					if (strpos($p, 'own') === 0) {
						list($ownAdditions, $ownTrashcan, $ownresidue) = $this->processGroups($originals, $v, $ownAdditions, $ownTrashcan, $ownresidue);
						$bean->removeProperty($p);
					} elseif (strpos($p, 'shared') === 0) {
						list($sharedAdditions, $sharedTrashcan, $sharedresidue) = $this->processGroups($originals, $v, $sharedAdditions, $sharedTrashcan, $sharedresidue);
						$bean->removeProperty($p);
					} else {}
				}
			}
		}
		$this->storeBean($bean);
		if ($processLists) {
			$this->processEmbeddedBeans($bean, $embeddedBeans);
			$myFieldLink = $bean->getMeta('type').'_id';
			$this->processTrashcan($bean, $ownTrashcan);
			$this->processAdditions($bean, $ownAdditions);
			$this->processResidue($ownresidue);
			foreach($sharedTrashcan as $trash) $this->assocManager->unassociate($trash, $bean);
			$this->processSharedAdditions($bean, $sharedAdditions);
			foreach($sharedresidue as $residue) $this->store($residue);
		}
		$this->signal('after_update', $bean);
		return (int) $bean->id;
	}
	/**
	 * Stores a cleaned bean; i.e. only scalar values. This is the core of the store()
	 * method. When all lists and embedded beans (parent objects) have been processed and
	 * removed from the original bean the bean is passed to this method to be stored
	 * in the database.
	 * 
	 * @param RedBean_OODBBean $bean the clean bean 
	 */
	private function storeBean(RedBean_OODBBean $bean) {
		if (!$this->isFrozen) $this->check($bean);
		//what table does it want
		$table = $bean->getMeta('type');
		if ($bean->getMeta('tainted')) {
			//Does table exist? If not, create
			if (!$this->isFrozen && !$this->tableExists($this->writer->esc($table, true))) {
				$this->writer->createTable($table);
				$bean->setMeta('buildreport.flags.created', true);
			}
			if (!$this->isFrozen) {
				$columns = $this->writer->getColumns($table) ;
			}
			//does the table fit?
			$insertvalues = array();
			$insertcolumns = array();
			$updatevalues = array();
			foreach($bean as $p => $v) {
				$origV = $v;
				if ($p !== 'id') {
					if (!$this->isFrozen) {
						//Not in the chill list?
						if (!in_array($bean->getMeta('type'), $this->chillList)) {
							//Does the user want to specify the type?
							if ($bean->getMeta("cast.$p", -1) !== -1) {
								$cast = $bean->getMeta("cast.$p");
								$typeno = $this->getTypeFromCast($cast);
							} else {
								$cast = false;		
								//What kind of property are we dealing with?
								$typeno = $this->writer->scanType($v, true);
							}
							//Is this property represented in the table?
							if (isset($columns[$this->writer->esc($p, true)])) {
								//rescan
								$v = $origV;
								if (!$cast) $typeno = $this->writer->scanType($v, false);
								//yes it is, does it still fit?
								$sqlt = $this->writer->code($columns[$this->writer->esc($p, true)]);
								if ($typeno > $sqlt) {
									//no, we have to widen the database column type
									$this->writer->widenColumn($table, $p, $typeno);
									$bean->setMeta('buildreport.flags.widen', true);
								}
							} else {
								//no it is not
								$this->writer->addColumn($table, $p, $typeno);
								$bean->setMeta('buildreport.flags.addcolumn', true);
								//@todo: move build commands here... more practical
								$this->processBuildCommands($table, $p, $bean);
							}
						}
					}
					//Okay, now we are sure that the property value will fit
					$insertvalues[] = $v;
					$insertcolumns[] = $p;
					$updatevalues[] = array('property' => $p, 'value' => $v);
				}
			}
			if (!$this->isFrozen && ($uniques = $bean->getMeta('buildcommand.unique'))) {
				foreach($uniques as $unique) $this->writer->addUniqueIndex($table, $unique);
			}
			$rs = $this->writer->updateRecord($table, $updatevalues, $bean->id);
			$bean->id = $rs;
			$bean->setMeta('tainted', false);
		}
	}
	/**
	 * Processes a list of beans from a bean. A bean may contain lists. This
	 * method handles shared addition lists; i.e. the $bean->sharedObject properties.
	 * 
	 * @param RedBean_OODBBean $bean             the bean
	 * @param array            $sharedAdditions  list with shared additions
	 */
	private function processSharedAdditions($bean, $sharedAdditions) {
		foreach($sharedAdditions as $addition) {
			if ($addition instanceof RedBean_OODBBean) {
				$this->assocManager->associate($addition, $bean);
			} else {
				throw new RedBean_Exception_Security('Array may only contain RedBean_OODBBeans');
			}
		}
	}
	/**
	 * Processes a list of beans from a bean. A bean may contain lists. This
	 * method handles own lists; i.e. the $bean->ownObject properties.
	 * A residue is a bean in an own-list that stays where it is. This method
	 * checks if there have been any modification to this bean, in that case
	 * the bean is stored once again, otherwise the bean will be left untouched.
	 *  
	 * @param RedBean_OODBBean $bean       the bean
	 * @param array            $ownresidue list 
	 */
	private function processResidue($ownresidue) {
		foreach($ownresidue as $residue) {
			if ($residue->getMeta('tainted')) {
				$this->store($residue);
			}
		}
	}
	/**
	 * Processes a list of beans from a bean. A bean may contain lists. This
	 * method handles own lists; i.e. the $bean->ownObject properties.
	 * A trash can bean is a bean in an own-list that has been removed 
	 * (when checked with the shadow). This method
	 * checks if the bean is also in the dependency list. If it is the bean will be removed.
	 * If not, the connection between the bean and the owner bean will be broken by
	 * setting the ID to NULL.
	 *  
	 * @param RedBean_OODBBean $bean        the bean
	 * @param array            $ownTrashcan list 
	 */
	private function processTrashcan($bean, $ownTrashcan) {
		$myFieldLink = $bean->getMeta('type').'_id';
		if (is_array($ownTrashcan) && count($ownTrashcan)>0) {
			$first = reset($ownTrashcan);
			if ($first instanceof RedBean_OODBBean) {
				$alias = $bean->getMeta('sys.alias.'.$first->getMeta('type'));
				if ($alias) $myFieldLink = $alias.'_id';
			}
		}
		foreach($ownTrashcan as $trash) {
			if (isset($this->dep[$trash->getMeta('type')]) && in_array($bean->getMeta('type'), $this->dep[$trash->getMeta('type')])) {
				$this->trash($trash);
			} else {
				$trash->$myFieldLink = null;
				$this->store($trash);
			}
		}
	}
	/**
	 * Processes embedded beans.
	 * Each embedded bean will be indexed and foreign keys will
	 * be created if the bean is in the dependency list.
	 * 
	 * @param RedBean_OODBBean $bean          bean
	 * @param array            $embeddedBeans embedded beans
	 */
	private function processEmbeddedBeans($bean, $embeddedBeans) {
		foreach($embeddedBeans as $linkField => $embeddedBean) {
			if (!$this->isFrozen) {
				$this->writer->addIndex($bean->getMeta('type'),
							'index_foreignkey_'.$bean->getMeta('type').'_'.$embeddedBean->getMeta('type'),
							 $linkField);
				$isDep = $this->isDependentOn($bean->getMeta('type'), $embeddedBean->getMeta('type'));
				$this->writer->addFK($bean->getMeta('type'), $embeddedBean->getMeta('type'), $linkField, 'id', $isDep);
			}
		}	
	}
	/**
	 * Part of the store() functionality.
	 * Handles all new additions after the bean has been saved.
	 * Stores addition bean in own-list, extracts the id and
	 * adds a foreign key. Also adds a constraint in case the type is
	 * in the dependent list.
	 * 
	 * @param RedBean_OODBBean $bean         bean
	 * @param array            $ownAdditions list of addition beans in own-list 
	 */
	private function processAdditions($bean, $ownAdditions) {
		$myFieldLink = $bean->getMeta('type').'_id';
		if ($bean && count($ownAdditions)>0) {
			$first = reset($ownAdditions);
			if ($first instanceof RedBean_OODBBean) {
				$alias = $bean->getMeta('sys.alias.'.$first->getMeta('type'));
				if ($alias) $myFieldLink = $alias.'_id';
			}
		}
		foreach($ownAdditions as $addition) {
			if ($addition instanceof RedBean_OODBBean) {  
				$addition->$myFieldLink = $bean->id;
				$addition->setMeta('cast.'.$myFieldLink, 'id');
				$this->store($addition);
				if (!$this->isFrozen) {
					$this->writer->addIndex($addition->getMeta('type'),
						'index_foreignkey_'.$addition->getMeta('type').'_'.$bean->getMeta('type'),
						 $myFieldLink);
					$isDep = $this->isDependentOn($addition->getMeta('type'), $bean->getMeta('type'));
					$this->writer->addFK($addition->getMeta('type'), $bean->getMeta('type'), $myFieldLink, 'id', $isDep);
				}
			} else {
				throw new RedBean_Exception_Security('Array may only contain RedBean_OODBBeans');
			}
		}
	}
	/**
	 * Checks whether reference type has been marked as dependent on target type.
	 * This is the result of setting reference type as a key in R::dependencies() and
	 * putting target type in its array. 
	 * 
	 * @param string $refType   reference type
	 * @param string $otherType other type / target type
	 * 
	 * @return boolean 
	 */
	protected function isDependentOn($refType, $otherType) {
		return (boolean) (isset($this->dep[$refType]) && in_array($otherType, $this->dep[$refType]));
	}
	/**
	 * Loads a bean from the object database.
	 * It searches for a RedBean_OODBBean Bean Object in the
	 * database. It does not matter how this bean has been stored.
	 * RedBean uses the primary key ID $id and the string $type
	 * to find the bean. The $type specifies what kind of bean you
	 * are looking for; this is the same type as used with the
	 * dispense() function. If RedBean finds the bean it will return
	 * the RedBean_OODB Bean object; if it cannot find the bean
	 * RedBean will return a new bean of type $type and with
	 * primary key ID 0. In the latter case it acts basically the
	 * same as dispense().
	 * 
	 * Important note:
	 * If the bean cannot be found in the database a new bean of
	 * the specified type will be generated and returned.
	 * 
	 * @param string  $type type of bean you want to load
	 * @param integer $id   ID of the bean you want to load
	 * 
	 * @return RedBean_OODBBean $bean loaded bean
	 */
	public function load($type, $id) {
		$bean = $this->dispense($type);
		if ($this->stash && isset($this->stash[$id])) {
			$row = $this->stash[$id];
		} else {
			try {
				$rows = $this->writer->selectRecord($type, array('id' => array($id)));
			} catch(RedBean_Exception_SQL $e ) {
				if (
				$this->writer->sqlStateIn($e->getSQLState(),
				array(
					RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_COLUMN,
					RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_TABLE)
				)
				) {
					$rows = 0;
					if ($this->isFrozen) throw $e; //only throw if frozen;
				}
			}
			if (empty($rows)) return $bean;
			$row = array_pop($rows);
		}
		$bean->setMeta('sys.orig', $row);
		foreach($row as $p => $v) {
			$bean->$p = $v;
		}
		$this->signal('open', $bean);
		$bean->setMeta('tainted', false);
		return $bean;
	}
	/**
	 * Removes a bean from the database.
	 * This function will remove the specified RedBean_OODBBean
	 * Bean Object from the database.
	 * 
	 * @throws RedBean_Exception_Security $exception
	 * 
	 * @param RedBean_OODBBean|RedBean_SimpleModel $bean bean you want to remove from database
	 */
	public function trash($bean) {
		if ($bean instanceof RedBean_SimpleModel) $bean = $bean->unbox();
		if (!($bean instanceof RedBean_OODBBean)) throw new RedBean_Exception_Security('OODB Store requires a bean, got: '.gettype($bean));
		$this->signal('delete', $bean);
		foreach($bean as $p => $v) {
			if ($v instanceof RedBean_OODBBean) {
				$bean->removeProperty($p);
			}
			if (is_array($v)) {
				if (strpos($p, 'own') === 0) {
					$bean->removeProperty($p);
				} elseif (strpos($p, 'shared') === 0) {
					$bean->removeProperty($p);
				}
			}
		}
		if (!$this->isFrozen) $this->check($bean);
		try {
			$this->writer->selectRecord($bean->getMeta('type'),
				array('id' => array($bean->id)), null, true );
		}catch(RedBean_Exception_SQL $e) {
			if (!$this->writer->sqlStateIn($e->getSQLState(),
			array(
			RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_COLUMN,
			RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_TABLE)
			)) throw $e;
		}
		$bean->id = 0;
		$this->signal('after_delete', $bean);
	}
	/**
	 * Returns an array of beans. Pass a type and a series of ids and
	 * this method will bring you the correspondig beans.
	 * 
	 * important note: Because this method loads beans using the load()
	 * function (but faster) it will return empty beans with ID 0 for 
	 * every bean that could not be located. The resulting beans will have the
	 * passed IDs as their keys.
	 *
	 * @param string $type type of beans 
	 * @param array  $ids  ids to load
	 *
	 * @return array $beans resulting beans (may include empty ones)
	 */
	public function batch($type, $ids) {
		if (!$ids) return array();
		$collection = array();
		try {
			$rows = $this->writer->selectRecord($type, array('id' => $ids));
		} catch(RedBean_Exception_SQL $e) {
			if (!$this->writer->sqlStateIn($e->getSQLState(),
			array(
			RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_COLUMN,
			RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_TABLE)
			)) throw $e;
			$rows = false;
		}
		$this->stash = array();
		if (!$rows) return array();
		foreach($rows as $row) {
			$this->stash[$row['id']] = $row;
		}
		foreach($ids as $id) {
			$collection[$id] = $this->load($type, $id);
		}
		$this->stash = NULL;
		return $collection;
	}
	/**
	 * This is a convenience method; it converts database rows
	 * (arrays) into beans. Given a type and a set of rows this method
	 * will return an array of beans of the specified type loaded with
	 * the data fields provided by the result set from the database.
	 * 
	 * @param string $type type of beans you would like to have
	 * @param array  $rows rows from the database result
	 * 
	 * @return array $collectionOfBeans collection of beans
	 */
	public function convertToBeans($type, $rows) {
		$collection = array();
		$this->stash = array();
		foreach($rows as $row) {
			$id = $row['id'];
			$this->stash[$id] = $row;
			$collection[$id] = $this->load($type, $id);
		}
		$this->stash = NULL;
		return $collection;
	}
	/**
	 * Returns the number of beans we have in DB of a given type.
	 *
	 * @param string $type   type of bean we are looking for
	 * @param string $addSQL additional SQL snippet
	 * @param array  $params parameters to bind to SQL
	 *
	 * @return integer $num number of beans found
	 */
	public function count($type, $addSQL = '', $params = array()) {
		try {
			return (int) $this->writer->count($type, $addSQL, $params);
		} catch(RedBean_Exception_SQL $e) {
			if (!$this->writer->sqlStateIn($e->getSQLState(),array(RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_TABLE))) throw $e;
		}
		return 0;
	}
	/**
	 * Trash all beans of a given type.
	 *
	 * @param string $type type
	 *
	 * @return boolean $yesNo whether we actually did some work or not..
	 */
	public function wipe($type) {
		try {
			$this->writer->wipe($type);
			return true;
		} catch(RedBean_Exception_SQL $e) {
			if (!$this->writer->sqlStateIn($e->getSQLState(),
			array(RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_TABLE)
			)) throw $e;
			return false;
		}
	}
	/**
	 * Returns an Association Manager for use with OODB.
	 * A simple getter function to obtain a reference to the association manager used for
	 * storage and more.
	 *
	 * @throws Exception
	 * @return RedBean_AssociationManager $assoc Association Manager
	 */
	public function getAssociationManager() {
		if (!isset($this->assocManager)) throw new Exception('No association manager available.');
		return $this->assocManager;
	}
	/**
	 * Sets the association manager instance to be used by this OODB.
	 * A simple setter function to set the association manager to be used for storage and
	 * more.
	 * 
	 * @param RedBean_AssociationManager $assoc sets the association manager to be used
	 * 
	 * @return void
	 */
	public function setAssociationManager(RedBean_AssociationManager $assoc) {
		$this->assocManager = $assoc;
	}
	/**
	 * Sets a dependency list. Dependencies can be used to make
	 * certain beans depend on others. This causes dependent beans to get removed
	 * once the bean they depend on has been removed as well.
	 * A dependency takes the form:
	 * 
	 * $me => depends on array( $bean1, $bean2 )
	 * 
	 * For instance a to inform RedBeanPHP about the fact that a page
	 * depends on a book:
	 * 
	 * 'page' => array('book')
	 * 
	 * A bean can depend on multiple other beans.
	 * 
	 * A dependency does two things:
	 * 
	 * 1. Adds a ON CASCADE DELETE 
	 * 2. trashes the depending bean if the entry in the ownList is removed 
	 * 
	 * @param array $dep 
	 */
	public function setDepList($dep) {
		$this->dep = $dep;
	}
	/**
	 * Preloads certain properties for beans.
	 * Understands aliases.
	 * 
	 * Usage: $redbean->preload($books, array('coauthor'=>'author'));
	 * 
	 * Usage for nested beans:
	 * 
	 * $redbean->preload($texts, array('page', 'page.book', 'page.book.author'));
	 * 
	 * preloads pages, books and authors.
	 * You may also use a shortcut here: 
	 * 
	 * $redbean->preload($texts, array('page', '*.book', '*.author'));
	 * 
	 * Can also load preload lists:
	 * 
	 * $redbean->preload($books, array('ownPage'=>'page', '*.ownText'=>'text', 'sharedTag'=>'tag'));
	 * 
	 * @param array $beans beans
	 * @param array $types types to load
	 */
	public function preload($beans, $types, $closure = null) {
		if (is_string($types)) $types = explode(',', $types);
		$oldFields = array(); $i = 0; $retrievals = array(); $oldField = '';
		foreach($types as $key => $typeInfo) {
			list($type,$sqlObj) = (is_array($typeInfo) ? $typeInfo : array($typeInfo, null));
			$map = $ids = $retrievals[$i] = array();
			$field = (is_numeric($key)) ? $type : $key;//use an alias?
			if (strpos($field, '*') !== false) { 
				$oldFields[]= $oldField; 
				$field = str_replace('*', implode('.', $oldFields), $field);
			}
			if (strpos($field, '&') !== false) $field = str_replace('&', implode('.', $oldFields), $field);
			$filteredBeans = $beans;
			$counterID = 0;
			foreach($filteredBeans as $bean) {
				$bean->setMeta('sys.input-bean-id', array($counterID => $counterID));
				$counterID++;
			}
			while($p = strpos($field, '.')) { //filtering: find the right beans in the path
				$nesting = substr($field, 0, $p);
				$filtered = array();
				foreach($filteredBeans as $bean) {
					$inputBeanID = $bean->getMeta('sys.input-bean-id');
					if (is_array($bean->$nesting)) {
						$nestedBeans = $bean->$nesting;
						foreach($nestedBeans as $nestedBean) {
							$currentInputBeanIDs = $nestedBean->getMeta('sys.input-bean-id'); 
							if (!is_array($currentInputBeanIDs)) $currentInputBeanIDs = array();
							$addInputIDs = $bean->getMeta('sys.input-bean-id');
							foreach($addInputIDs as $addInputID) $currentInputBeanIDs[$addInputID] = $addInputID;
							$nestedBean->setMeta('sys.input-bean-id', $currentInputBeanIDs);
						}
						$filtered = array_merge($filtered, $nestedBeans);
					} elseif (!is_null($bean->$nesting)) {
						$nestedBean = $bean->$nesting;
						$currentInputBeanIDs = $nestedBean->getMeta('sys.input-bean-id'); 
						if (!is_array($currentInputBeanIDs)) $currentInputBeanIDs = array();
						$addInputIDs = $bean->getMeta('sys.input-bean-id');
						foreach($addInputIDs as $addInputID) $currentInputBeanIDs[$addInputID] = $addInputID;
						$nestedBean->setMeta('sys.input-bean-id', $currentInputBeanIDs);	
						$filtered[] = $bean->$nesting;
					}
				}
				$filteredBeans = $filtered;
				$field = substr($field, $p+1);
			}
			$oldField = $field;
			if (strpos($type, '.')) $type = $field;
			if (strpos($field, 'shared') !== 0) {
				foreach($filteredBeans as $bean) { //gather ids to load the desired bean collections
					if (strpos($field, 'own') === 0) { //based on bean->id for ownlist
						$id = $bean->id; $ids[$id] = $id;
					} elseif($id = $bean->{$field.'_id'}){ //based on bean_id for parent
						$ids[$id] = $id; 
						if (!isset($map[$id])) $map[$id] = array();
						$map[$id][] = $bean;
					}
				}
			}
			if (strpos($field, 'shared') === 0) {
				$bean = reset($filteredBeans);
				$link = $bean->getMeta('type').'_id';
				$keys = $this->assocManager->related($filteredBeans, $type, true);
				$linkTable = $this->assocManager->getTable(array($type, $bean->getMeta('type')));
				$linkBeans = $this->batch($linkTable, $keys);
				$linked = $targetIDs = array();
				$targetIDField = $type.'_id';
				foreach($linkBeans as $linkBean) {
					$linkID = $linkBean->$link;
					if (!isset($linked[$linkID])) $linked[$linkID] = array();
					$linked[$linkID][] = $linkBean;
					$targetIDs[$linkBean->$targetIDField] = $linkBean->$targetIDField;
				}
				$sharedBeans = $this->find($type, array('id' => $targetIDs), $sqlObj, (!is_null($sqlObj)));
				foreach($filteredBeans as $filteredBean) {
					$list = array();
					if (isset($linked[$filteredBean->id])) {
						foreach($linked[$filteredBean->id] as $linkBean) {
							foreach($sharedBeans as $sharedBean) {
								if ($sharedBean->id == $linkBean->$targetIDField) $list[$sharedBean->id] = $sharedBean; 
							}
						}
					}
					$filteredBean->setProperty($field, $list);
					$inputBeanIDs = $filteredBean->getMeta('sys.input-bean-id');
					foreach($inputBeanIDs as $inputBeanID) {
						if (!isset($retrievals[$i][$inputBeanID])) $retrievals[$i][$inputBeanID] = array();
						foreach($list as $listKey => $listBean) $retrievals[$i][$inputBeanID][$listKey] = $listBean;
					}
				}
			} elseif (strpos($field, 'own') === 0) {//preload for own-list using find
				$link = $bean->getMeta('type').'_id';
				$children = $this->find($type, array($link => $ids), $sqlObj, (!is_null($sqlObj)));
				foreach($filteredBeans as $filteredBean) {
					$list = array();
					foreach($children as $child) if ($child->$link == $filteredBean->id) $list[$child->id] = $child;
					$filteredBean->setProperty($field, $list);
					$inputBeanIDs = $filteredBean->getMeta('sys.input-bean-id');
					foreach($inputBeanIDs as $inputBeanID) {
						if (!isset($retrievals[$i][$inputBeanID])) $retrievals[$i][$inputBeanID] = array();
						foreach($list as $listKey => $listBean) $retrievals[$i][$inputBeanID][$listKey] = $listBean;
					}
				}
			} else { //preload for parent objects using batch()
				foreach($this->batch($type, $ids) as $parent) {
					foreach($map[$parent->id] as $childBean) {
						$childBean->setProperty($field, $parent);
						$inputBeanIDs = $childBean->getMeta('sys.input-bean-id');
						foreach($inputBeanIDs as $inputBeanID) $retrievals[$i][$inputBeanID] = $parent;
					}
				}
			}
			$i++;
		}
		if ($closure) {
			$key = 0; 
			foreach($beans as $bean) {
				$params = array();
				foreach($retrievals as $r) $params[] = (isset($r[$key])) ? $r[$key] : null; 
				array_unshift($params, $bean);
				call_user_func_array($closure, $params);
				$key ++;
			}
		}
	}
}

class RedBean_ToolBox {
	/**
	 * @var RedBean_OODB
	 */
	protected $oodb;
	/**
	 * @var RedBean_QueryWriter
	 */
	protected $writer;
	/**
	 * @var RedBean_Adapter_DBAdapter
	 */
	protected $adapter;
	/**
	 * Constructor.
	 *
	 * @param RedBean_OODB              $oodb    Object Database
	 * @param RedBean_Adapter_DBAdapter $adapter Adapter
	 * @param RedBean_QueryWriter       $writer  Writer
	 *
	 * return RedBean_ToolBox $toolbox Toolbox
	 */
	public function __construct(RedBean_OODB $oodb, RedBean_Adapter $adapter, RedBean_QueryWriter $writer) {
		$this->oodb = $oodb;
		$this->adapter = $adapter;
		$this->writer = $writer;
		return $this;
	}
	/**
	 * Returns the query writer in this toolbox.
	 * 
	 * @return RedBean_QueryWriter $writer writer
	 */
	public function getWriter() {
		return $this->writer;
	}
	/**
	 * Returns the OODB instance in this toolbox.
	 * 
	 * @return RedBean_OODB $oodb Object Database
	 */
	public function getRedBean() {
		return $this->oodb;
	}
	/**
	 * Returns the database adapter in this toolbox.
	 * 
	 * @return RedBean_Adapter_DBAdapter $adapter Adapter
	 */
	public function getDatabaseAdapter() {
		return $this->adapter;
	}
}

class RedBean_AssociationManager extends RedBean_Observable {
	/**
	 * @var RedBean_OODB
	 */
	protected $oodb;
	/**
	 * @var RedBean_Adapter_DBAdapter
	 */
	protected $adapter;
	/**
	 * @var RedBean_QueryWriter
	 */
	protected $writer;
	/**
	 * Constructor
	 *
	 * @param RedBean_ToolBox $tools toolbox
	 */
	public function __construct(RedBean_ToolBox $tools) {
		$this->oodb = $tools->getRedBean();
		$this->adapter = $tools->getDatabaseAdapter();
		$this->writer = $tools->getWriter();
		$this->toolbox = $tools;
	}
	/**
	 * Creates a table name based on a types array.
	 * Manages the get the correct name for the linking table for the
	 * types provided.
	 *
	 * @todo find a nice way to decouple this class from QueryWriter?
	 * 
	 * @param array $types 2 types as strings
	 *
	 * @return string $table table
	 */
	public function getTable($types) {
		return RedBean_QueryWriter_AQueryWriter::getAssocTableFormat($types);
	}
	/**
	 * Associates two beans with eachother using a many-to-many relation.
	 *
	 * @param RedBean_OODBBean $bean1 bean1
	 * @param RedBean_OODBBean $bean2 bean2
	 */
	public function associate($beans1, $beans2) {
		$results = array();
		if (!is_array($beans1)) $beans1 = array($beans1);
		if (!is_array($beans2)) $beans2 = array($beans2);
		foreach($beans1 as $bean1) {
			foreach($beans2 as $bean2) {
				$table = $this->getTable(array($bean1->getMeta('type') , $bean2->getMeta('type')));
				$bean = $this->oodb->dispense($table);
				$results[] = $this->associateBeans($bean1, $bean2, $bean);
			}
		}
		return (count($results)>1) ? $results : reset($results);
	}
	/**
	 * Associates a pair of beans. This method associates two beans, no matter
	 * what types.Accepts a base bean that contains data for the linking record.
	 *
	 * @param RedBean_OODBBean $bean1 first bean
	 * @param RedBean_OODBBean $bean2 second bean
	 * @param RedBean_OODBBean $bean  base bean
	 *
	 * @return mixed $id either the link ID or null
	 */
	protected function associateBeans(RedBean_OODBBean $bean1, RedBean_OODBBean $bean2, RedBean_OODBBean $bean) {
		$property1 = $bean1->getMeta('type') . '_id';
		$property2 = $bean2->getMeta('type') . '_id';
		if ($property1 == $property2) $property2 = $bean2->getMeta('type').'2_id';
		//add a build command for Unique Indexes
		$bean->setMeta('buildcommand.unique' , array(array($property1, $property2)));
		//add a build command for Single Column Index (to improve performance in case unqiue cant be used)
		$indexName1 = 'index_for_'.$bean->getMeta('type').'_'.$property1;
		$indexName2 = 'index_for_'.$bean->getMeta('type').'_'.$property2;
		$bean->setMeta('buildcommand.indexes', array($property1 => $indexName1, $property2 => $indexName2));
		$this->oodb->store($bean1);
		$this->oodb->store($bean2);
		$bean->setMeta("cast.$property1", "id");
		$bean->setMeta("cast.$property2", "id");
		$bean->$property1 = $bean1->id;
		$bean->$property2 = $bean2->id;
		try {
			$id = $this->oodb->store($bean);
			//On creation, add constraints....
			if (!$this->oodb->isFrozen() &&
				$bean->getMeta('buildreport.flags.created')){
				$bean->setMeta('buildreport.flags.created', 0);
				if (!$this->oodb->isFrozen())
				$this->writer->addConstraint($bean1, $bean2);
			}
			$results[] = $id;
		} catch(RedBean_Exception_SQL $e) {
			if (!$this->writer->sqlStateIn($e->getSQLState(),
			array(RedBean_QueryWriter::C_SQLSTATE_INTEGRITY_CONSTRAINT_VIOLATION))) throw $e;
		}
	}
	/**
	 * Returns all ids of beans of type $type that are related to $bean. If the
	 * $getLinks parameter is set to boolean TRUE this method will return the ids
	 * of the association beans instead. You can also add additional SQL. This SQL
	 * will be appended to the original query string used by this method. Note that this
	 * method will not return beans, just keys. For a more convenient method see the R-facade
	 * method related(), that is in fact a wrapper for this method that offers a more
	 * convenient solution. If you want to make use of this method, consider the
	 * OODB batch() method to convert the ids to beans.
	 * 
	 * Since 3.2, you can now also pass an array of beans instead just one
	 * bean as the first parameter.
	 *
	 * @throws RedBean_Exception_SQL
	 *
	 * @param RedBean_OODBBean|array $bean     reference bean
	 * @param string				 $type     target type
	 * @param bool					 $getLinks whether you are interested in the assoc records
	 * @param bool					 $sql      room for additional SQL
	 *
	 * @return array $ids
	 */
	public function related($bean, $type, $getLinks = false, $sql = false) {
		if (!is_array($bean) && !($bean instanceof RedBean_OODBBean)) throw new RedBean_Exception_Security('Expected array or RedBean_OODBBean but got:'.gettype($bean));
		$ids = array();
		if (is_array($bean)) {
			$beans = $bean;
			foreach($beans as $b) {
				if (!($b instanceof RedBean_OODBBean)) throw new RedBean_Exception_Security('Expected RedBean_OODBBean in array but got:'.gettype($b));
				$ids[] = $b->id;
			}
			$bean = reset($beans);
		} else $ids[] = $bean->id;
		$table = $this->getTable(array($bean->getMeta('type') , $type));
		if ($type == $bean->getMeta('type')) {
			$type .= '2';
			$cross = 1;
		} else $cross = 0;
		if (!$getLinks) $targetproperty = $type.'_id'; else $targetproperty = 'id';
		$property = $bean->getMeta('type').'_id';
		try {
			$sqlFetchKeys = $this->writer->selectRecord($table, array($property => $ids), $sql, false);
			$sqlResult = array();
			foreach($sqlFetchKeys as $row) {
				if (isset($row[$targetproperty])) {
					$sqlResult[] = $row[$targetproperty];
				}
			}
			if ($cross) {
				$sqlFetchKeys2 = $this->writer->selectRecord($table, array($targetproperty => $ids), $sql, false);
				foreach($sqlFetchKeys2 as $row) {
					if (isset($row[$property])) {
						$sqlResult[] = $row[$property];
					}
				}
			}
			return $sqlResult; //or returns rows in case of $sql != empty
		} catch(RedBean_Exception_SQL $e) {
			if (!$this->writer->sqlStateIn($e->getSQLState(),
			array(
			RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_COLUMN,
			RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_TABLE)
			)) throw $e;
			return array();
		}
	}
	/**
	 * Breaks the association between two beans. This method unassociates two beans. If the
	 * method succeeds the beans will no longer form an association. In the database
	 * this means that the association record will be removed. This method uses the
	 * OODB trash() method to remove the association links, thus giving FUSE models the
	 * opportunity to hook-in additional business logic. If the $fast parameter is
	 * set to boolean TRUE this method will remove the beans without their consent,
	 * bypassing FUSE. This can be used to improve performance.
	 *
	 * @param RedBean_OODBBean $bean1 first bean
	 * @param RedBean_OODBBean $bean2 second bean
	 * @param boolean          $fast  If TRUE, removes the entries by query without FUSE
	 */
	public function unassociate($beans1, $beans2, $fast = null) {
		if (!is_array($beans1)) $beans1 = array($beans1);
		if (!is_array($beans2)) $beans2 = array($beans2);
		foreach($beans1 as $bean1) {
			foreach($beans2 as $bean2) {
				$this->oodb->store($bean1);
				$this->oodb->store($bean2);
				$table = $this->getTable(array($bean1->getMeta('type') , $bean2->getMeta('type')));
				$type = $bean1->getMeta('type');
				if ($type == $bean2->getMeta('type')) {
					$type .= '2';
					$cross = 1;
				} else $cross = 0;
				$property1 = $type.'_id';
				$property2 = $bean2->getMeta('type').'_id';
				$value1 = (int) $bean1->id;
				$value2 = (int) $bean2->id;
				try {
					$rows = $this->writer->selectRecord($table, array(
						$property1 => array($value1), $property2 => array($value2)), null, $fast
					);
					if ($cross) {
						$rows2 = $this->writer->selectRecord($table, array(
						$property2 => array($value1), $property1 => array($value2)), null, $fast
						);
						if ($fast) continue;
						$rows = array_merge($rows, $rows2);
					}
					if ($fast) continue;
					$beans = $this->oodb->convertToBeans($table, $rows);
					foreach($beans as $link) $this->oodb->trash($link);
				} catch(RedBean_Exception_SQL $e) {
					if (!$this->writer->sqlStateIn($e->getSQLState(),
					array(
					RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_COLUMN,
					RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_TABLE)
					)) throw $e;
				}
			}
		}
	}
	/**
	 * Removes all relations for a bean. This method breaks every connection between
	 * a certain bean $bean and every other bean of type $type. Warning: this method
	 * is really fast because it uses a direct SQL query however it does not inform the
	 * models about this. If you want to notify FUSE models about deletion use a foreach-loop
	 * with unassociate() instead. (that might be slower though)
	 *
	 * @param RedBean_OODBBean $bean reference bean
	 * @param string           $type type of beans that need to be unassociated
	 *
	 * @return void
	 */
	public function clearRelations(RedBean_OODBBean $bean, $type) {
		$this->oodb->store($bean);
		$table = $this->getTable(array($bean->getMeta('type'), $type));
		if ($type == $bean->getMeta('type')) {
			$property2 = $type.'2_id';
			$cross = 1;
		}
		else $cross = 0;
		$property = $bean->getMeta('type').'_id';
		try {
			$this->writer->selectRecord($table, array($property => array($bean->id)), null, true);
			if ($cross) {
				$this->writer->selectRecord($table, array($property2 => array($bean->id)), null, true);
			}
		} catch(RedBean_Exception_SQL $e) {
			if (!$this->writer->sqlStateIn($e->getSQLState(),
			array(
			RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_COLUMN,
			RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_TABLE)
			)) throw $e;
		}
	}
	/**
	 * Given two beans this function returns TRUE if they are associated using a
	 * many-to-many association, FALSE otherwise.
	 *
	 * @throws RedBean_Exception_SQL
	 *
	 * @param RedBean_OODBBean $bean1 bean
	 * @param RedBean_OODBBean $bean2 bean
	 *
	 * @return bool $related whether they are associated N-M
	 */
	public function areRelated(RedBean_OODBBean $bean1, RedBean_OODBBean $bean2) {
		if (!$bean1->getID() || !$bean2->getID()) return false;
		$table = $this->getTable(array($bean1->getMeta('type'), $bean2->getMeta('type')));
		$type = $bean1->getMeta('type');
		if ($type == $bean2->getMeta('type')) {
			$type .= '2';
			$cross = 1;
		} else $cross = 0;
		$property1 = $type.'_id';
		$property2 = $bean2->getMeta('type').'_id';
		$value1 = (int) $bean1->id;
		$value2 = (int) $bean2->id;
		try {
			$rows = $this->writer->selectRecord($table, array(
				$property1 => array($value1), $property2 => array($value2)), null
			);
			if ($cross) {
				$rows2 = $this->writer->selectRecord($table, array(
				$property2 => array($value1), $property1 => array($value2)), null
				);
				$rows = array_merge($rows, $rows2);
			}
		}catch(RedBean_Exception_SQL $e) {
			if (!$this->writer->sqlStateIn($e->getSQLState(),
			array(
			RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_COLUMN,
			RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_TABLE)
			)) throw $e;
			return false;
		}
		return (count($rows)>0);
	}
	/**
	 * @deprecated
	 * @param array  $beans    beans
	 * @param string $property property
	 */
	public function swap($beans, $property) {
		$bean1 = array_shift($beans);
		$bean2 = array_shift($beans);
		$tmp = $bean1->$property;
		$bean1->$property = $bean2->$property;
		$bean2->$property = $tmp;
		$this->oodb->store($bean1);
		$this->oodb->store($bean2);
	}
	/**
	 * Returns all the beans associated with $bean.
	 * This method will return an array containing all the beans that have
	 * been associated once with the associate() function and are still
	 * associated with the bean specified. The type parameter indicates the
	 * type of beans you are looking for. You can also pass some extra SQL and
	 * values for that SQL to filter your results after fetching the
	 * related beans.
	 *
	 * Dont try to make use of subqueries, a subquery using IN() seems to
	 * be slower than two queries!
	 *
	 * Since 3.2, you can now also pass an array of beans instead just one
	 * bean as the first parameter.
	 *
	 * @param RedBean_OODBBean|array $bean the bean you have
	 * @param string				 $type the type of beans you want
	 * @param string				 $sql  SQL snippet for extra filtering
	 * @param array					 $val  values to be inserted in SQL slots
	 *
	 * @return array $beans	beans yielded by your query.
	 */
	public function relatedSimple($bean, $type, $sql = null, $values = array()) {
		$keys = $this->related($bean, $type);
		if (count($keys) == 0 || !is_array($keys)) return array();
		if (!$sql) return $this->oodb->batch($type, $keys);
		$rows = $this->writer->selectRecord($type, array('id' => $keys), array($sql, $values), false);
		return $this->oodb->convertToBeans($type, $rows);
	}
	/**
	* Returns only single associated bean.
	*
	* @param RedBean_OODBBean $bean bean provided
	* @param string $type type of bean you are searching for
	* @param string $sql SQL for extra filtering
	* @param array $values values to be inserted in SQL slots
	*
	*
	* @return RedBean_OODBBean $bean
	*/
	public function relatedOne(RedBean_OODBBean $bean, $type, $sql = null, $values = array()) {
		$beans = $this->relatedSimple($bean, $type, $sql, $values);
		if (!count($beans) || !is_array($beans)) return null;
		return reset($beans);
	}
	/**
	 * The opposite of related(). Returns all the beans that are not
	 * associated with the bean provided.
	 *
	 * @param RedBean_OODBBean $bean   bean provided
	 * @param string           $type   type of bean you are searching for
	 * @param string           $sql    SQL for extra filtering
	 * @param array            $values values to be inserted in SQL slots
	 *
	 * @return array $beans beans
	 */
	public function unrelated(RedBean_OODBBean $bean, $type, $sql = null, $values = array()) {
		$keys = $this->related($bean, $type);
		$rows = $this->writer->selectRecord($type, array('id' => $keys), array($sql, $values), false, true);
		return $this->oodb->convertToBeans($type, $rows);
	}
}

class RedBean_AssociationManager_ExtAssociationManager extends RedBean_AssociationManager {
	/**
	 * @deprecated
	 */
	public function extAssociate(RedBean_OODBBean $bean1, RedBean_OODBBean $bean2, RedBean_OODBBean $baseBean) {
		$table = $this->getTable(array($bean1->getMeta('type') , $bean2->getMeta('type')));
		$baseBean->setMeta('type', $table);
		return $this->associateBeans($bean1, $bean2, $baseBean);
	}
	/**
	 * @deprecated
	 */
	public function extAssociateSimple($beans1, $beans2, $extra = null) {
		if (!is_array($extra)) {
			$info = json_decode($extra, true);
			if (!$info) $info = array('extra' => $extra);
		} else {
			$info = $extra;
		}
		$bean = $this->oodb->dispense('xtypeless');
		$bean->import($info);
		return $this->extAssociate($beans1, $beans2, $bean);
	}
}

class RedBean_Setup {
	/**
	 * This method checks the DSN string.
	 * 
	 * @throws RedBean_Exception_NotImplemented
	 * 
	 * @param string $dsn
	 * 
	 * @return boolean $true
	 */
	private static function checkDSN($dsn) {
		if (!preg_match('/^(mysql|sqlite|pgsql|cubrid|oracle):/', strtolower(trim($dsn)))) trigger_error('Unsupported DSN');
		return true;
	}
	/**
	 * Initializes the database and prepares a toolbox.
	 *
	 * @param  string|PDO $dsn      Database Connection String (or PDO instance)
	 * @param  string     $username Username for database
	 * @param  string     $password Password for database
	 * @param  boolean    $frozen   Start in frozen mode?
	 *
	 * @return RedBean_ToolBox $toolbox
	 */
	public static function kickstart($dsn, $username = null, $password = null, $frozen = false ) {
		if ($dsn instanceof PDO) {
			$db = new RedBean_Driver_PDO($dsn); $dsn = $db->getDatabaseType();
		} else {
			self::checkDSN($dsn);
			if (strpos($dsn, 'oracle') === 0) 
				$db = new RedBean_Driver_OCI($dsn, $username, $password);	
			else
				$db = new RedBean_Driver_PDO($dsn, $username, $password);			
		}
		$adapter = new RedBean_Adapter_DBAdapter($db);
		if (strpos($dsn, 'pgsql') === 0) $writer = new RedBean_QueryWriter_PostgreSQL($adapter);
		else if (strpos($dsn, 'sqlite') === 0) $writer = new RedBean_QueryWriter_SQLiteT($adapter);
		else if (strpos($dsn, 'cubrid') === 0) $writer = new RedBean_QueryWriter_CUBRID($adapter);
		else if (strpos($dsn, 'oracle') === 0) $writer = new RedBean_QueryWriter_Oracle($adapter);	
		else $writer = new RedBean_QueryWriter_MySQL($adapter);
		$redbean = new RedBean_OODB($writer);
		if ($frozen) $redbean->freeze(true);
		$toolbox = new RedBean_ToolBox($redbean, $adapter, $writer);
		return $toolbox;
	}
}

interface RedBean_IModelFormatter {
	/**
	 * ModelHelper will call this method of the class
	 * you provide to discover the model
	 *
	 * @param string $model
	 *
	 * @return string $formattedModel
	 */
	public function formatModel($model);
}

interface RedBean_Logger {
  /**
   * @param string $message the message to log. (optional)
   */
  public function log();
}

class RedBean_Logger_Default implements RedBean_Logger {
  /**
   * Default logger method logging to STDOUT.
   * This is the default/reference implementation of a logger.
   * This method will write the message value to STDOUT (screen).
   *
   * @param $message (optional)
   */
  public function log() {
    if (func_num_args() > 0) {
      foreach (func_get_args() as $argument) {
        if (is_array($argument)) echo print_r($argument, true); else echo $argument;
		echo "<br>\n";
      }
    }
  }
}

interface RedBean_BeanHelper {
	/**
	 * @abstract
	 * @return RedBean_Toolbox $toolbox toolbox
	 */
	public function getToolbox();	
	/**
	 * Given a certain bean this method will
	 * return the corresponding model.
	 * 
	 * @param RedBean_OODBBean $bean
	 */
	public function getModelForBean(RedBean_OODBBean $bean);	
}

class RedBean_BeanHelper_Facade implements RedBean_BeanHelper {
	/**
	 * @see RedBean_BeanHelper::getToolbox
	 */
	public function getToolbox() {
		return RedBean_Facade::$toolbox;
	}
	/**
	 * @see RedBean_BeanHelper::getModelForBean
	 */
	public function getModelForBean(RedBean_OODBBean $bean) {
		$modelName = RedBean_ModelHelper::getModelName($bean->getMeta('type'), $bean);
		if (!class_exists($modelName)) return null;
		$obj = RedBean_ModelHelper::factory($modelName);
		$obj->loadBean($bean);
		return $obj;
	}
}

class RedBean_SimpleModel {
	/**
	 * @var RedBean_OODBBean
	 */
	protected $bean;
	/**
	 * Used by FUSE: the ModelHelper class to connect a bean to a model.
	 * This method loads a bean in the model.
	 *
	 * @param RedBean_OODBBean $bean bean
	 */
	public function loadBean(RedBean_OODBBean $bean) {
		$this->bean = $bean;
	}
	/**
	 * Magic Getter to make the bean properties available from
	 * the $this-scope.
	 *
	 * @param string $prop property
	 *
	 * @return mixed $propertyValue value
	 */
	public function __get($prop) {
		return $this->bean->$prop;
	}
	/**
	 * Magic Setter
	 *
	 * @param string $prop  property
	 * @param mixed  $value value
	 */
	public function __set($prop, $value) {
		$this->bean->$prop = $value;
	}
	/**
	 * Isset implementation
	 *
	 * @param  string $key key
	 *
	 * @return
	 */
	public function __isset($key) {
		return (isset($this->bean->$key));
	}
	/**
	 * Box the bean using the current model.
	 * 
	 * @return RedBean_SimpleModel $box a bean in a box
	 */
	public function box() {
		return $this;
	}
	/**
	 * Unbox the bean from the model.
	 * 
	 * @return RedBean_OODBBean $bean bean 
	 */
	public function unbox(){
		return $this->bean;
	}
}

class RedBean_ModelHelper implements RedBean_Observer {
	/**
	 * @var RedBean_IModelFormatter
	 */
	private static $modelFormatter;
	/**
	 * @var type 
	 */
	private static $dependencyInjector;
	/**
	 * @var array 
	 */
	private static $modelCache = array();
	/**
	 * @see RedBean_Observer::onEvent
	 */
	public function onEvent($eventName, $bean) {
		$bean->$eventName();
	}
	/**
	 * Given a model ID (model identifier) this method returns the
	 * full model name.
	 *
	 * @param string $model
	 * @param RedBean_OODBBean $bean
	 * 
	 * @return string $fullname
	 */
	public static function getModelName($model, $bean = null) {
		if (isset(self::$modelCache[$model])) return self::$modelCache[$model];
		if (self::$modelFormatter){
			$modelID = self::$modelFormatter->formatModel($model, $bean);
		} else {
			$modelID = 'Model_'.ucfirst($model);
		}
		self::$modelCache[$model] = $modelID;
		return self::$modelCache[$model];
	}
	/**
	 * Sets the model formatter to be used to discover a model
	 * for Fuse.
	 *
	 * @param string $modelFormatter
	 */
	public static function setModelFormatter($modelFormatter) {
		self::$modelFormatter = $modelFormatter;
	}
	/**
	 * Obtains a new instance of $modelClassName, using a dependency injection
	 * container if possible.
	 * 
	 * @param string $modelClassName name of the model
	 */
	public static function factory($modelClassName) {
		if (self::$dependencyInjector) {
			return self::$dependencyInjector->getInstance($modelClassName);
		}
		return new $modelClassName();
	}
	/**
	 * Sets the dependency injector to be used.
	 * 
	 * @param RedBean_DependencyInjector $di injecto to be used
	 */
	public static function setDependencyInjector(RedBean_DependencyInjector $di) {
		self::$dependencyInjector = $di;
	}
	/**
	 * Stops the dependency injector from resolving dependencies. Removes the
	 * reference to the dependency injector.
	 */
	public static function clearDependencyInjector() {
		self::$dependencyInjector = null;
	}
	/**
	 * Attaches the FUSE event listeners. Now the Model Helper will listen for
	 * CRUD events. If a CRUD event occurs it will send a signal to the model
	 * that belongs to the CRUD bean and this model will take over control from
	 * there.
	 * 
	 * @param Observable $observable 
	 */
	public function attachEventListeners(RedBean_Observable $observable) {
		foreach(array('update', 'open', 'delete', 'after_delete', 'after_update', 'dispense') as $e) $observable->addEventListener($e, $this);
	}
}

 class RedBean_SQLHelper {
	/**
	 * @var RedBean_Adapter 
	 */
	protected $adapter;
	/**
	 * @var boolean
	 */
	protected $capture = false;
	/**
	 * @var string
	 */
	protected $sql = '';
	/**
	 * @var array
	 */
	protected $params = array();
	/**
	 * Constructor
	 * 
	 * @param RedBean_DBAdapter $adapter database adapter for querying
	 */
	public function __construct(RedBean_Adapter $adapter) {
		$this->adapter = $adapter;
	}
	/**
	 * Magic method to construct SQL query
	 * 
	 * @param string $funcName name of the next SQL statement/keyword
	 * @param array  $args     list of statements to be seperated by commas
	 * 
	 * @return mixed $result   either self or result depending on mode 
	 */
	public function __call($funcName, $args = array()) {
		$funcName = str_replace('_', ' ', $funcName);
		if ($this->capture) {
			$this->sql .= ' '.$funcName . ' '.implode(',', $args);
			return $this;
		} else {
			return $this->adapter->getCell('SELECT '.$funcName.'('.implode(',', $args).')');	
		}	
	}
	/**
	 * Begins SQL query
	 * 
	 * @return RedBean_SQLHelper $this chainable
	 */
	public function begin() {
		$this->capture = true;
		return $this;
	}
	/**
	 * Adds a value to the parameter list
	 * 
	 * @param mixed $param parameter to be added
	 * 
	 * @return RedBean_SQLHelper $this chainable
	 */
	public function put($param) {
		$this->params[] = $param;
		return $this;
	}
	/**
	 * Executes query and returns result
	 * 
	 * @return mixed $result
	 */
	public function get($what = '') {
		$what = 'get'.ucfirst($what);
		$rs = $this->adapter->$what($this->sql, $this->params);
		$this->clear();
		return $rs;
	}
	/**
	 * Clears the parameter list as well as the SQL query string.
	 * 
	 * @return RedBean_SQLHelper $this chainable
	 */
	public function clear() {
		$this->sql = '';
		$this->params = array();
		$this->capture = false; //turn off capture mode (issue #142)
		return $this;
	}
	/**
	 * To explicitly add a piece of SQL.
	 * 
	 * @param string $sql sql
	 * 
	 * @return RedBean_SQLHelper 
	 */
	public function addSQL($sql) {
		if ($this->capture) {
			$this->sql .= ' '.$sql.' ';
			return $this;
		}
	}
	/**
	 * Returns query parts.
	 * 
	 * @return array $queryParts query parts. 
	 */
	public function getQuery() {
		$list = array($this->sql, $this->params);
		$this->clear();
		return $list;
	}
	/**
	 * Nests another query builder query in the current query.
	 * 
	 * @param RedBean_SQLHelper $sqlHelper 
	 */
	public function nest(RedBean_SQLHelper $sqlHelper) {
		list($sql, $params) = $sqlHelper->getQuery();
		$this->sql .= $sql;
		$this->params += $params;
		return $this;
	}
	/**
	 * Writes a '(' to the sql query.
	 */
	public function open() {
		if ($this->capture) {
			$this->sql .= ' ( ';
			return $this;
		}
	}
	/**
	 * Writes a ')' to the sql query.
	 */
	public function close() {
		if ($this->capture) {
			$this->sql .= ' ) ';
			return $this;
		}
	}
	/**
	 * Generates question mark slots for an array of values.
	 *
	 * @param array $array Array with values to generate slots for
	 * 
	 * @return string $slots
	 */
	public function genSlots($array) {
		if (is_array($array) && count($array)>0) {
			$filler = array_fill(0, count($array), '?');
			return implode(',', $filler);
		} else {
			return '';
		}
	}
	/**
	 * Returns a new SQL Helper with the same adapter as the current one.
	 * 
	 * @return RedBean_SQLHelper 
	 */
	public function getNew() {
		return new self($this->adapter);
	}
}

class RedBean_TagManager {
	/**
	 * @var RedBean_Toolbox 
	 */
	protected $toolbox;
	/**
	 * @var RedBean_AssociationManager
	 */
	protected $associationManager;
	/**
	 * @var RedBean_OODBBean 
	 */
	protected $redbean;
	/**
	 * Constructor
	 *
	 * @param RedBean_Toolbox $toolbox 
	 */
	public function __construct(RedBean_Toolbox $toolbox) {
		$this->toolbox = $toolbox;
		$this->redbean = $toolbox->getRedBean();
		$this->associationManager = $this->redbean->getAssociationManager();
	}
	/**
	 * Finds a tag bean by it's title.
	 * 
	 * @param string $title title
	 * 
	 * @return RedBean_OODBBean $bean | null
	 */
	public function findTagByTitle($title) {
		$beans = $this->redbean->find('tag', array('title' => array($title)));
		if ($beans) {
			$bean = reset($beans);
			return $bean;
		}
		return null;
	}
	/**
	 * Tests whether a bean has been associated with one ore more
	 * of the listed tags. If the third parameter is TRUE this method
	 * will return TRUE only if all tags that have been specified are indeed
	 * associated with the given bean, otherwise FALSE.
	 * If the third parameter is FALSE this
	 * method will return TRUE if one of the tags matches, FALSE if none
	 * match.
	 *
	 * @param  RedBean_OODBBean $bean bean to check for tags
	 * @param  array            $tags list of tags
	 * @param  boolean          $all  whether they must all match or just some
	 *
	 * @return boolean $didMatch whether the bean has been assoc. with the tags
	 */
	public function hasTag($bean, $tags, $all = false) {
		$foundtags = $this->tag($bean);
		if (is_string($foundtags)) $foundtags = explode(',', $tags);
		$same = array_intersect($tags, $foundtags);
		if ($all) return (implode(',', $same) === implode(',', $tags));
		return (bool) (count($same)>0);
	}
	/**
	 * Removes all sepcified tags from the bean. The tags specified in
	 * the second parameter will no longer be associated with the bean.
	 *
	 * @param  RedBean_OODBBean $bean    tagged bean
	 * @param  array            $tagList list of tags (names)
	 *
	 * @return void
	 */
	public function untag($bean, $tagList) {
		if ($tagList !== false && !is_array($tagList)) $tags = explode(",", (string)$tagList); else $tags = $tagList;
		foreach($tags as $tag) if ($t = $this->findTagByTitle($tag)) $this->associationManager->unassociate($bean, $t);
	}
	/**
	 * Tags a bean or returns tags associated with a bean.
	 * If $tagList is null or omitted this method will return a
	 * comma separated list of tags associated with the bean provided.
	 * If $tagList is a comma separated list (string) of tags all tags will
	 * be associated with the bean.
	 * You may also pass an array instead of a string.
	 *
	 * @param RedBean_OODBBean $bean    bean
	 * @param mixed				$tagList tags
	 *
	 * @return string $commaSepListTags
	 */
	public function tag(RedBean_OODBBean $bean, $tagList = null) {
		if (is_null($tagList)) {
			$tags = array();
			$keys = $this->associationManager->related($bean, 'tag'); 
			if ($keys) {
				$tags = $this->redbean->batch('tag', $keys);
			}
			$foundTags = array();
			foreach($tags as $tag) {
				$foundTags[] = $tag->title;
			}
			return $foundTags;
		}
		$this->associationManager->clearRelations($bean, 'tag');
		$this->addTags($bean, $tagList);
	}
	/**
	 * Adds tags to a bean.
	 * If $tagList is a comma separated list of tags all tags will
	 * be associated with the bean.
	 * You may also pass an array instead of a string.
	 *
	 * @param RedBean_OODBBean  $bean    bean
	 * @param array				$tagList list of tags to add to bean
	 *
	 * @return void
	 */
	public function addTags(RedBean_OODBBean $bean, $tagList) {
		if ($tagList !== false && !is_array($tagList)) $tags = explode(",", (string)$tagList); else $tags = $tagList;
		if ($tagList === false) return;
		foreach($tags as $tag) {
			if (!$t = $this->findTagByTitle($tag)) {
				$t = $this->redbean->dispense('tag');
				$t->title = $tag;
				$this->redbean->store($t);
			}
			$this->associationManager->associate($bean, $t);
		}
	}
	/**
	 * Returns all beans that have been tagged with one of the tags given.
	 *
	 * @param  $beanType type of bean you are looking for
	 * @param  $tagList  list of tags to match
	 *
	 * @return array
	 */
	public function tagged($beanType, $tagList) {
		if ($tagList !== false && !is_array($tagList)) $tags = explode(",", (string) $tagList); else $tags = $tagList;
		$collection = array();
		$tags = $this->redbean->find('tag', array('title' => $tags));
		if (is_array($tags) && count($tags)>0) {
			$collectionKeys = $this->associationManager->related($tags, $beanType);
			if ($collectionKeys) {
				$collection = $this->redbean->batch($beanType, $collectionKeys);
			}
		}
		return $collection;
	}
	/**
	 * Returns all beans that have been tagged with ALL of the tags given.
	 *
	 * @param  $beanType type of bean you are looking for
	 * @param  $tagList  list of tags to match
	 *
	 * @return array
	 */
	public function taggedAll($beanType, $tagList) {
		if ($tagList !== false && !is_array($tagList)) $tags = explode(",", (string)$tagList); else $tags = $tagList;
		$beans = array();
		foreach($tags as $tag) {
			$beans = $this->tagged($beanType, $tag);
			if (isset($oldBeans)) $beans = array_intersect_assoc($beans, $oldBeans);
			$oldBeans = $beans;
		}
		return $beans;
	}
}

class RedBean_LabelMaker {	
	/**
	 * @var RedBean_Toolbox 
	 */
	protected $toolbox;
	/**
	 * Constructor.
	 * 
	 * @param RedBean_ToolBox $toolbox 
	 */
	public function __construct(RedBean_ToolBox $toolbox) {
		$this->toolbox = $toolbox;
	}
	/**
	 * A label is a bean with only an id, type and name property.
	 * This function will dispense beans for all entries in the array. The
	 * values of the array will be assigned to the name property of each
	 * individual bean.
	 *
	 * @param string $type   type of beans you would like to have
	 * @param array  $labels list of labels, names for each bean
	 *
	 * @return array $bean a list of beans with type and name property
	 */
	public function dispenseLabels($type, $labels) {
		$labelBeans = array();
		foreach($labels as $label) {
			$labelBean = $this->toolbox->getRedBean()->dispense($type);
			$labelBean->name = $label;
			$labelBeans[] = $labelBean;
		}
		return $labelBeans;
	}
	/**
	 * Gathers labels from beans. This function loops through the beans,
	 * collects the values of the name properties of each individual bean
	 * and stores the names in a new array. The array then gets sorted using the
	 * default sort function of PHP (sort).
	 *
	 * @param array $beans list of beans to loop
	 *
	 * @return array $array list of names of beans
	 */
	public function gatherLabels($beans) {
		$labels = array();
		foreach($beans as $bean) $labels[] = $bean->name;
		sort($labels);
		return $labels;
	}
}

class RedBean_Finder {
	/**
	 * @var RedBean_ToolBox 
	 */
	protected $toolbox;
	/**
	 * @var RedBean_OODB
	 */
	protected $redbean;
	/**
	 * Constructor.
	 * The Finder requires a toolbox.
	 * 
	 * @param RedBean_ToolBox $toolbox 
	 */
	public function __construct(RedBean_ToolBox $toolbox) {
		$this->toolbox = $toolbox;
		$this->redbean = $toolbox->getRedBean();
	}
	/**
	 * Finds a bean using a type and a where clause (SQL).
	 * As with most Query tools in RedBean you can provide values to
	 * be inserted in the SQL statement by populating the value
	 * array parameter; you can either use the question mark notation
	 * or the slot-notation (:keyname).
	 *
	 * @param string $type   type   the type of bean you are looking for
	 * @param string $sql    sql    SQL query to find the desired bean, starting right after WHERE clause
	 * @param array  $values values array of values to be bound to parameters in query
	 *
	 * @return array $beans  beans
	 */
	public function find($type, $sql = null, $values = array()) {
		if ($sql instanceof RedBean_SQLHelper) list($sql, $values) = $sql->getQuery();
		if (!is_array($values)) throw new InvalidArgumentException('Expected array, ' . gettype($values) . ' given.');
		return $this->redbean->find($type, array(), array($sql, $values));
	}
	/**
	 * @see RedBean_Finder::find
	 * The findAll() method differs from the find() method in that it does
	 * not assume a WHERE-clause, so this is valid:
	 *
	 * R::findAll('person', ' ORDER BY name DESC ');
	 *
	 * Your SQL does not have to start with a valid WHERE-clause condition.
	 * 
	 * @param string $type   type   the type of bean you are looking for
	 * @param string $sql    sql    SQL query to find the desired bean, starting right after WHERE clause
	 * @param array  $values values array of values to be bound to parameters in query
	 *
	 * @return array $beans  beans
	 */
	public function findAll($type, $sql = null, $values = array()) {
		if ($sql instanceof RedBean_SQLHelper) list($sql, $values) = $sql->getQuery();
		if (!is_array($values)) throw new InvalidArgumentException('Expected array, '.gettype($values).' given.');
		return $this->redbean->find($type, array(), array($sql, $values), true);
	}
	/**
	 * @see RedBean_Finder::find
	 * The variation also exports the beans (i.e. it returns arrays).
	 * 
	 * @param string $type   type   the type of bean you are looking for
	 * @param string $sql    sql    SQL query to find the desired bean, starting right after WHERE clause
	 * @param array  $values values array of values to be bound to parameters in query
	 *
	 * @return array $arrays arrays
	 */
	public function findAndExport($type, $sql = null, $values = array()) {
		$items = $this->find($type, $sql, $values);
		$arr = array();
		foreach($items as $key => $item) $arr[$key] = $item->export();
		return $arr;
	}
	/**
	 * @see RedBean_Finder::find
	 * This variation returns the first bean only.
	 * 
	 * @param string $type   type  
	 * @param string $sql    sql    
	 * @param array  $values values 
	 *
	 * @return RedBean_OODBBean $bean
	 */
	public function findOne($type, $sql = null, $values = array()) {
		$items = $this->find($type, $sql, $values);
		$found = reset($items);
		if (!$found) return null;
		return $found;
	}
	/**
	 * @see RedBean_Finder::find
	 * This variation returns the last bean only.
	 * 
	 * @param string $type   type   
	 * @param string $sql    sql    
	 * @param array  $values values 
	 *
	 * @return RedBean_OODBBean $bean
	 */
	public function findLast($type, $sql = null, $values = array()) {
		$items = $this->find($type, $sql, $values);
		$found = end($items);
		if (!$found) return null;
		return $found;
	}
	/**
	 * @see RedBean_Finder::find
	 * Convience method. Tries to find beans of a certain type,
	 * if no beans are found, it dispenses a bean of that type.
	 *
	 * @param  string $type   type
	 * @param  string $sql    sql
	 * @param  array  $values values
	 *
	 * @return array $beans Contains RedBean_OODBBean instances
	 */
	public function findOrDispense($type, $sql = null, $values = array()) {
		$foundBeans = $this->find($type, $sql, $values);
		if (count($foundBeans) == 0) return array($this->redbean->dispense($type)); else return $foundBeans;
	}
}

class RedBean_Facade {
	/**
	 * @var array
	 */
	public static $toolboxes = array();
	/**
	 * @var RedBean_ToolBox
	 */
	public static $toolbox;
	/**
	 * @var RedBean_OODB
	 */
	public static $redbean;
	/**
	 * @var RedBean_QueryWriter
	 */
	public static $writer;
	/**
	 * @var RedBean_DBAdapter
	 */
	public static $adapter;
	/**
	 * @var RedBean_AssociationManager
	 */
	public static $associationManager;
	/**
	 * @var RedBean_ExtAssociationManager
	 */
	public static $extAssocManager;
	/**
	 * @var RedBean_TagManager
	 */
	public static $tagManager;
	/**
	 * @var RedBean_DuplicationManager 
	 */
	public static $duplicationManager;
	/**
	 * @var RedBean_LabelMaker 
	 */
	public static $labelMaker;
	/**
	 * @var RedBean_Finder
	 */
	public static $finder;
	/**
	 * @var string
	 */
	public static $currentDB = '';
	/**
	 * @var RedBean_SQLHelper
	 */
	public static $f;
	/**
	 * @var boolean
	 */
	private static $strictType = true;
	/**
	 * Get version
	 * @return string
	 */
	public static function getVersion() {
		return '3.4.2';
	}
	/**
	 * Kickstarts redbean for you. This method should be called before you start using
	 * RedBean. The Setup() method can be called without any arguments, in this case it will
	 * try to create a SQLite database in /tmp called red.db (this only works on UNIX-like systems).
	 *
	 * @param string $dsn      Database connection string
	 * @param string $username Username for database
	 * @param string $password Password for database
	 *
	 * @return void
	 */
	public static function setup($dsn = null, $username = null, $password = null) {
		if (function_exists('sys_get_temp_dir')) $tmp = sys_get_temp_dir(); else $tmp = 'tmp';
		if (is_null($dsn)) $dsn = 'sqlite:/'.$tmp.'/red.db';
		self::addDatabase('default', $dsn, $username, $password);
		self::selectDatabase('default');
		return self::$toolbox;
	}
	/**
	 * Starts a transaction within a closure (or other valid callback).
	 * If an Exception is thrown inside, the operation is automatically rolled back.
	 * If no Exception happens, it commits automatically.
	 * It also supports (simulated) nested transactions (that is useful when 
	 * you have many methods that needs transactions but are unaware of
	 * each other).
	 * ex:
	 * 		$from = 1;
	 * 		$to = 2;
	 * 		$ammount = 300;
	 * 		
	 * 		R::transaction(function() use($from, $to, $ammount)
	 * 	    {
	 * 			$accountFrom = R::load('account', $from);
	 * 			$accountTo = R::load('account', $to);
	 * 			
	 * 			$accountFrom->money -= $ammount;
	 * 			$accountTo->money += $ammount;
	 * 			
	 * 			R::store($accountFrom);
	 * 			R::store($accountTo);
	 *      });
	 * 
	 * @param callable $callback Closure (or other callable) with the transaction logic
	 * 
	 * @return void
	 */
	public static function transaction($callback) {
		if (!is_callable($callback)) throw new InvalidArgumentException('R::transaction needs a valid callback.');
		static $depth = 0;
		try {
			if ($depth == 0) self::begin();
			$depth++;
			call_user_func($callback); //maintain 5.2 compatibility
			$depth--;
			if ($depth == 0) self::commit();
		} catch(Exception $e) {
			$depth--;
			if ($depth == 0) self::rollback();
			throw $e;
		}
	}
	/**
	 * Adds a database to the facade, afterwards you can select the database using
	 * selectDatabase($key).
	 *
	 * @param string      $key    ID for the database
	 * @param string      $dsn    DSN for the database
	 * @param string      $user   User for connection
	 * @param null|string $pass   Password for connection
	 * @param bool        $frozen Whether this database is frozen or not
	 *
	 * @return void
	 */
	public static function addDatabase($key, $dsn, $user = null, $pass = null, $frozen = false) {
		self::$toolboxes[$key] = RedBean_Setup::kickstart($dsn, $user, $pass, $frozen);
	}
	/**
	 * Selects a different database for the Facade to work with.
	 *
	 * @param  string $key Key of the database to select
	 * @return int 1
	 */
	public static function selectDatabase($key) {
		if (self::$currentDB === $key) return false;
		self::configureFacadeWithToolbox(self::$toolboxes[$key]);
		self::$currentDB = $key;
		return true;
	}
	/**
	 * Toggles DEBUG mode.
	 * In Debug mode all SQL that happens under the hood will
	 * be printed to the screen or logged by provided logger.
	 *
	 * @param boolean $tf
	 * @param RedBean_Logger $logger
	 */
	public static function debug($tf = true, $logger = NULL) {
		if (!$logger) $logger = new RedBean_Logger_Default;
		if (!isset(self::$adapter)) throw new RedBean_Exception_Security('Use R::setup() first.');
		self::$adapter->getDatabase()->setDebugMode($tf, $logger);
	}
	/**
	 * Stores a RedBean OODB Bean and returns the ID.
	 *
	 * @param  RedBean_OODBBean|RedBean_SimpleModel $bean bean
	 *
	 * @return integer $id id
	 */
	public static function store($bean) {
		return self::$redbean->store($bean);
	}
	/**
	 * Toggles fluid or frozen mode. In fluid mode the database
	 * structure is adjusted to accomodate your objects. In frozen mode
	 * this is not the case.
	 *
	 * You can also pass an array containing a selection of frozen types.
	 * Let's call this chilly mode, it's just like fluid mode except that
	 * certain types (i.e. tables) aren't touched.
	 *
	 * @param boolean|array $trueFalse
	 */
	public static function freeze($tf = true) {
		self::$redbean->freeze($tf);
	}
	/**
	* Loads multiple types of beans with the same ID.
	* This might look like a strange method, however it can be useful
	* for loading a one-to-one relation.
	* 
	* Usage:
	* list($author, $bio) = R::load('author, bio', $id);
	*
	* @param string|array $types
	* @param mixed        $id
	*
	* @return RedBean_OODBBean $bean
	*/ 
	public static function loadMulti($types, $id) {
		if (is_string($types) && strpos($types, ',') !== false) $types = explode(',', $types);
		if (is_array($types)) {
			$list = array();
			foreach($types as $typeItem) {
				$list[] = self::$redbean->load($typeItem, $id);
			}
			return $list;
		}
		return array();
	}
	/**
	 * Loads the bean with the given type and id and returns it.
	 *
	 * Usage:
	 * $book = R::load('book', $id); -- loads a book bean
	 *
	 * Can also load one-to-one related beans:
	 * 
	 * @param string  $type type
	 * @param integer $id   id of the bean you want to load
	 *
	 * @return RedBean_OODBBean $bean
	 */
	public static function load($type, $id) {
		return self::$redbean->load($type, $id);
	}
	/**
	 * Deletes the specified bean.
	 *
	 * @param RedBean_OODBBean|RedBean_SimpleModel $bean bean to be deleted
	 *
	 * @return mixed
	 */
	public static function trash($bean) {
		return self::$redbean->trash($bean);
	}
	/**
	 * Dispenses a new RedBean OODB Bean for use with
	 * the rest of the methods.
	 * 
	 * @todo extract from facade
	 * 
	 *
	 * @param string  $type   type
	 * @param integer $number number of beans to dispense
	 * 
	 * @return array $oneOrMoreBeans
	 */
	public static function dispense($type, $num = 1) {
		if (!preg_match('/^[a-z0-9]+$/', $type) && self::$strictType) throw new RedBean_Exception_Security('Invalid type: '.$type); 
		return self::$redbean->dispense($type, $num);
	}
	/**
	 * Toggles strict bean type names.
	 * If set to true (default) this will forbid the use of underscores and 
	 * uppercase characters in bean type strings (R::dispense).
	 * 
	 * @param boolean $trueFalse 
	 */
	public static function setStrictTyping($trueFalse) {
		self::$strictType = (boolean) $trueFalse;
	}
	/**
	 * Convience method. Tries to find beans of a certain type,
	 * if no beans are found, it dispenses a bean of that type.
	 *
	 * @param  string $type   type of bean you are looking for
	 * @param  string $sql    SQL code for finding the bean
	 * @param  array  $values parameters to bind to SQL
	 *
	 * @return array $beans Contains RedBean_OODBBean instances
	 */
	public static function findOrDispense($type, $sql = null, $values = array()) {
		return self::$finder->findOrDispense($type, $sql, $values);
	}
	/**
	 * Associates two Beans. This method will associate two beans with eachother.
	 * You can then get one of the beans by using the related() function and
	 * providing the other bean. You can also provide a base bean in the extra
	 * parameter. This base bean allows you to add extra information to the association
	 * record. Note that this is for advanced use only and the information will not
	 * be added to one of the beans, just to the association record.
	 * It's also possible to provide an array or JSON string as base bean. If you
	 * pass a scalar this function will interpret the base bean as having one
	 * property called 'extra' with the value of the scalar.
	 *
	 * @todo extract from facade
	 * 
	 * @param RedBean_OODBBean $bean1 bean that will be part of the association
	 * @param RedBean_OODBBean $bean2 bean that will be part of the association
	 * @param mixed $extra            bean, scalar, array or JSON providing extra data.
	 *
	 * @return mixed
	 */
	public static function associate($beans1, $beans2, $extra = null) {
		if (!$extra) {
			return self::$associationManager->associate($beans1, $beans2);
		} else {
			return self::$extAssocManager->extAssociateSimple($beans1, $beans2, $extra);
		}
	}
	/**
	 * Breaks the association between two beans.
	 * This functions breaks the association between a pair of beans. After
	 * calling this functions the beans will no longer be associated with
	 * eachother. Calling related() with either one of the beans will no longer
	 * return the other bean.
	 *
	 * @param RedBean_OODBBean $bean1 bean
	 * @param RedBean_OODBBean $bean2 bean
	 *
	 * @return mixed
	 */
	public static function unassociate($beans1,  $beans2, $fast = false) {
		return self::$associationManager->unassociate($beans1, $beans2, $fast);
	}
	/**
	 * Returns all the beans associated with $bean.
	 * This method will return an array containing all the beans that have
	 * been associated once with the associate() function and are still
	 * associated with the bean specified. The type parameter indicates the
	 * type of beans you are looking for. You can also pass some extra SQL and
	 * values for that SQL to filter your results after fetching the
	 * related beans.
	 *
	 * Dont try to make use of subqueries, a subquery using IN() seems to
	 * be slower than two queries!
	 *
	 * Since 3.2, you can now also pass an array of beans instead just one
	 * bean as the first parameter.
	 *
	 * @param RedBean_OODBBean|array $bean the bean you have
	 * @param string				 $type the type of beans you want
	 * @param string				 $sql  SQL snippet for extra filtering
	 * @param array					 $val  values to be inserted in SQL slots
	 *
	 * @return array $beans	beans yielded by your query.
	 */
	public static function related($bean, $type, $sql = null, $values = array()) {
		return self::$associationManager->relatedSimple($bean, $type, $sql, $values);
	}
	/**
	* Returns only single associated bean.
	*
	* @param RedBean_OODBBean $bean bean provided
	* @param string $type type of bean you are searching for
	* @param string $sql SQL for extra filtering
	* @param array $values values to be inserted in SQL slots
	*
	*
	* @return RedBean_OODBBean $bean
	*/
	public static function relatedOne(RedBean_OODBBean $bean, $type, $sql = null, $values = array()) {
		return self::$associationManager->relatedOne($bean, $type, $sql, $values);
	}
	/**
	 * Checks whether a pair of beans is related N-M. This function does not
	 * check whether the beans are related in N:1 way.
	 *
	 * @param RedBean_OODBBean $bean1 first bean
	 * @param RedBean_OODBBean $bean2 second bean
	 *
	 * @return bool $yesNo whether they are related
	 */
	public static function areRelated(RedBean_OODBBean $bean1, RedBean_OODBBean $bean2) {
		return self::$associationManager->areRelated($bean1, $bean2);
	}
	/**
	 * The opposite of related(). Returns all the beans that are not
	 * associated with the bean provided.
	 *
	 * @param RedBean_OODBBean $bean   bean provided
	 * @param string           $type   type of bean you are searching for
	 * @param string           $sql    SQL for extra filtering
	 * @param array            $values values to be inserted in SQL slots
	 *
	 * @return array $beans beans
	 */
	public static function unrelated(RedBean_OODBBean $bean, $type, $sql = null, $values = array()) {
		return self::$associationManager->unrelated($bean, $type, $sql, $values);
	}
	/**
	 * Clears all associated beans.
	 * Breaks all many-to-many associations of a bean and a specified type.
	 *
	 * @param RedBean_OODBBean $bean bean you wish to clear many-to-many relations for
	 * @param string           $type type of bean you wish to break associatons with
	 *
	 * @return void
	 */
	public static function clearRelations(RedBean_OODBBean $bean, $type) {
		self::$associationManager->clearRelations($bean, $type);
	}
	/**
	 * Finds a bean using a type and a where clause (SQL).
	 * As with most Query tools in RedBean you can provide values to
	 * be inserted in the SQL statement by populating the value
	 * array parameter; you can either use the question mark notation
	 * or the slot-notation (:keyname).
	 *
	 * @param string $type   type   the type of bean you are looking for
	 * @param string $sql    sql    SQL query to find the desired bean, starting right after WHERE clause
	 * @param array  $values values array of values to be bound to parameters in query
	 *
	 * @return array $beans  beans
	 */
	public static function find($type, $sql = null, $values = array()) {
		return self::$finder->find($type, $sql, $values);
	}
	/**
	 * @see RedBean_Facade::find
	 * The findAll() method differs from the find() method in that it does
	 * not assume a WHERE-clause, so this is valid:
	 *
	 * R::findAll('person',' ORDER BY name DESC ');
	 *
	 * Your SQL does not have to start with a valid WHERE-clause condition.
	 * 
	 * @param string $type   type   the type of bean you are looking for
	 * @param string $sql    sql    SQL query to find the desired bean, starting right after WHERE clause
	 * @param array  $values values array of values to be bound to parameters in query
	 *
	 * @return array $beans  beans
	 */
	public static function findAll($type, $sql = null, $values = array()) {
		return self::$finder->findAll($type, $sql, $values);
	}
	/**
	 * @see RedBean_Facade::find
	 * The variation also exports the beans (i.e. it returns arrays).
	 * 
	 * @param string $type   type   the type of bean you are looking for
	 * @param string $sql    sql    SQL query to find the desired bean, starting right after WHERE clause
	 * @param array  $values values array of values to be bound to parameters in query
	 *
	 * @return array $arrays arrays
	 */
	public static function findAndExport($type, $sql = null, $values = array()) {
		return self::$finder->findAndExport($type, $sql, $values);
	}
	/**
	 * @see RedBean_Facade::find
	 * This variation returns the first bean only.
	 * 
	 * @param string $type   type   the type of bean you are looking for
	 * @param string $sql    sql    SQL query to find the desired bean, starting right after WHERE clause
	 * @param array  $values values array of values to be bound to parameters in query
	 *
	 * @return RedBean_OODBBean $bean
	 */
	public static function findOne($type, $sql = null, $values = array()) {
		return self::$finder->findOne($type, $sql, $values);
	}
	/**
	 * @see RedBean_Facade::find
	 * This variation returns the last bean only.
	 * 
	 * @param string $type   type   the type of bean you are looking for
	 * @param string $sql    sql    SQL query to find the desired bean, starting right after WHERE clause
	 * @param array  $values values array of values to be bound to parameters in query
	 *
	 * @return RedBean_OODBBean $bean
	 */
	public static function findLast($type, $sql = null, $values = array()) {
		return self::$finder->findLast($type, $sql, $values);
	}
	/**
	 * Returns an array of beans. Pass a type and a series of ids and
	 * this method will bring you the correspondig beans.
	 *
	 * important note: Because this method loads beans using the load()
	 * function (but faster) it will return empty beans with ID 0 for
	 * every bean that could not be located. The resulting beans will have the
	 * passed IDs as their keys.
	 *
	 * @param string $type type of beans
	 * @param array  $ids  ids to load
	 *
	 * @return array $beans resulting beans (may include empty ones)
	 */
	public static function batch($type, $ids) {
		return self::$redbean->batch($type, $ids);
	}
	/**
	 * Convenience function to execute Queries directly.
	 * Executes SQL.
	 *
	 * @param string $sql	 sql    SQL query to execute
	 * @param array  $values values a list of values to be bound to query parameters
	 *
	 * @return integer $affected  number of affected rows
	 */
	public static function exec($sql, $values = array()) {
		return self::query('exec', $sql, $values);
	}
	/**
	 * Convenience function to execute Queries directly.
	 * Executes SQL.
	 *
	 * @param string $sql	 sql    SQL query to execute
	 * @param array  $values values a list of values to be bound to query parameters
	 *
	 * @return array $results
	 */
	public static function getAll($sql, $values = array()) {
		return self::query('get', $sql, $values);
	}
	/**
	 * Convenience function to execute Queries directly.
	 * Executes SQL.
	 *
	 * @param string $sql	 sql    SQL query to execute
	 * @param array  $values values a list of values to be bound to query parameters
	 *
	 * @return string $result scalar
	 */
	public static function getCell($sql, $values = array()) {
		return self::query('getCell', $sql, $values);
	}
	/**
	 * Convenience function to execute Queries directly.
	 * Executes SQL.
	 *
	 * @param string $sql	 sql    SQL query to execute
	 * @param array  $values values a list of values to be bound to query parameters
	 *
	 * @return array $results
	 */
	public static function getRow($sql, $values = array()) {
		return self::query('getRow', $sql, $values);
	}
	/**
	 * Convenience function to execute Queries directly.
	 * Executes SQL.
	 *
	 * @param string $sql	 sql    SQL query to execute
	 * @param array  $values values a list of values to be bound to query parameters
	 *
	 * @return array $results
	 */
	public static function getCol($sql, $values = array()) {
		return self::query('getCol', $sql, $values);
	}
	/**
	 * Internal Query function, executes the desired query. Used by
	 * all facade query functions. This keeps things DRY.
	 *
	 * @throws RedBean_Exception_SQL
	 *
	 * @param string $method desired query method (i.e. 'cell', 'col', 'exec' etc..)
	 * @param string $sql    the sql you want to execute
	 * @param array  $values array of values to be bound to query statement
	 *
	 * @return array $results results of query
	 */
	private static function query($method, $sql, $values) {
		if (!self::$redbean->isFrozen()) {
			try {
				$rs = RedBean_Facade::$adapter->$method($sql, $values);
			}catch(RedBean_Exception_SQL $e) {
				if(self::$writer->sqlStateIn($e->getSQLState(),
				array(
				RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_COLUMN,
				RedBean_QueryWriter::C_SQLSTATE_NO_SUCH_TABLE)
				)) {
					return array();
				} else {
					throw $e;
				}
			}
			return $rs;
		} else {
			return RedBean_Facade::$adapter->$method($sql, $values);
		}
	}
	/**
	 * Convenience function to execute Queries directly.
	 * Executes SQL.
	 * Results will be returned as an associative array. The first
	 * column in the select clause will be used for the keys in this array and
	 * the second column will be used for the values. If only one column is
	 * selected in the query, both key and value of the array will have the
	 * value of this field for each row.
	 *
	 * @param string $sql	 sql    SQL query to execute
	 * @param array  $values values a list of values to be bound to query parameters
	 *
	 * @return array $results
	 */
	public static function getAssoc($sql, $values = array()) {
		return self::query('getAssoc', $sql, $values);
	}
	/**
	 * Makes a copy of a bean. This method makes a deep copy
	 * of the bean.The copy will have the following features.
	 * - All beans in own-lists will be duplicated as well
	 * - All references to shared beans will be copied but not the shared beans themselves
	 * - All references to parent objects (_id fields) will be copied but not the parents themselves
	 * In most cases this is the desired scenario for copying beans.
	 * This function uses a trail-array to prevent infinite recursion, if a recursive bean is found
	 * (i.e. one that already has been processed) the ID of the bean will be returned.
	 * This should not happen though.
	 *
	 * Note:
	 * This function does a reflectional database query so it may be slow.
	 *
	 * @param RedBean_OODBBean $bean  bean to be copied
	 * @param array            $trail for internal usage, pass array()
	 * @param boolean          $pid   for internal usage
	 *
	 * @return array $copiedBean the duplicated bean
	 */
	public static function dup($bean, $trail = array(), $pid = false, $filters = array()) {
		self::$duplicationManager->setFilters($filters);
		return self::$duplicationManager->dup($bean, $trail, $pid);
	}
	/**
	 * Exports a collection of beans. Handy for XML/JSON exports with a
	 * Javascript framework like Dojo or ExtJS.
	 * What will be exported:
	 * - contents of the bean
	 * - all own bean lists (recursively)
	 * - all shared beans (not THEIR own lists)
	 *
	 * @param	array|RedBean_OODBBean $beans   beans to be exported
	 * @param	boolean                $parents whether you want parent beans to be exported
	 * @param   array                  $filters whitelist of types
	 *
	 * @return	array $array exported structure
	 */
	public static function exportAll($beans, $parents = false, $filters = array()) {
		return self::$duplicationManager->exportAll($beans, $parents, $filters);
	}
	/**
	 * @deprecated
	 * 
	 * @param array  $beans    beans
	 * @param string $property property
	 */
	public static function swap($beans, $property) {
		return self::$associationManager->swap($beans, $property);
	}
	/**
	 * Converts a series of rows to beans.
	 *
	 * @param string $type type
	 * @param array  $rows must contain an array of arrays.
	 *
	 * @return array $beans
	 */
	public static function convertToBeans($type, $rows) {
		return self::$redbean->convertToBeans($type, $rows);
	}
	/**
	 * Part of RedBeanPHP Tagging API.
	 * Tests whether a bean has been associated with one ore more
	 * of the listed tags. If the third parameter is TRUE this method
	 * will return TRUE only if all tags that have been specified are indeed
	 * associated with the given bean, otherwise FALSE.
	 * If the third parameter is FALSE this
	 * method will return TRUE if one of the tags matches, FALSE if none
	 * match.
	 *
	 * @param  RedBean_OODBBean $bean bean to check for tags
	 * @param  array            $tags list of tags
	 * @param  boolean          $all  whether they must all match or just some
	 *
	 * @return boolean $didMatch whether the bean has been assoc. with the tags
	 */
	public static function hasTag($bean, $tags, $all = false) {
		return self::$tagManager->hasTag($bean, $tags, $all);
	}
	/**
	 * Part of RedBeanPHP Tagging API.
	 * Removes all sepcified tags from the bean. The tags specified in
	 * the second parameter will no longer be associated with the bean.
	 *
	 * @param  RedBean_OODBBean $bean    tagged bean
	 * @param  array            $tagList list of tags (names)
	 *
	 * @return void
	 */
	public static function untag($bean, $tagList) {
		return self::$tagManager->untag($bean, $tagList);
	}
	/**
	 * Part of RedBeanPHP Tagging API.
	 * Tags a bean or returns tags associated with a bean.
	 * If $tagList is null or omitted this method will return a
	 * comma separated list of tags associated with the bean provided.
	 * If $tagList is a comma separated list (string) of tags all tags will
	 * be associated with the bean.
	 * You may also pass an array instead of a string.
	 *
	 * @param RedBean_OODBBean $bean    bean
	 * @param mixed				$tagList tags
	 *
	 * @return string $commaSepListTags
	 */
	public static function tag(RedBean_OODBBean $bean, $tagList = null) {
		return self::$tagManager->tag($bean, $tagList);
	}
	/**
	 * Part of RedBeanPHP Tagging API.
	 * Adds tags to a bean.
	 * If $tagList is a comma separated list of tags all tags will
	 * be associated with the bean.
	 * You may also pass an array instead of a string.
	 *
	 * @param RedBean_OODBBean  $bean    bean
	 * @param array				$tagList list of tags to add to bean
	 *
	 * @return void
	 */
	public static function addTags(RedBean_OODBBean $bean, $tagList) {
		return self::$tagManager->addTags($bean, $tagList);
	}
	/**
	 * Part of RedBeanPHP Tagging API.
	 * Returns all beans that have been tagged with one of the tags given.
	 *
	 * @param  $beanType type of bean you are looking for
	 * @param  $tagList  list of tags to match
	 *
	 * @return array
	 */
	public static function tagged($beanType, $tagList) {
		return self::$tagManager->tagged($beanType, $tagList);
	}
	/**
	 * Part of RedBeanPHP Tagging API.
	 * Returns all beans that have been tagged with ALL of the tags given.
	 *
	 * @param  $beanType type of bean you are looking for
	 * @param  $tagList  list of tags to match
	 *
	 * @return array
	 */
	public static function taggedAll($beanType, $tagList) {
		return self::$tagManager->taggedAll($beanType, $tagList);
	}
	/**
	 * Wipes all beans of type $beanType.
	 *
	 * @param string $beanType type of bean you want to destroy entirely.
	 */
	public static function wipe($beanType) {
		return RedBean_Facade::$redbean->wipe($beanType);
	}
	/**
	 * Counts beans
	 *
	 * @param string $beanType type of bean
	 * @param string $addSQL   additional SQL snippet (for filtering, limiting)
	 * @param array  $params   parameters to bind to SQL
	 *
	 * @return integer $numOfBeans
	 */
	public static function count($beanType, $addSQL = '', $params = array()) {
		return RedBean_Facade::$redbean->count($beanType, $addSQL, $params);
	}
	/**
	 * Configures the facade, want to have a new Writer? A new Object Database or a new
	 * Adapter and you want it on-the-fly? Use this method to hot-swap your facade with a new
	 * toolbox.
	 *
	 * @param RedBean_ToolBox $tb toolbox
	 *
	 * @return RedBean_ToolBox $tb old, rusty, previously used toolbox
	 */
	public static function configureFacadeWithToolbox(RedBean_ToolBox $tb) {
		$oldTools = self::$toolbox;
		self::$toolbox = $tb;
		self::$writer = self::$toolbox->getWriter();
		self::$adapter = self::$toolbox->getDatabaseAdapter();
		self::$redbean = self::$toolbox->getRedBean();
		self::$finder = new RedBean_Finder(self::$toolbox);
		self::$associationManager = new RedBean_AssociationManager(self::$toolbox);
		self::$redbean->setAssociationManager(self::$associationManager);
		self::$labelMaker = new RedBean_LabelMaker(self::$toolbox);
		self::$extAssocManager = new RedBean_AssociationManager_ExtAssociationManager(self::$toolbox);
		$helper = new RedBean_ModelHelper();
		$helper->attachEventListeners(self::$redbean);
		self::$associationManager->addEventListener('delete', $helper);
		self::$duplicationManager = new RedBean_DuplicationManager(self::$toolbox);
		self::$tagManager = new RedBean_TagManager(self::$toolbox);
		self::$f = new RedBean_SQLHelper(self::$adapter);
		return $oldTools;
	}
	/**
	 * Facade Convience method for adapter transaction system.
	 * Begins a transaction.
	 *
	 * @return void
	 */
	public static function begin() {
		if (!self::$redbean->isFrozen()) return false;
		self::$adapter->startTransaction();
		return true;
	}
	/**
	 * Facade Convience method for adapter transaction system.
	 * Commits a transaction.
	 *
	 * @return void
	 */
	public static function commit() {
		if (!self::$redbean->isFrozen()) return false;
		self::$adapter->commit();
		return true;
	}
	/**
	 * Facade Convience method for adapter transaction system.
	 * Rolls back a transaction.
	 *
	 * @return void
	 */
	public static function rollback() {
		if (!self::$redbean->isFrozen()) return false;
		self::$adapter->rollback();
		return true;
	}
	/**
	 * Returns a list of columns. Format of this array:
	 * array( fieldname => type )
	 * Note that this method only works in fluid mode because it might be
	 * quite heavy on production servers!
	 *
	 * @param  string $table   name of the table (not type) you want to get columns of
	 *
	 * @return array  $columns list of columns and their types
	 */
	public static function getColumns($table) {
		return self::$writer->getColumns($table);
	}
	/**
	 * Generates question mark slots for an array of values.
	 *
	 * @param array $array
	 * @return string $slots
	 */
	public static function genSlots($array) {
		return self::$f->genSlots($array);
	}
	/**
	 * Nukes the entire database.
	 */
	public static function nuke() {
		if (!self::$redbean->isFrozen()) {
			self::$writer->wipeAll();
		}
	}
	/**
	 * Sets a list of dependencies.
	 * A dependency list contains an entry for each dependent bean.
	 * A dependent bean will be removed if the relation with one of the
	 * dependencies gets broken.
	 *
	 * Example:
	 *
	 * array(
	 *	'page' => array('book', 'magazine')
	 * )
	 *
	 * A page will be removed if:
	 *
	 * unset($book->ownPage[$pageID]);
	 *
	 * or:
	 *
	 * unset($magazine->ownPage[$pageID]);
	 *
	 * but not if:
	 *
	 * unset($paper->ownPage[$pageID]);
	 *
	 *
	 * @param array $dep list of dependencies
	 */
	public static function dependencies($dep) {
		self::$redbean->setDepList($dep);
	}
	/**
	 * Short hand function to store a set of beans at once, IDs will be
	 * returned as an array. For information please consult the R::store()
	 * function.
	 * A loop saver.
	 *
	 * @param array $beans list of beans to be stored
	 *
	 * @return array $ids list of resulting IDs
	 */
	public static function storeAll($beans) {
		$ids = array();
		foreach($beans as $bean) $ids[] = self::store($bean);
		return $ids;
	}
	/**
	 * Short hand function to trash a set of beans at once.
	 * For information please consult the R::trash() function.
	 * A loop saver.
	 *
	 * @param array $beans list of beans to be trashed
	 */
	public static function trashAll($beans) {
		foreach($beans as $bean) self::trash($bean);
	}
	/**
	 * A label is a bean with only an id, type and name property.
	 * This function will dispense beans for all entries in the array. The
	 * values of the array will be assigned to the name property of each
	 * individual bean.
	 *
	 * @param string $type   type of beans you would like to have
	 * @param array  $labels list of labels, names for each bean
	 *
	 * @return array $bean a list of beans with type and name property
	 */
	public static function dispenseLabels($type, $labels) {
		return self::$labelMaker->dispenseLabels($type, $labels);
	}
	/**
	 * Gathers labels from beans. This function loops through the beans,
	 * collects the values of the name properties of each individual bean
	 * and stores the names in a new array. The array then gets sorted using the
	 * default sort function of PHP (sort).
	 *
	 * @param array $beans list of beans to loop
	 *
	 * @return array $array list of names of beans
	 */
	public static function gatherLabels($beans) {
		return self::$labelMaker->gatherLabels($beans);
	}
	/**
	 * Closes the database connection.
	 */
	public static function close() {
		if (isset(self::$adapter)) self::$adapter->close();
	}
	/**
	 * Simple convenience function, returns ISO date formatted representation
	 * of $time.
	 *
	 * @param mixed $time UNIX timestamp
	 *
	 * @return type
	 */
	public static function isoDate($time = null) {
		if (!$time) $time = time();
		return @date('Y-m-d', $time);
	}
	/**
	 * Simple convenience function, returns ISO date time
	 * formatted representation
	 * of $time.
	 *
	 * @param mixed $time UNIX timestamp
	 *
	 * @return type
	 */
	public static function isoDateTime($time = null) {
		if (!$time) $time = time();
		return @date('Y-m-d H:i:s', $time);
	}
	/**
	 * Optional accessor for neat code.
	 * Sets the database adapter you want to use.
	 * 
	 * @param RedBean_Adapter $adapter 
	 */
	public static function setDatabaseAdapter(RedBean_Adapter $adapter) {
		self::$adapter = $adapter;
	}
	/**
	 * Optional accessor for neat code.
	 * Sets the database adapter you want to use.
	 *
	 * @param RedBean_QueryWriter $writer 
	 */
	public static function setWriter(RedBean_QueryWriter $writer) {
		self::$writer = $writer;
	}
	/**
	 * Optional accessor for neat code.
	 * Sets the database adapter you want to use.
	 *
	 * @param RedBean_OODB $redbean 
	 */
	public static function setRedBean(RedBean_OODB $redbean) {
		self::$redbean = $redbean;
	}
	/**
	 * Optional accessor for neat code.
	 * Sets the database adapter you want to use.
	 *
	 * @return RedBean_DatabaseAdapter $adapter
	 */
	public static function getDatabaseAdapter() {
		return self::$adapter;
	}
	/**
	 * Optional accessor for neat code.
	 * Sets the database adapter you want to use.
	 *
	 * @return RedBean_QueryWriter $writer
	 */
	public static function getWriter() {
		return self::$writer;
	}
	/**
	 * Optional accessor for neat code.
	 * Sets the database adapter you want to use.
	 *
	 * @return RedBean_RedBean $redbean
	 */
	public static function getRedBean() {
		return self::$redbean;
	}
	/**
	 * Preloads certain properties for beans.
	 * Understands aliases.
	 * 
	 * Usage: R::preload($books, array('coauthor'=>'author'));
	 * 
	 * @param array $beans beans
	 * @param array $types types to load
	 */
	public static function preload($beans, $types, $closure = null) {
		return self::$redbean->preload($beans, $types, $closure);
	}
	//Alias for Preload.
	public static function each($beans, $types, $closure = null) { 
		return self::preload($beans, $types, $closure); 
	}
	/**
	 * Facade method for RedBean_QueryWriter_AQueryWriter::renameAssocation()
	 * 
	 * @param string|array $from
	 * @param string $to 
	 */
	public static function renameAssociation($from, $to = null) { 
		RedBean_QueryWriter_AQueryWriter::renameAssociation($from, $to); 
	}
}
//Compatibility with PHP 5.2 and earlier
if (!function_exists('lcfirst')) {
	function lcfirst($str){ return (string)(strtolower(substr($str, 0, 1)).substr($str, 1)); }
}

interface RedBean_Plugin { }; 

class RedBean_Plugin_BeanCan implements RedBean_Plugin {
	/**
	 * @var RedBean_ModelHelper
	 */
	private $modelHelper;
	/**
	 * Constructor.
	 */
	public function __construct() {
		$this->modelHelper = new RedBean_ModelHelper;
	}
	/**
	 * Writes a response object for the client (JSON encoded). Internal method.
	 *
	 * @param mixed   $result       result
	 * @param integer $id           request ID
	 * @param integer $errorCode    error code from server
	 * @param string  $errorMessage error message from server
	 *
	 * @return string $json JSON encoded response.
	 */
	private function resp($result = null, $id = null, $errorCode = '-32603', $errorMessage = 'Internal Error') {
		$response = array('jsonrpc' => '2.0');
		if (!is_null($id)) { $response['id'] = $id; }
		if ($result) {
			$response['result'] = $result;
		} else {
			$response['error'] = array('code' => $errorCode, 'message' => $errorMessage);
		}
		return (json_encode($response));
	}
	/**
	 * Processes a JSON object request.
	 *
	 * @param array $jsonObject JSON request object
	 *
	 * @return mixed $result result
	 */
	public function handleJSONRequest($jsonString) {
		//Decode JSON string
		$jsonArray = json_decode($jsonString, true);
		if (!$jsonArray) return $this->resp(null, null, -32700, 'Cannot Parse JSON');
		if (!isset($jsonArray['jsonrpc'])) return $this->resp(null, null, -32600, 'No RPC version');
		if (($jsonArray['jsonrpc'] != '2.0')) return $this->resp(null, null, -32600, 'Incompatible RPC Version');
		//DO we have an ID to identify this request?
		if (!isset($jsonArray['id'])) return $this->resp(null, null, -32600, 'No ID');
		//Fetch the request Identification String.
		$id = $jsonArray['id'];
		//Do we have a method?
		if (!isset($jsonArray['method'])) return $this->resp(null, $id, -32600, 'No method');
		//Do we have params?
		if (!isset($jsonArray['params'])) {
			$data = array();
		} else {
			$data = $jsonArray['params'];
		}
		//Check method signature
		$method = explode(':', trim($jsonArray['method']));
		if (count($method) != 2) {
			return $this->resp(null, $id, -32600, 'Invalid method signature. Use: BEAN:ACTION');
		}
		//Collect Bean and Action
		$beanType = $method[0];
		$action = $method[1];
		//May not contain anything other than ALPHA NUMERIC chars and _
		if (preg_match('/\W/', $beanType)) return $this->resp(null, $id, -32600, 'Invalid Bean Type String');
		if (preg_match('/\W/', $action)) return $this->resp(null, $id, -32600, 'Invalid Action String');
		try {
			switch($action) {
				case 'store':
					if (!isset($data[0])) return $this->resp(null, $id, -32602, 'First param needs to be Bean Object');
					$data = $data[0];
					if (!isset($data['id'])) $bean = RedBean_Facade::dispense($beanType); else $bean = RedBean_Facade::load($beanType, $data['id']);
					$bean->import($data);
					$rid = RedBean_Facade::store($bean);
					return $this->resp($rid, $id);
				case 'load':
					if (!isset($data[0])) return $this->resp(null, $id, -32602, 'First param needs to be Bean ID');
					$bean = RedBean_Facade::load($beanType, $data[0]);
					return $this->resp($bean->export(), $id);
				case 'trash':
					if (!isset($data[0])) return $this->resp(null, $id, -32602, 'First param needs to be Bean ID');
					$bean = RedBean_Facade::load($beanType, $data[0]);
					RedBean_Facade::trash($bean);
					return $this->resp('OK', $id);
				case 'export':
					if (!isset($data[0])) return $this->resp(null, $id, -32602, 'First param needs to be Bean ID');
					$bean = RedBean_Facade::load($beanType, $data[0]);
					$array = RedBean_Facade::exportAll(array($bean), true);
					return $this->resp($array, $id);
				default:
					$modelName = $this->modelHelper->getModelName($beanType);
					if (!class_exists($modelName)) return $this->resp(null, $id, -32601, 'No such bean in the can!');
					$beanModel = new $modelName;
					if (!method_exists($beanModel, $action)) return $this->resp(null, $id, -32601, "Method not found in Bean: $beanType ");
					return $this->resp(call_user_func_array(array($beanModel, $action), $data), $id);
			}
		} catch(Exception $exception) {
			return $this->resp(null, $id, -32099, $exception->getCode().'-'.$exception->getMessage());
		}
	}
	/**
	 * Support for RESTFul GET-requests.
	 * Only supports very BASIC REST requests, for more functionality please use
	 * the JSON-RPC 2 interface.
	 * 
	 * @param string $pathToResource RESTFul path to resource
	 * 
	 * @return string $json a JSON encoded response ready for sending to client
	 */
	public function handleRESTGetRequest($pathToResource) {
		if (!is_string($pathToResource)) return $this->resp(null, 0, -32099, 'IR');
		$resourceInfo = explode('/', $pathToResource);
		$type = $resourceInfo[0];
		try {
			if (count($resourceInfo) < 2) {
				return $this->resp(RedBean_Facade::findAndExport($type));
			} else {
				$id = (int) $resourceInfo[1];
				return $this->resp(RedBean_Facade::load($type, $id)->export(), $id);
			}
		} catch(Exception $e) {
			return $this->resp(null, 0, -32099);
		}
	}
}

class RedBean_Plugin_QueryLogger implements RedBean_Observer, RedBean_Plugin {
	/**
	 * @var array
	 */
	protected $logs = array();
	/**
	 * Creates a new instance of the Query Logger and attaches
	 * this logger to the adapter.
	 *
	 * @static
	 * @param RedBean_Observable $adapter the adapter you want to attach to
	 *
	 * @return RedBean_Plugin_QueryLogger $querylogger instance of the Query Logger
	 */
	public static function getInstanceAndAttach(RedBean_Observable $adapter) {
		$queryLog = new RedBean_Plugin_QueryLogger;
		$adapter->addEventListener('sql_exec', $queryLog);
		return $queryLog;
	}
	/**
	 * Singleton pattern
	 * Constructor - private
	 */
	private function __construct(){}
	/**
	 * Implementation of the onEvent() method for Observer interface.
	 * If a query gets executed this method gets invoked because the
	 * adapter will send a signal to the attached logger.
	 *
	 * @param  string $eventName          ID of the event (name)
	 * @param  RedBean_DBAdapter $adapter adapter that sends the signal
	 *
	 * @return void
	 */
	public function onEvent($eventName, $adapter) {
		if ($eventName == 'sql_exec') {
			$this->logs[] = $adapter->getSQL();
		}
	}
	/**
	 * Searches the logs for the given word and returns the entries found in
	 * the log container.
	 *
	 * @param  string $word word to look for
	 *
	 * @return array $entries entries that contain the keyword
	 */
	public function grep($word) {
		$found = array();
		foreach($this->logs as $log) {
			if (strpos($log, $word) !== false) {
				$found[] = $log;
			}
		}
		return $found;
	}
	/**
	 * Returns all the logs.
	 *
	 * @return array $logs logs
	 */
	public function getLogs() { return $this->logs; }
	/**
	 * Clears the logs.
	 *
	 * @return void
	 */
	public function clear() { $this->logs = array();}
}

class RedBean_Plugin_Cooker implements RedBean_Plugin {
	/**
	 * @var boolean
	 */
	private static $loadBeans = false;
	/**
	 * @var boolean
	 */
	private static $useNULLForEmptyString = false;
	/**
	 * If you enable bean loading graph will load beans if there is an ID in the array.
	 * This is very powerful but can also cause security issues if a user knows how to
	 * manipulate beans and there is no model based ID validation.
	 * 
	 * @param boolean $yesNo 
	 */
	public static function enableBeanLoading($yesNo) {
		self::$loadBeans = ($yesNo);
	}
	/**
	 * Sets the toolbox to be used by graph()
	 *
	 * @param RedBean_Toolbox $toolbox toolbox
	 * @return void
	 */
	public function setToolbox(RedBean_Toolbox $toolbox) {
		$this->toolbox = $toolbox;
		$this->redbean = $this->toolbox->getRedbean();
	}
	/**
	 * Turns an array (post/request array) into a collection of beans.
	 * Handy for turning forms into bean structures that can be stored with a
	 * single call.
	 * 
	 * Typical usage:
	 * 
	 * $struct = R::graph($_POST);
	 * R::store($struct);
	 * 
	 * Example of a valid array:
	 * 
	 *	$form = array(
	 *		'type' => 'order',
	 *		'ownProduct' => array(
	 *			array('id' => 171, 'type' => 'product'),
	 *		),
	 *		'ownCustomer' => array(
	 *			array('type' => 'customer', 'name' => 'Bill')
	 *		),
	 * 		'sharedCoupon' => array(
	 *			array('type' => 'coupon', 'name' => '123'),
	 *			array('type' => 'coupon', 'id' => 3)
	 *		)
	 *	);
	 * 
	 * Each entry in the array will become a property of the bean.
	 * The array needs to have a type-field indicating the type of bean it is
	 * going to be. The array can have nested arrays. A nested array has to be
	 * named conform the bean-relation conventions, i.e. ownPage/sharedPage
	 * each entry in the nested array represents another bean.
	 *  
	 * @param	array   $array       array to be turned into a bean collection
	 * @param   boolean $filterEmpty whether you want to exclude empty beans
	 *
	 * @return	array $beans beans
	 */
	public function graph($array, $filterEmpty = false) {
      	$beans = array();
		if (is_array($array) && isset($array['type'])) {
			$type = $array['type'];
			unset($array['type']);
			//Do we need to load the bean?
			if (isset($array['id'])) {
				if (self::$loadBeans) {
					$id = (int) $array['id'];
					$bean = $this->redbean->load($type, $id);
				} else {
					throw new RedBean_Exception_Security('Attempt to load a bean in Cooker. Use enableBeanLoading to override but please read security notices first.');
				}
			} else {
				$bean = $this->redbean->dispense($type);
			}
			foreach($array as $property => $value) {
				if (is_array($value)) {
					$bean->$property = $this->graph($value, $filterEmpty);
				} else {
					if($value == '' && self::$useNULLForEmptyString){
						$bean->$property = null;
                    } else $bean->$property = $value;
				}
			}
			return $bean;
		} elseif (is_array($array)) {
			foreach($array as $key => $value) {
				$listBean = $this->graph($value, $filterEmpty);
				if (!($listBean instanceof RedBean_OODBBean)) {
					throw new RedBean_Exception_Security('Expected bean but got :'.gettype($listBean)); 
				}
				if ($listBean->isEmpty()) {  
					if (!$filterEmpty) { 
						$beans[$key] = $listBean;
					}
				} else { 
					$beans[$key] = $listBean;
				}
			}
			return $beans;
		} else {
			throw new RedBean_Exception_Security('Expected array but got :'.gettype($array)); 
		}
	}
	/**
	 * Toggles the use-NULL flag.
	 *  
	 * @param boolean $yesNo 
	 */
	public function setUseNullFlag($yesNo) {
		self::$useNULLForEmptyString = (boolean) $yesNo;
	}
}


class RedBean_Plugin_Cache extends RedBean_OODB implements RedBean_Plugin {
	/**
	 * @var array 
	 */
	protected $cache = array();
	/**
	 * @var integer 
	 */
	protected $hits = 0;
	/**
	 * @var integer 
	 */
	protected $misses = 0;
	/**
	 * Constructor.
	 * Cache decorates RedBeanPHP OODB class, so needs a writer.
	 * 
	 * @param RedBean_QueryWriter $writer 
	 */
	public function __construct(RedBean_QueryWriter $writer) {
		parent::__construct($writer);
	}
	/**
	 * Loads a bean by type and id. If the bean cannot be found an
	 * empty bean will be returned instead. This is a cached version
	 * of the loader, if the bean has been cached it will be served
	 * from cache, otherwise the bean will be retrieved from the database
	 * as usual an a new cache entry will be added..
	 * 
	 * @param string  $type type of bean you are looking for
	 * @param integer $id   identifier of the bean
	 * 
	 * @return RedBean_OODBBean $bean the bean object found
	 */
	public function load($type, $id) {
		if (isset($this->cache[$type][$id])) {
			$this->hits ++;
			$bean = $this->cache[$type][$id];
		} else {
			$this->misses ++;
			$bean = parent::load($type, $id);
			if ($bean->id) {
				if (!isset($this->cache[$type])) $this->cache[$type] = array();
				$this->cache[$type][$id] = $bean;
			}
		}
		return $bean;
	}
	/**
	 * Stores a RedBean OODBBean and caches it.
	 * 
	 * @param RedBean_OODBBean $bean the bean you want to store
	 * 
	 * @return integer $id 
	 */
	public function store($bean) {
		$id = parent::store($bean);
		$type = $bean->getMeta('type');
		if (!isset($this->cache[$type])) $this->cache[$type] = array();
		$this->cache[$type][$id] = $bean;
		return $id;
	}
	/**
	 * Trashes a RedBean OODBBean and removes it from cache.
	 * 
	 * @param RedBean_OODBBean $bean bean
	 * @return mixed 
	 */
	public function trash($bean) {
		$type = $bean->getMeta('type');
		$id = $bean->id;
		if (isset($this->cache[$type][$id])) unset($this->cache[$type][$id]);
		return parent::trash($bean);
	}
	/**
	 * Flushes the cache for a given type.
	 * 
	 * @param string $type
	 * 
	 * @return RedBean_Plugin_Cache 
	 */
	public function flush($type) {
		if (isset($this->cache[$type])) $this->cache[$type] = array();
		return $this;
	}
	/**
	 * Flushes the cache completely.
	 * 
	 * @return RedBean_Plugin_Cache 
	 */
	public function flushAll() {
		$this->cache = array();
		return $this;
	}
	/**
	 * Returns the number of hits. If a call to load() or
	 * batch() can use the cache this counts as a hit.
	 * Otherwise it's a miss.
	 * 
	 * @return integer 
	 */
	public function getHits() {
		return $this->hits;
	}
	/**
	 * Returns the number of hits. If a call to load() or
	 * batch() can use the cache this counts as a hit.
	 * Otherwise it's a miss.
	 * 
	 * @return integer 
	 */
	public function getMisses() {
		return $this->misses;
	}
	/**
	 * Resets hits counter to 0.
	 */
	public function resetHits() {
		$this->hits = 0;
	}
	/**
	 * Resets misses counter to 0.
	 */
	public function resetMisses() {
		$this->misses = 0;
	}
}

class RedBean_DependencyInjector {
	/**
	 * @var array 
	 */
	protected $dependencies = array();
	/**
	 * Adds a dependency to the list.
	 * You can add dependencies using this method. Pass both the key of the
	 * dependency and the dependency itself. The key of the dependency is a 
	 * name that should match the setter. For instance if you have a dependency
	 * class called My_Mailer and a setter on the model called setMailSystem
	 * you should pass an instance of My_Mailer with key MailSystem.
	 * The injector will now look for a setter called setMailSystem.
	 * 
	 * @param string $dependencyID name of the dependency (should match setter)
	 * @param mixed  $dependency   the service to be injected
	 */
	public function addDependency($dependencyID, $dependency) {
		$this->dependencies[$dependencyID] = $dependency;
	}
	/**
	 * Returns an instance of the class $modelClassName completely
	 * configured as far as possible with all the available
	 * service objects in the dependency list.
	 * 
	 * @param string $modelClassName the name of the class of the model
	 * 
	 * @return mixed $object the model/object
	 */
	public function getInstance($modelClassName) {
		$object = new $modelClassName;
		if ($this->dependencies && is_array($this->dependencies)) {
			foreach($this->dependencies as $key => $dep) {
				$depSetter = 'set'.$key;
				if (method_exists($object, $depSetter)) {
					$object->$depSetter($dep);
				}
			}
		}
		return $object;
	}
}

class RedBean_DuplicationManager {
	/**
	 * @var RedBean_Toolbox
	 */
	protected $toolbox;
	/**
	 * @var RedBean_AssociationManager
	 */
	protected $associationManager;
	/**
	 * @var RedBean_OODB
	 */
	protected $redbean;
	/**
	 * @var array
	 */
	protected $tables = array();
	/**
	 * @var array
	 */
	protected $columns = array();
	/**
	 * @var array
	 */
	protected $filters = array();
	/**
	 * @var array
	 */
	protected $cacheTables = false;
	/**
	 * Constructor,
	 * creates a new instance of DupManager.
	 * @param RedBean_Toolbox $toolbox 
	 */
	public function __construct(RedBean_Toolbox $toolbox) {
		$this->toolbox = $toolbox;
		$this->redbean = $toolbox->getRedBean();
		$this->associationManager = $this->redbean->getAssociationManager();
	}
	/**
	 * For better performance you can pass the tables in an array to this method.
	 * If the tables are available the duplication manager will not query them so
	 * this might be beneficial for performance.
	 * 
	 * @param array $tables 
	 */
	public function setTables($tables) {
		foreach($tables as $key => $value) {
			if (is_numeric($key)) {
				$this->tables[] = $value;
			} else {
				$this->tables[] = $key;
				$this->columns[$key] = $value;
			}
		}
		$this->cacheTables = true;
	}
	/**
	 * Returns a schema array for cache.
	 * 
	 * @return array 
	 */
	public function getSchema() {
		return $this->columns;
	}
	/**
	 * Indicates whether you want the duplication manager to cache the database schema.
	 * If this flag is set to TRUE the duplication manager will query the database schema
	 * only once. Otherwise the duplicationmanager will, by default, query the schema
	 * every time a duplication action is performed (dup()).
	 * 
	 * @param boolean $yesNo 
	 */
	public function setCacheTables($yesNo) {
		$this->cacheTables = $yesNo;
	}
	/**
	 * A filter array is an array with table names.
	 * By setting a table filter you can make the duplication manager only take into account
	 * certain bean types. Other bean types will be ignored when exporting or making a
	 * deep copy. If no filters are set all types will be taking into account, this is
	 * the default behavior.
	 * 
	 * @param array $filters 
	 */
	public function setFilters($filters) {
		$this->filters = $filters;
	}
	/**
	 * Determines whether the bean has an own list based on
	 * schema inspection from realtime schema or cache.
	 * 
	 * @param string $type   bean type
	 * @param string $target type of list you want to detect
	 * 
	 * @return boolean 
	 */
	protected function hasOwnList($type, $target) {
		return (isset($this->columns[$target][$type.'_id']));
	}
	/**
	 * Determines whether the bea has a shared list based on
	 * schema inspection from realtime schema or cache.
	 * 
	 * @param string $type   bean type
	 * @param string $target type of list you are looking for
	 * 
	 * @return boolean 
	 */
	protected function hasSharedList($type, $target) {
		return (in_array(RedBean_QueryWriter_AQueryWriter::getAssocTableFormat(array($type, $target)), $this->tables));
	}
	/**
	 * Makes a copy of a bean. This method makes a deep copy
	 * of the bean.The copy will have the following features.
	 * - All beans in own-lists will be duplicated as well
	 * - All references to shared beans will be copied but not the shared beans themselves
	 * - All references to parent objects (_id fields) will be copied but not the parents themselves
	 * In most cases this is the desired scenario for copying beans.
	 * This function uses a trail-array to prevent infinite recursion, if a recursive bean is found
	 * (i.e. one that already has been processed) the ID of the bean will be returned.
	 * This should not happen though.
	 *
	 * Note:
	 * This function does a reflectional database query so it may be slow.
	 *
	 * Note:
	 * this function actually passes the arguments to a protected function called
	 * duplicate() that does all the work. This method takes care of creating a clone
	 * of the bean to avoid the bean getting tainted (triggering saving when storing it).
	 * 
	 * @param RedBean_OODBBean $bean  bean to be copied
	 * @param array            $trail for internal usage, pass array()
	 * @param boolean          $pid   for internal usage
	 *
	 * @return array $copiedBean the duplicated bean
	 */
	public function dup($bean, $trail = array(), $pid = false) {
		if (!count($this->tables))  $this->tables = $this->toolbox->getWriter()->getTables();
		if (!count($this->columns)) foreach($this->tables as $table) $this->columns[$table] = $this->toolbox->getWriter()->getColumns($table);
		$beanCopy = clone($bean);
		$rs = $this->duplicate($beanCopy, $trail, $pid);
		if (!$this->cacheTables) {
			$this->tables = array();
			$this->columns = array();
		}
		return $rs;
	}
	/**
	 * @see RedBean_DuplicationManager::dup
	 *
	 * @param RedBean_OODBBean $bean  bean to be copied
	 * @param array            $trail trail to prevent infinite loops
	 * @param boolean          $pid   preserve IDs
	 *
	 * @return array $copiedBean the duplicated bean
	 */
	protected function duplicate($bean, $trail = array(), $pid = false) {
		$type = $bean->getMeta('type');
		$key = $type.$bean->getID();
		if (isset($trail[$key])) return $bean;
		$trail[$key] = $bean;
		$copy = $this->redbean->dispense($type);
		$copy->importFrom($bean);
		$copy->id = 0;
		$tables = $this->tables;
		foreach($tables as $table) {
			if (is_array($this->filters) && count($this->filters) && !in_array($table, $this->filters)) continue;
			if ($table == $type) continue;
			$owned = 'own'.ucfirst($table);
			$shared = 'shared'.ucfirst($table);
			if ($this->hasSharedList($type, $table)) {
				if ($beans = $bean->$shared) {
					$copy->$shared = array();
					foreach($beans as $subBean) {
						array_push($copy->$shared, $subBean);
					}
				}
			} elseif ($this->hasOwnList($type, $table)) {
				if ($beans = $bean->$owned) {
					$copy->$owned = array();
					foreach($beans as $subBean) {
						array_push($copy->$owned, $this->duplicate($subBean, $trail, $pid));
					}
				}
				$copy->setMeta('sys.shadow.'.$owned, null);
			}
			$copy->setMeta('sys.shadow.'.$shared, null);
		}
		if ($pid) $copy->id = $bean->id;
		return $copy;
	}
	/**
	 * Exports a collection of beans. Handy for XML/JSON exports with a
	 * Javascript framework like Dojo or ExtJS.
	 * What will be exported:
	 * - contents of the bean
	 * - all own bean lists (recursively)
	 * - all shared beans (not THEIR own lists)
	 *
	 * @param	array|RedBean_OODBBean $beans   beans to be exported
	 * @param   boolean				   $parents also export parents
	 * @param   array                  $filters only these types (whitelist)
	 * 
	 * @return	array $array exported structure
	 */
	public function exportAll($beans, $parents = false, $filters = array()) {
		$array = array();
		if (!is_array($beans)) $beans = array($beans);
		foreach($beans as $bean) {
			   $this->setFilters($filters);
			   $f = $this->dup($bean, array(), true);
			   $array[] = $f->export(false, $parents, false, $filters);
		}
		return $array;
	}
}

class R extends RedBean_Facade{
  			public static function graph($array, $filterEmpty=false) { $c = new RedBean_Plugin_Cooker(); $c->setToolbox(self::$toolbox);return $c->graph($array, $filterEmpty);} 
}
