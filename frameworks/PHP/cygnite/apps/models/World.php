<?php 
namespace Apps\Models;

use Cygnite\Database\ActiveRecord;

class World extends ActiveRecord
{
    //your database connection name
    protected $database = 'hello_world';

    /*
     | By default Every model class name used as table name
     | "User" => 'user'
     | You can also override the table name here
     */
    //protected $tableName = 'users';

    protected $primaryKey = 'id';

    public function __construct()
    {
        parent::__construct();
    }
}// End of the User Model