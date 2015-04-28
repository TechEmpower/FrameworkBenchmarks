<?php
namespace Apps\Modules\Admin\Models;

use Cygnite\Application;
use Cygnite\Database\Schema;
use Cygnite\Common\UrlManager\Url;
use Cygnite\Database\ActiveRecord;

class User extends ActiveRecord
{
    //your database connection name
    protected $database = 'cygnite';

    protected $primaryKey = 'id';

    public function __construct()
    {
        parent::__construct();
    }
}// End of the User Model