<?php
namespace Apps\Modules\Admin\Controllers;

use Cygnite\Mvc\View\Widget;
use Cygnite\Foundation\Application;
use Apps\Modules\Admin\Models\User;
use Cygnite\Mvc\Controller\AbstractBaseController;

class UserController extends AbstractBaseController
{
     /*
     * Your constructor.
     * @access public
     *
     */
    public function __construct()
    {
        parent::__construct();

    }

    /**
     * Default method for your controller. Render widget and return
     * @access public
     *
     */
   public function indexAction($id)
   {
        $users = array();
        //$users = User::all();
       return Widget::make('admin:user', function ($widget)
       {
           return $widget->render(true); // If you pass true Widget will understand you are trying to access widget

       }, array('greet' => 'Hello! Widget'));
   }
}//End of your controller
