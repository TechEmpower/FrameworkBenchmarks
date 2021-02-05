<?php
//URLs routing
$urlpatterns = [
  '/' => urls('app/urls.php'),
];

//Error handler
$errorhandlers = [
  '404' => 'view.page_not_found',
  '403' => 'view.forbidden',
  '500' => 'view.internal_server_error',
];
