<?php
//Include views
require_once 'view.php';

//URLs routing
$urlpatterns = [
  '/plaintext' => 'view.plaintext',
  '/json' => 'view.json',
  '/queries/{queries}' => 'view.queries',
  '/updates/{queries}' => 'view.updates',
  '/fortunes' => 'view.fortunes',
];
