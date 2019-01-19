<?php

class JsonController extends AppController
{

    public function index()
    {
        View::select(null, null);
        header('Content-type: application/json');
        echo json_encode(['message' => 'Hello, World!']);
    }
}
