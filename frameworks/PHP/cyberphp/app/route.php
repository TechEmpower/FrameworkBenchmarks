<?php
use Cyber\Request;

return [
    ['/', 'GET', 'app\controller\Index@hello'],
    ['/json', 'GET', 'app\controller\Index@json'],
    ['/plaintext', 'GET', 'app\controller\Index@plaintext'],
    ['/db', 'GET', 'app\controller\Index@db'],
    ['/fortunes', 'GET', 'app\controller\Index@fortunes'],
    ['/queries/{q}', 'GET', 'app\controller\Index@queries'],
    ['/updates/{q}', 'GET', 'app\controller\Index@updates'],
];