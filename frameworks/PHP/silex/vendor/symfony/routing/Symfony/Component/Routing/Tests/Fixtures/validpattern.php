<?php
use Symfony\Component\Routing\RouteCollection;
use Symfony\Component\Routing\Route;

$collection = new RouteCollection();
$collection->add('blog_show', new Route(
    '/blog/{slug}',
    array('_controller' => 'MyBlogBundle:Blog:show'),
    array('locale' => '\w+'),
    array('compiler_class' => 'RouteCompiler'),
    '{locale}.example.com',
    array('https'),
    array('GET','POST','put','OpTiOnS')
));
$collection->add('blog_show_legacy', new Route(
    '/blog/{slug}',
    array('_controller' => 'MyBlogBundle:Blog:show'),
    array('_method' => 'GET|POST|put|OpTiOnS', '_scheme' => 'https', 'locale' => '\w+',),
    array('compiler_class' => 'RouteCompiler'),
    '{locale}.example.com'
));

return $collection;
