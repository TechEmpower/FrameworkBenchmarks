<?php declare(strict_types=1);
// use MY\Base\Helper\ViewHelper as V;
    
?>
<!doctype html>
<html>
<head>
  <meta charset="utf-8">
  <title>Hello DuckPHP!</title>
</head>
<body>
<h1>Hello DuckPHP</h1>
Now is [<?=$var?>]
<hr/>
<div>
    欢迎使用 DuckPHP ,<?php echo $var;?>
    <a href="<?=__url('test/done')?>">查看 Demo 结果</a>
</div>
<hr />
<div>
所有例子
<ul>
    <li><a href="/demo.php"> demo.php 单一文件演示所有操作</a>
    <li><a href="/helloworld.php"> helloworld.php 常见的 helloworld</a>
    <li><a href="/just-route.php">just-route.php 只要路由</a>
    <li><a href="/api.php">作为 api 服务器的例子，不需要控制器了 </a>
    <li><a href="/traditional.php">traditional.php 传统模式,一个文件解决，不折腾那么多 </a>
</ul>
完整模式下的其他例子
<ul>
    <li><a href="/full/auth.php">auth.php 简单的用户验证系统</a>
    <li><a href="/full/blog.php">blog.php 简单的博客</a>
    <li><a href="/full/dbtest.php">dbtest.php 数据库演示</a>
    <li><a href="/full/rpc.php">一个远程调用 json rpc 的例子 </a>
</ul>
</div>
</body>
</html>