<?php
use Cygnite\Mvc\View\Widget;
use Cygnite\AssetManager\AssetCollection;
use Cygnite\Common\UrlManager\Url;

$asset = AssetCollection::make(function ($asset)
    {
        // group of stylesheets
        $asset->add('style', array('path' => 'assets/css/bootstrap/css/bootstrap.min.css'))
            ->add('style', array('path' => 'assets/css/bootstrap/css/bootstrap-theme.min.css'))
            ->add('style', array('path' => 'assets/css/cygnite/bootstrap/datatables-bootstrap.css'))
            ->add('style', array('path' => 'assets/css/cygnite/flash.css'))
            ->add('style', array('path' => 'assets/css/cygnite/wysihtml5/prettify.css'))
            ->add('style', array('path' => 'assets/css/cygnite/wysihtml5/bootstrap-wysihtml5.css'));

        // Group of scripts
        $asset->add('script', array('path' => 'assets/js/cygnite/jquery/1.10.1/jquery.min.js'))
            ->add('script', array('path' => 'assets/js/twitter/bootstrap/js/bootstrap.min.js'))
            ->add('script', array('path' => 'assets/js/dataTables/jquery.dataTables.min.js'))
            ->add('script', array('path' => 'assets/js/dataTables/datatables-bootstrap.js'))
            ->add('script', array('path' => 'assets/js/dataTables/datatables.fnReloadAjax.js'))
            ->add('script', array('path' => 'assets/js/dataTables/prettify.js'));

        return $asset;
    });
?>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">

    <!-- Always force latest IE rendering engine (even in intranet) & Chrome Frame -->
    <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
    <link rel="shortcut icon" href="<?php echo Url::getBase(); ?>assets/img/cygnite/fevicon.png" > </link>

    <title><?php echo $this->title; ?></title>
    <meta name="keywords" content="CRUD Application" />
    <meta name="author" content="Cygnite Framework Bootstrap Starter Site." />
    <!-- Google will often use this as its description of your page/site. Make it good. -->
    <meta name="description" content="Cygnite CRUD Generator." />
    <!--  Mobile Viewport Fix -->
    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0">

    <?php $asset->dump('style');// Header Style block ?>
    <!-- HTML5 shim, for IE6-8 support of HTML5 elements -->
    <!--[if lt IE 9]>
    <script src="http://html5shim.googlecode.com/svn/trunk/html5.js"></script>
    <![endif]-->
</head>
<body>

<!-- Fluid Container -->
<div class='container'>

    <!-- Content -->
    <?php echo $yield; ?>
    <!-- ./ Content -->

    <!-- Footer -->
    <footer class="clearfix"></footer>
    <!-- ./ Footer -->

</div>
<!-- ./ Container End -->
<?php
//Script block. Scripts will render here
$asset->dump('script');
?>

<script type="text/javascript">
    $(function () {
        $('#dataGrid').DataTable();
    });
</script>

<style type="text/css">
    .navbar-inverse {background: none repeat scroll 0 0 #07508f!important;}
</style>

</body>
</html>