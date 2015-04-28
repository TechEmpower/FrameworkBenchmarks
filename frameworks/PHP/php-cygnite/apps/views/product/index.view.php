<?php
use Cygnite\AssetManager\Asset;
use Cygnite\Common\UrlManager\Url;

if ($this->hasFlash('success')) {
    echo $this->getFlash('success');
} elseif ($this->hasError()) {
    echo $this->getFlash('error');
}
?>
<div class="page-header">
	<h3>CRUD Application
        <div class="pull-right">
        <!--<span class="glyphicon glyphicon-plus-sign">-->
        <?php echo Asset::link('product/add', 'Add Product', array('class' => 'btn btn-small btn-info cboxElement')); ?>
        </div>
	</h3>
</div>

<table  id="dataGrid" class="table table-striped table-hover">
    <thead>
        <tr>
            <th>Sl No.</th>
            			<th>Product Name</th>
			<th>Category</th>
			<th>Description</th>
			<th>Validity</th>
			<th>Price</th>
			<th>Created At</th>
			<th>Updated At</th>


            <th class="sorter-false">Action</th>
        </tr>
    </thead>
    <tbody>
    <?php
    if (count($records) > 0) {
        //$i = 1;
        if (Url::segment(2) == '' || Url::segment(3) == '' || Url::segment(3) == 1 ) {
            $i =1;
        } else {
            $i = (Url::segment(3) - 1) * $records[0]->perPage + 1;
        }

        $rowType = null;
        foreach ($records as $key => $row) {

            $rowType = ($i % 2 == 0) ? 'even' : 'odd';
    ?>
        <tr class='<?php echo $rowType; ?>'>
            <td> <?php echo $i; ?></td>
            			<td><?php echo $row->product_name; ?></td>
			<td><?php echo $row->category; ?></td>
			<td><?php echo $row->description; ?></td>
			<td><?php echo $row->validity; ?></td>
			<td><?php echo $row->price; ?></td>
			<td><?php echo $row->created_at; ?></td>
			<td><?php echo $row->updated_at; ?></td>


            <td>
                <?php
                echo Asset::link('product/show/' . $row->id, 'View', array('class' => 'btn btn btn-info btn-xs'));
                echo Asset::link('product/edit/' . $row->id, 'Edit', array('class' => 'btn btn-default btn-xs'));
                echo Asset::link('product/delete/' . $row->id, 'Delete', array('class' => 'btn btn-danger btn-xs' ));
                ?>

            </td>
        </tr>
    <?php $i++;
        }
    }
    ?>
    </tbody>

</table>

<nav > <?php //echo $this->links; ?> </nav>