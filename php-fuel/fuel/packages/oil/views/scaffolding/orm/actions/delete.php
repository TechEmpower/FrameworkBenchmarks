		is_null($id) and Response::redirect('<?php echo $controller_name ?>');

		if ($<?php echo $singular_name; ?> = Model_<?php echo $model_name; ?>::find($id))
		{
			$<?php echo $singular_name; ?>->delete();

			Session::set_flash('success', 'Deleted <?php echo $singular_name; ?> #'.$id);
		}

		else
		{
			Session::set_flash('error', 'Could not delete <?php echo $singular_name; ?> #'.$id);
		}

		Response::redirect('<?php echo $uri; ?>');
