		if ($<?php echo $singular_name; ?> = Model_<?php echo $model_name; ?>::find_one_by_id($id))
		{
			$<?php echo $singular_name; ?>->delete();

			Session::set_flash('success', 'Deleted <?php echo $singular_name; ?> #'.$id);
		}

		else
		{
			Session::set_flash('error', 'Could not delete <?php echo $singular_name; ?> #'.$id);
		}

		Response::redirect('<?php echo $uri; ?>');
