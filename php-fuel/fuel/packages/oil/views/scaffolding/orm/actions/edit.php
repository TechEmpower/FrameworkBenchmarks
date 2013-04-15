		is_null($id) and Response::redirect('<?php echo $controller_name ?>');

		if ( ! $<?php echo $singular_name; ?> = Model_<?php echo $model_name; ?>::find($id))
		{
			Session::set_flash('error', 'Could not find <?php echo $singular_name; ?> #'.$id);
			Response::redirect('<?php echo $controller_name ?>');
		}

		$val = Model_<?php echo $model_name; ?>::validate('edit');

		if ($val->run())
		{
<?php foreach ($fields as $field): ?>
			$<?php echo $singular_name; ?>-><?php echo $field['name']; ?> = Input::post('<?php echo $field['name']; ?>');
<?php endforeach; ?>

			if ($<?php echo $singular_name; ?>->save())
			{
				Session::set_flash('success', 'Updated <?php echo $singular_name; ?> #' . $id);

				Response::redirect('<?php echo $uri; ?>');
			}

			else
			{
				Session::set_flash('error', 'Could not update <?php echo $singular_name; ?> #' . $id);
			}
		}

		else
		{
			if (Input::method() == 'POST')
			{
<?php foreach ($fields as $field): ?>
				$<?php echo $singular_name; ?>-><?php echo $field['name']; ?> = $val->validated('<?php echo $field['name']; ?>');
<?php endforeach; ?>

				Session::set_flash('error', $val->error());
			}

			$this->template->set_global('<?php echo $singular_name; ?>', $<?php echo $singular_name; ?>, false);
		}

		$this->template->title = "<?php echo ucfirst($plural_name); ?>";
		$this->template->content = View::forge('<?php echo $view_path; ?>/edit');
