		$<?php echo $singular_name; ?> = Model_<?php echo $model_name; ?>::find_one_by_id($id);

		if (Input::method() == 'POST')
		{
			$val = Model_<?php echo $model_name; ?>::validate('edit');

			if ($val->run())
			{
<?php foreach ($fields as $field): ?>
				$<?php echo $singular_name; ?>-><?php echo $field['name']; ?> = Input::post('<?php echo $field['name']; ?>');
<?php endforeach; ?>

				if ($<?php echo $singular_name; ?>->save())
				{
					Session::set_flash('success', e('Updated <?php echo $singular_name; ?> #'.$id));
					Response::redirect('<?php echo $uri; ?>');
				}
				else
				{
					Session::set_flash('error', 'Nothing updated.');
				}
			}
			else
			{
				Session::set_flash('error', $val->error());
			}
		}

		$this->template->set_global('<?php echo $singular_name; ?>', $<?php echo $singular_name; ?>, false);
		$this->template->title = "<?php echo ucfirst($plural_name); ?>";
		$this->template->content = View::forge('<?php echo $view_path; ?>/edit');
