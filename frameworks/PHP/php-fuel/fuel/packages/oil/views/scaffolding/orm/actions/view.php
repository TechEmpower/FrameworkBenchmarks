		is_null($id) and Response::redirect('<?php echo $controller_name ?>');

		if ( ! $data['<?php echo $singular_name ?>'] = Model_<?php echo $model_name ?>::find($id))
		{
			Session::set_flash('error', 'Could not find <?php echo $singular_name; ?> #'.$id);
			Response::redirect('<?php echo $controller_name ?>');
		}

		$this->template->title = "<?php echo ucfirst($singular_name) ?>";
		$this->template->content = View::forge('<?php echo $view_path ?>/view', $data);
