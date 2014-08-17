		is_null($id) and Response::redirect('<?php echo $controller_name ?>');

		$data['<?php echo $singular_name ?>'] = Model_<?php echo $model_name ?>::find_by_pk($id);

		$this->template->title = "<?php echo ucfirst($singular_name) ?>";
		$this->template->content = View::forge('<?php echo $view_path ?>/view', $data);
