		$data['<?php echo $plural_name ?>'] = Model_<?php echo $model_name; ?>::find('all');
		$this->template->title = "<?php echo ucfirst($plural_name); ?>";
		$this->template->content = View::forge('<?php echo $view_path ?>/index', $data);
