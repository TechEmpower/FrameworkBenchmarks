<?php
return array(
	"classes-files" => array(),
	"excludeds" => array(),
	"libraries-parts" => array(),
	"callback" => function ($pre) {
		$pre->addApplicationControllers()
			->addUbiquityViews()
			->addApplicationModels()
			->addUbiquityBasics(true);
	}
);
