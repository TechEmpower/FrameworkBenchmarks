{
	modules => [qw(JSON Template::Toolkit)],
	modules_init => {
		'Template::Toolkit' => {
			STRICT => 1,
			OUTLINE_TAG => qr{\V*%%}, # https://github.com/abw/Template2/issues/320
			ENCODING => 'utf8',
			INCLUDE_PATH => 'views',
		},
	},
}

