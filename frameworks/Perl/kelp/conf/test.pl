{
	'+modules' => [qw(Logger)],

	modules_init => {
		Logger => {
			outputs => [
				[
					'Screen',
					name      => 'logs',
					min_level => 'debug',
					stderr => 1,
					newline => 1,
					utf8 => 1,
				],
			],
		},

		'Template::Toolkit' => {
			DEBUG => 1,
		},
	},
}

