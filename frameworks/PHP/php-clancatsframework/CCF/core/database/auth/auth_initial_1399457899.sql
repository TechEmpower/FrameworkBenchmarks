# ---> up

DROP TABLE IF EXISTS `auth_logins`;

CREATE TABLE IF NOT EXISTS `auth_logins` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `restore_id` int(255) NOT NULL,
  `restore_token` varchar(255) NOT NULL,
  `last_login` int(11) NOT NULL,
  `client_agent` varchar(255) NOT NULL,
  `client_ip` varchar(255) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `restore_id` (`restore_id`,`restore_token`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

DROP TABLE IF EXISTS `auth_users`;

CREATE TABLE IF NOT EXISTS `auth_users` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `active` tinyint(1) NOT NULL,
  `email` varchar(255) NOT NULL,
  `password` varchar(255) NOT NULL,
  `storage` text NOT NULL,
  `last_login` int(11) NOT NULL,
  `created_at` int(11) NOT NULL,
  `modified_at` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

# ---> down

DROP table `auth_logins`;
DROP table `auth_users`;

