# ---> up

DROP TABLE IF EXISTS `auth_groups`;

CREATE TABLE IF NOT EXISTS `auth_groups` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) NOT NULL,
  `created_at` int(11) NOT NULL,
  `modified_at` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

# ---> down

DROP table `auth_groups`;