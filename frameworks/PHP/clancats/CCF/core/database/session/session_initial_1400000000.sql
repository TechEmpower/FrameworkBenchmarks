# ---> up

CREATE TABLE IF NOT EXISTS `sessions` (
  `id` char(32) NOT NULL,
  `client_agent` varchar(255) NOT NULL,
  `client_ip` varchar(16) NOT NULL,
  `client_lang` varchar(5) NOT NULL,
  `client_port` int(11) NOT NULL,
  `current_lang` varchar(5) NOT NULL,
  `last_active` int(11) NOT NULL,
  `content` text NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

# ---> down

DROP table `sessions`;