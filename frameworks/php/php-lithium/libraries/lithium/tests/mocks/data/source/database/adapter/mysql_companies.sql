CREATE TABLE IF NOT EXISTS `companies` (
  `id` int(11) NOT NULL AUTO_INCREMENT PRIMARY KEY,
  `name` varchar(255),
  `active` tinyint(1),
  `created` datetime,
  `modified` datetime
);
