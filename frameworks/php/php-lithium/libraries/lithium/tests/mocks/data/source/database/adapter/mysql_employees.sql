CREATE TABLE IF NOT EXISTS `employees` (
  `id` int(11) NOT NULL AUTO_INCREMENT PRIMARY KEY,
  `company_id` int(11) NOT NULL,
  `name` varchar(255),
  `active` tinyint(1),
  `created` datetime,
  `modified` datetime,
  FOREIGN KEY (`company_id`) REFERENCES `companies`(`id`)
);
