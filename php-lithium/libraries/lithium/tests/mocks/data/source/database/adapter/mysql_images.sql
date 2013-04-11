CREATE TABLE IF NOT EXISTS `images` (
  `id` int(11) NOT NULL AUTO_INCREMENT PRIMARY KEY,
  `gallery_id` int(11) DEFAULT NULL,
  `image` tinytext,
  `title` varchar(50) DEFAULT NULL,
  FOREIGN KEY (`gallery_id`) REFERENCES `galleries`(`id`)
);