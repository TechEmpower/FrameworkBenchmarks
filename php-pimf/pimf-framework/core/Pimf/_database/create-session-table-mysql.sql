CREATE TABLE IF NOT EXISTS `sessions` (
     `id` VARCHAR(40) NOT NULL,
     `last_activity` INT(10) NOT NULL,
     `data` TEXT NOT NULL,
     PRIMARY KEY (`id`)
);