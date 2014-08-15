CREATE TABLE IF NOT EXISTS `pimf_cache` (
  `key` int(11) NOT NULL,
  `value` longtext,
  `expiration` INT(10) NOT NULL,
  PRIMARY KEY (`key`)
) ROW_FORMAT=COMPRESSED KEY_BLOCK_SIZE=16;
