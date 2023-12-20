/*
 Source Schema         : hello_world
*/

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for fortune
-- ----------------------------
DROP TABLE IF EXISTS `fortune`;
CREATE TABLE `fortune`  (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `message` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of fortune
-- ----------------------------

INSERT INTO `fortune` (`message`) VALUES ('Fortune 1');
INSERT INTO `fortune` (`message`) VALUES ('Fortune 2');
INSERT INTO `fortune` (`message`) VALUES ('Fortune 3');
INSERT INTO `fortune` (`message`) VALUES ('Fortune 4');
INSERT INTO `fortune` (`message`) VALUES ('Fortune 5');
INSERT INTO `fortune` (`message`) VALUES ('Fortune 6');
INSERT INTO `fortune` (`message`) VALUES ('Fortune 7');
INSERT INTO `fortune` (`message`) VALUES ('Fortune 8');
INSERT INTO `fortune` (`message`) VALUES ('Fortune 9');
INSERT INTO `fortune` (`message`) VALUES ('Fortune 10');
-- ----------------------------
-- Table structure for world
-- ----------------------------
DROP TABLE IF EXISTS `world`;
CREATE TABLE `world`  (
  `id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT,
  `randomNumber` int(11) NOT NULL,
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of world
-- ----------------------------
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));
INSERT INTO world (randomNumber) VALUES (FLOOR(1 + RAND() * 10001));

SET FOREIGN_KEY_CHECKS = 1;
