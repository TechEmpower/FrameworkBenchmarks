<?php
/**
 * School
 *
 * Shows an example of a school system using the ORM
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Controller;

class School extends \MyController
{

	public function run()
	{
		$this->load_database();

		// You can over-ride this in certain models if needed,
		// allowing you to use multiple databases.
		// Model_Name::$db = new DB(config('other_database'));

		// New Dorm
		$d = new \Model\Dorm();
		$d->name = 'Dorm 1';
		$d->save();

		// New Student in Dorm
		$s1 = new \Model\Student();
		$s1->name = 'Mary';
		$s1->dorm_id = $d->id;
		$s1->save();

		// New Student in Dorm
		$s2 = new \Model\Student();
		$s2->name = 'Jane';
		$s2->dorm_id = $d->id;
		$s2->save();

		// New Car for student
		$c = new \Model\Car();
		$c->name = 'Truck';
		$c->student_id = $s1->id;
		$c->save(); // Insert

		$c->name = $s1->name. '\'s Truck'; // Change car name
		$c->save(); // Update

		// New Softball club
		$c = new \Model\Club();
		$c->name = 'Softball';
		$c->save();

		// Mary is in softball
		$m = new \Model\Membership();
		$m->club_id = $c->id;
		$m->student_id = $s1->id;
		$m->save();

		// Jane is in softball
		$m = new \Model\Membership();
		$m->club_id = $c->id;
		$m->student_id = $s2->id;
		$m->save();

		$this->content = dump('Created school objects');

		$clubs = \Model\Club::fetch();
		foreach($clubs as $club)
		{
			$club->load();
			foreach($club->students() as $student)
			{
				/*
				 * This student may have already been removed
				 */
				if($student->load())
				{
					$this->content .= dump('Removing '. $student->name. ' and her records');

					// Remove their car, club membership, and them
					$student->delete();
				}
			}
			$club->delete();
		}

		foreach(\Model\Dorm::fetch() as $dorm)
		{
			$this->content .= dump('Removing the '. $dorm->name. ' dorm');
			$dorm->delete(); // Delete the dorm
		}

		$this->content .= dump('Removed school objects');

		// Load the view file
		$this->content .= new \Micro\View('School/Index');

		// Load global theme sidebar
		$this->sidebar = new \Micro\View('Sidebar');

	}
}

/* Table Schema:

CREATE TABLE IF NOT EXISTS `car` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(100) DEFAULT NULL,
  `student_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `student_id` (`student_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 ;

-- --------------------------------------------------------

CREATE TABLE IF NOT EXISTS `club` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(100) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 ;

-- --------------------------------------------------------

CREATE TABLE IF NOT EXISTS `dorm` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(100) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 ;

-- --------------------------------------------------------

CREATE TABLE IF NOT EXISTS `membership` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `club_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `club_id` (`club_id`),
  KEY `student_id` (`student_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 ;

-- --------------------------------------------------------

CREATE TABLE IF NOT EXISTS `student` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `dorm_id` smallint(6) DEFAULT NULL,
  `name` varchar(100) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 ;

*/
