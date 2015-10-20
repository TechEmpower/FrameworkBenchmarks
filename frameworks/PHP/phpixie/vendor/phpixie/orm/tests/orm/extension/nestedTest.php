<?php
require_once(__DIR__.'/../../files/nested_orm.php');

class ORM_Extension_Nested_Test extends PHPUnit_Framework_TestCase
{
	protected $nodes = array();
	
	protected function setUp()
	{
	
		$this->db_file = tempnam(sys_get_temp_dir(), 'test.sqlite');
		$this->conf_file = tempnam(sys_get_temp_dir(), 'test.conf');
		file_put_contents($this->db_file, '');
		$db = new PDO('sqlite:'.$this->db_file);
		$db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
		$db->exec("CREATE TABLE nested(id INT PRIMARY_KEY, lpos INT, rpos INT, depth INT)");
		$this->pixie = $this->getMock('\PHPixie\Pixie',array('find_file'));
		$this->pixie->expects($this->any())
                 ->method('find_file')
                 ->will($this->returnValue($this->conf_file));
		$this->pixie->db = new \PHPixie\DB($this->pixie);
		$this->pixie->orm = new \PHPixie\ORM($this->pixie);
				 
		$this->pixie->config->set('db.orm.connection', 'sqlite:'.$this->db_file);
		$this->pixie->config->set('db.orm.driver', 'pdo');
	}
	
	protected function tearDown()
	{	
		$db = $this->pixie->db->get('orm');
		$db->conn = null;
		unlink($this->db_file);
		unlink($this->conf_file);
	}
	
	public function testTree() {
		$n = array();
		$this->makeNode(1, null);
		$this->nodeTest(1, 1, 2, 0);
		
		$this->makeNode(2, 1);
		$this->nodeTest(2, 2, 3, 1);
		$this->nodeTest(1, 1, 4, 0);
		
		$this->makeNode(3, 2);
		//
		$this->nodeTest(3, 3, 4, 2);
		$this->nodeTest(2, 2, 5, 1);
		$this->nodeTest(1, 1, 6, 0);
		
		$this->makeNode(4, 2);
		$this->nodeTest(4, 5, 6, 2);
		$this->nodeTest(3, 3, 4, 2);
		$this->nodeTest(2, 2, 7, 1);
		$this->nodeTest(1, 1, 8, 0);
		
		$this->makeNode(5, null);
		$this->nodeTest(5, 9, 10, 0);
		$this->nodeTest(4, 5, 6, 2);
		$this->nodeTest(3, 3, 4, 2);
		$this->nodeTest(2, 2, 7, 1);
		$this->nodeTest(1, 1, 8, 0);
		
		$this->moveNode(2, 5);
		
		$this->nodeTest(5, 3, 10, 0);
		$this->nodeTest(4, 7, 8, 2);
		$this->nodeTest(3, 5, 6, 2);
		$this->nodeTest(2, 4, 9, 1);
		$this->nodeTest(1, 1, 2, 0);
		
		$this->moveNode(4, 1);
		
		$this->nodeTest(5, 5, 10, 0);
		$this->nodeTest(4, 2, 3, 1);
		$this->nodeTest(3, 7, 8, 2);
		$this->nodeTest(2, 6, 9, 1);
		$this->nodeTest(1, 1, 4, 0);
		
		$this->moveNode(4, 2);
		
		$this->nodeTest(5, 3, 10, 0);
		$this->nodeTest(4, 7, 8, 2);
		$this->nodeTest(3, 5, 6, 2);
		$this->nodeTest(2, 4, 9, 1);
		$this->nodeTest(1, 1, 2, 0);
		
		$this->moveChildren(2, 1);
		//print_r($this->pixie-> db->get('orm')->execute("SELECT * from nested")->as_array());
		$this->nodeTest(5, 7, 10, 0);
		$this->nodeTest(4, 4, 5, 1);
		$this->nodeTest(3, 2, 3, 1);
		$this->nodeTest(2, 8, 9, 1);
		$this->nodeTest(1, 1, 6, 0);
		
		
		$this->moveChildren(1, 2);
		$this->nodeTest(5, 3, 10, 0);
		$this->nodeTest(4, 7, 8, 2);
		$this->nodeTest(3, 5, 6, 2);
		$this->nodeTest(2, 4, 9, 1);
		$this->nodeTest(1, 1, 2, 0);
		
		$query = $this->nodes[5]->nested->children()->query->query();
		$this->assertEquals(3, $query[1][0]);
		$this->assertEquals(10, $query[1][1]);
		
		$this->moveChildren(2, null);
		$this->nodeTest(5, 3, 6, 0);
		$this->nodeTest(4, 9, 10, 0);
		$this->nodeTest(3, 7, 8, 0);
		$this->nodeTest(2, 4, 5, 1);
		$this->nodeTest(1, 1, 2, 0);
		
		$this->moveNode(3, 2);
		$this->moveNode(4, 2);
		$this->nodeTest(5, 3, 10, 0);
		$this->nodeTest(4, 7, 8, 2);
		$this->nodeTest(3, 5, 6, 2);
		$this->nodeTest(2, 4, 9, 1);
		$this->nodeTest(1, 1, 2, 0);
		
		$this->deleteNode(3);
		$this->nodeTest(5, 3, 8, 0);
		$this->nodeTest(4, 5, 6, 2);
		$this->nodeTest(2, 4, 7, 1);
		$this->nodeTest(1, 1, 2, 0);
		
		$this->deleteNode(2);
		$this->nodeTest(5, 3, 4, 0);
		$this->nodeTest(1, 1, 2, 0);
		
		$this->deleteNode(1);
		$this->nodeTest(5, 1, 2, 0);
		
		$this->deleteNode(5);
		$this->assertEquals(true,empty($this->nodes));
		
		$n = new \Model\Nested($this->pixie);
		$except = false;
		try {
			$n->nested->prepare_delete();
		}catch (Exception $e) {
			$except = true;
		}
		$this->assertEquals(true, $except);
		
		$except = false;
		try {
			$n->nested->move_children(null);
		}catch (Exception $e) {
			$except = true;
		}
		$this->assertEquals(true, $except);
		
		$except = false;
		try {
			$n->nested->children();
		}catch (Exception $e) {
			$except = true;
		}
		$this->assertEquals(true,$except);
		
	}
	
	protected function refreshNodes() {
		$nodes = new \Model\Nested($this->pixie);
		$nodes = $nodes->find_all();
		$this->nodes = array();
		foreach($nodes as $node) 
			$this->nodes[$node->id] = $node;
	}
	
	protected function makeNode($id, $parent = null) {
		$n = new \Model\Nested($this->pixie);
		$n->id = $id;
		$n->nested->prepare_append($parent?$this->nodes[$parent]:null)->save();
		$this->refreshNodes();
		$this->nodes[$id] = $n;
		return $n;
	}
	
	protected function moveNode($id, $parent) {
		$this->nodes[$id]->nested->prepare_append($parent?$this->nodes[$parent]:null);
		$this->nodes[$id]->save();
		$this->refreshNodes();
	}
	
	protected function moveChildren($id, $parent) {
		$this->nodes[$id]->nested->move_children($parent?$this->nodes[$parent]:null);
		$this->refreshNodes();
	}
	
	protected function deleteNode($id) {
		$this->nodes[$id]->nested->prepare_delete()->delete();
		$this->refreshNodes();
	}
	
	protected function nodeTest($id, $lpos, $rpos, $depth) {
		$this->assertEquals($lpos, $this->nodes[$id]->lpos);
		$this->assertEquals($rpos, $this->nodes[$id]->rpos);
		$this->assertEquals($depth, $this->nodes[$id]->depth);
	}
	
}