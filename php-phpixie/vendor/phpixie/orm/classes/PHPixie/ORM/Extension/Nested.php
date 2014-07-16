<?php
namespace PHPixie\ORM\Extension;

/**
 * Nested sets support for the ORM Model
 *
 * @package ORM
 */
class Nested extends \PHPixie\ORM\Extension{
	
	/**
	 * Method to modify each database query.
	 * Useful if you need to add an additional special condition.
	 *
	 * @param \PHPixie\DB\Query $query Query to modify
	 * @return \PHPixie\DB\Query Modified Query
	 */
	protected function modify_query($query) {
		return $query;
	}
	
	/**
	 * Builds a query to get maximum rpos value.
	 *
	 * @return \PHPixie\DB\Query
	 */
	protected function max_rpos_query() {
		return $this->modify_query(
					$this->model->conn->query('select')
						->fields($this->pixie->db->expr('MAX(rpos) as rpos'))
						->table($this->model->table)
				);
	}
	
	/**
	 * Builds a query to assign negative positions to child nodes.
	 *
	 * @return \PHPixie\DB\Query
	 */
	protected function reverse_children_pos_query() {
		return $this->modify_query(
					$this->model->conn->query('update')
						->table($this->model->table)
						->data(array(
							'lpos' => $this->pixie->db->expr('0-lpos'),
							'rpos' => $this->pixie->db->expr('0-rpos')
						))
						->where('lpos', '>', $this->model->lpos)
						->where('rpos', '<', $this->model->rpos)
				);
	}
	
	/**
	 * Builds a query to pad out rpos of nodes to the right of the insertion spot.
	 *
	 * @param int $lpos Lpos of insertion spot
	 * @param int $width Pad width
	 * @return \PHPixie\DB\Query
	 */
	protected function pad_rpos_query($lpos, $width) {
		return $this->modify_query(
					$this->model->conn->query('update')
						->table($this->model->table)
						->data(array(
							'rpos' => $this->pixie->db->expr('rpos + '.$width)
						))
					->where('rpos', '>=', $lpos)
				);
	}
	
	/**
	 * Builds a query to pad out lpos of nodes to the right of the insertion spot.
	 *
	 * @param int $lpos Lpos of insertion spot
	 * @param int $width Pad width
	 * @return \PHPixie\DB\Query
	 */
	protected function pad_lpos_query($lpos, $width) {
		return $this->modify_query(
					$this->model->conn->query('update')
						->table($this->model->table)
						->data(array(
							'lpos' => $this->pixie->db->expr('lpos + '.$width)
						))
						->where('lpos', '>=', $lpos)
				);
	}
	
	/**
	 * Builds a query to collapse rpos of nodes to the right of the current node.
	 *
	 * @param int $width Collapse width
	 * @param bool $collapse_self Whether to collapse the rpos of the current node too.
	 * @return \PHPixie\DB\Query
	 */
	protected function collapse_rpos_query($width, $collapse_self = false) {
		return $this->modify_query(
					$this->model->conn->query('update')
						->table($this->model->table)
						->data(array(
							'rpos' => $this->pixie->db->expr('rpos - '.($width))
						))
						->where('rpos', $collapse_self?'>=':'>', $this->model->rpos)
				);
	}
	
	/**
	 * Builds a query to collapse lpos of nodes to the right of the current node.
	 *
	 * @param int $width Collapse width
	 * @return \PHPixie\DB\Query
	 */
	protected function collapse_lpos_query($width) {
		return $this->modify_query(
					$this->model->conn->query('update')
						->table($this->model->table)
						->data(array(
							'lpos' => $this->pixie->db->expr('lpos - '.($width))
						))
						->where('lpos', '>', $this->model->rpos)
				);
	}
	
	/**
	 * Builds a query to put nodes with negative positions to their proper spots.
	 *
	 * @param int $pos_offset Offset by which to move the positions
	 * @param int $depth_offset Offset by which to modify depths
	 * @return \PHPixie\DB\Query
	 */
	protected function update_reversed_query($pos_offset, $depth_offset) {
		return $this->modify_query(
					$this->model->conn->query('update')
						->table($this->model->table)
						->data(array(
							'lpos' => $this->pixie->db->expr("0 - lpos + $pos_offset"),
							'rpos' => $this->pixie-> db->expr("0 - rpos + $pos_offset"),
							'depth' => $this->pixie->db->expr("depth + $depth_offset")
						))
					->where('rpos', '<', 0)
				);
	}
	
	/**
	 * Builds a query to delete children of the current node.
	 *
	 * @return \PHPixie\DB\Query
	 */
	protected function delete_children_query() {
		return $this->modify_query(
				$this->model->conn->query('delete')
					->table($this->model->table)
					->where('lpos', '>', $this->model->lpos)
					->where('rpos', '<', $this->model->rpos)
			);
	}
	
	/**
	 * Calculates the width of the current node.
	 *
	 * @return int
	 */
	protected function width() {
		return $this->model->loaded()?$this->model->rpos - $this->model->lpos + 1:2;
	}
	
	/**
	 * Move current node to a new position in the tree.
	 * 
	 * @param \PHPixie\ORM\Model $parent Parent to append the node to.
	 * @param bool $append_to_beginning  Prepend node to the beginning of children list.
	 * @param bool $children_only Whether to move children of the node instead of the whole node.
	 *                            Defaults to false.
	 * @return void
	 */
	protected function move_to($parent, $append_to_beginning = false, $children_only = false) {
		$width = $this->width();
		if ($children_only) {
			$width = $width - 2;
		}
		if ($parent != null) {
			$lpos = $append_to_beginning?$parent->lpos+1:$parent->rpos;
			$depth = $parent->depth + 1;
		}else{
			$lpos = ($append_to_beginning?0:$this->max_rpos_query()->execute()->current()->rpos) + 1;
			$depth = 0;
		}
		$rpos = $lpos + $width - 1;
		
		if ($this->model->loaded() && $width>2)
			$this->reverse_children_pos_query()->execute();
			
		$this->pad_rpos_query($lpos, $width)->execute();
		$this->pad_lpos_query($lpos, $width)->execute();
		
		if($this->model->loaded()){
			$this->collapse_rpos_query($width, $children_only)->execute();
			$this->collapse_lpos_query($width)->execute();
			$depth_offset = $depth - $this->model->depth;
			
			if ($lpos > $this->model->lpos){
				$lpos = $lpos - $width;
				$rpos = $rpos - $width;
			}
			
			$pos_offset = $lpos - $this->model->lpos;
			if ($children_only) {
				$pos_offset = $pos_offset - 1;
				$depth_offset = $depth_offset - 1;
			}
			$this->update_reversed_query($pos_offset, $depth_offset)->execute();
		}
		
		if (!$children_only) {
			$this->model->lpos = $lpos;
			$this->model->depth = $depth;
			$this->model->rpos = $rpos;
		}else {
			if ($lpos < $this->model->lpos)
				$this->model->lpos = $this->model->lpos + $width;
			$this->model->rpos = $this->model->lpos + 1;
		}
	}
	
	/**
	 * Prepare the tree for the node move. 
	 * Execute save() on the model afterwards.
	 * 
	 * @param \PHPixie\ORM\Model $parent Parent to append the node to.
	 * @param bool $append_to_beginning  Prepend node to the beginning of children list.
	 *
	 * @return \PHPixie\ORM\Model Current Model
	 */
	public function prepare_append($parent = null, $append_to_beginning = false){
		$this->move_to($parent, $append_to_beginning);
		return $this->model;
	}
	
	/**
	 * Move children of the current node to the new parent.
	 * 
	 * @param \PHPixie\ORM\Model $parent Parent to move child nodes to.
	 * @param bool $append_to_beginning  Prepend node to the beginning of children list.
	 *
	 * @return \PHPixie\ORM\Model Current Model
	 * @throw \Exception If associated model is not loaded
	 */
	public function move_children($parent = null, $append_to_beginning = false) {
		if (!$this->model->loaded())
			throw new \Exception("The model is not loaded, hence has no children.");
		if($this->width()>2)
			$this->move_to($parent, $append_to_beginning, true);
		return $this->model;
	}
	
	/**
	 * Prepare tree for deletion of the current node.
	 * Call delete() on the model afterwards.
	 * 
	 * @return \PHPixie\ORM\Model Current Model
	 * @throw \Exception If associated model is not loaded
	 */
	public function prepare_delete() {
		if (!$this->model->loaded())
			throw new \Exception("The model is not loaded");
		$width = $this->width();
		if($width>2)
			$this->delete_children_query()->execute();
		$this->collapse_rpos_query($width)->execute();
		$this->collapse_lpos_query($width)->execute();
		return $this->model;
	}
	
	/**
	 * ORM Model with conditions set so that you can get the children
	 * of the current node.
	 * 
	 * @return \PHPixie\ORM\Model Model with conditions set
	 * @throw \Exception If associated model is not loaded
	 */
	public function children() {
		if (!$this->model->loaded())
			throw new \Exception("The model is not loaded");
		return $this->pixie->orm->get($this->model->model_name)
				->where('lpos', '>', $this->model->lpos)
				->where('rpos', '<', $this->model->rpos)
				->order_by('lpos', 'asc');
	}
	
}