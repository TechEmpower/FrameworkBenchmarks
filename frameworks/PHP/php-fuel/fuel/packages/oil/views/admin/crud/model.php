<?php echo '<?php' ?>

class Model_<?php echo $model_name; ?> extends Model_Crud
{
	protected static $_table_name = '<?php echo $table; ?>';

	public static function validate($factory)
	{
		$val = Validation::forge($factory);
<?php foreach ($fields as $field): ?>
<?php
		$rules = array('required');

		if (in_array($field['type'], array('varchar', 'string', 'char')))
		{
			if ($field['name'] === 'email')
			{
				$rules[] = 'valid_email';
			}
			$rules[] = ! is_null($field['constraint']) ? "max_length[{$field['constraint']}]" : 'max_length[255]';
		}
		elseif (in_array($field['type'], array('int', 'intenger')))
		{
			$rules[] = 'valid_string[numeric]';
		}
		
		$rules = implode('|', $rules);
?>
		$val->add_field('<?php echo $field['name']; ?>', '<?php echo ucwords(str_replace('_', ' ', $field['name'])); ?>', '<?php echo $rules; ?>');
<?php endforeach; ?>

		return $val;
	}
}
