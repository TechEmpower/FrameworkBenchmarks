<?php echo '<?php echo Form::open(); ?>' ?>


	<fieldset>
<?php foreach ($fields as $field): ?>
		<div class="clearfix">
			<?php echo "<?php echo Form::label('". \Inflector::humanize($field['name']) ."', '{$field['name']}'); ?>\n"; ?>

			<div class="input">
<?php switch($field['type']):

				case 'text':
					echo "\t\t\t\t<?php echo Form::textarea('{$field['name']}', Input::post('{$field['name']}', isset(\${$singular_name}) ? \${$singular_name}->{$field['name']} : ''), array('class' => 'span8', 'rows' => 8)); ?>\n";
				break;

				default:
					echo "\t\t\t\t<?php echo Form::input('{$field['name']}', Input::post('{$field['name']}', isset(\${$singular_name}) ? \${$singular_name}->{$field['name']} : ''), array('class' => 'span4')); ?>\n";

endswitch; ?>

			</div>
		</div>
<?php endforeach; ?>
		<div class="actions">
			<?php echo '<?php'; ?> echo Form::submit('submit', 'Save', array('class' => 'btn btn-primary')); <?php echo '?>'; ?>


		</div>
	</fieldset>
<?php echo '<?php'; ?> echo Form::close(); <?php echo '?>'; ?>
