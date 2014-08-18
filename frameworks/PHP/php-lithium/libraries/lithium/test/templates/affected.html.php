<h3>Affected Tests</h3>
<ul class="metrics">

<?php foreach ($data as $class => $test): ?>
	<?php if ($test): ?>
		<li>
			<a title="run '<?php echo $test; ?>' tests" href="<?php echo $base ?>/test/<?php echo str_replace('\\', '/', $test); ?>"><?php echo $test ?></a>
		</li>
	<?php endif ?>
<?php endforeach ?>

</ul>