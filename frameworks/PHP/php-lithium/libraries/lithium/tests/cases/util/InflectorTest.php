<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\util;

use lithium\util\Inflector;
use lithium\analysis\Inspector;

class InflectorTest extends \lithium\test\Unit {

	public static $_test = 'bar';

	public function tearDown() {
		Inflector::reset();
	}

	/**
	 * Tests singularization inflection rules
	 *
	 * @return void
	 */
	public function testSingularize() {
		$this->assertEqual(Inflector::singularize('categorias'), 'categoria');
		$this->assertEqual(Inflector::singularize('menus'), 'menu');
		$this->assertEqual(Inflector::singularize('news'), 'news');
		$this->assertEqual(Inflector::singularize('food_menus'), 'food_menu');
		$this->assertEqual(Inflector::singularize('Menus'), 'Menu');
		$this->assertEqual(Inflector::singularize('FoodMenus'), 'FoodMenu');
		$this->assertEqual(Inflector::singularize('houses'), 'house');
		$this->assertEqual(Inflector::singularize('powerhouses'), 'powerhouse');
		$this->assertEqual(Inflector::singularize('quizzes'), 'quiz');
		$this->assertEqual(Inflector::singularize('Buses'), 'Bus');
		$this->assertEqual(Inflector::singularize('buses'), 'bus');
		$this->assertEqual(Inflector::singularize('matrix_rows'), 'matrix_row');
		$this->assertEqual(Inflector::singularize('matrices'), 'matrix');
		$this->assertEqual(Inflector::singularize('vertices'), 'vertex');
		$this->assertEqual(Inflector::singularize('indices'), 'index');
		$this->assertEqual(Inflector::singularize('Aliases'), 'Alias');
		$this->assertEqual(Inflector::singularize('Alias'), 'Alias');
		$this->assertEqual(Inflector::singularize('Media'), 'Media');
		$this->assertEqual(Inflector::singularize('alumni'), 'alumnus');
		$this->assertEqual(Inflector::singularize('bacilli'), 'bacillus');
		$this->assertEqual(Inflector::singularize('cacti'), 'cactus');
		$this->assertEqual(Inflector::singularize('foci'), 'focus');
		$this->assertEqual(Inflector::singularize('fungi'), 'fungus');
		$this->assertEqual(Inflector::singularize('nuclei'), 'nucleus');
		$this->assertEqual(Inflector::singularize('octopuses'), 'octopus');
		$this->assertEqual(Inflector::singularize('radii'), 'radius');
		$this->assertEqual(Inflector::singularize('stimuli'), 'stimulus');
		$this->assertEqual(Inflector::singularize('syllabi'), 'syllabus');
		$this->assertEqual(Inflector::singularize('termini'), 'terminus');
		$this->assertEqual(Inflector::singularize('viri'), 'virus');
		$this->assertEqual(Inflector::singularize('people'), 'person');
		$this->assertEqual(Inflector::singularize('gloves'), 'glove');
		$this->assertEqual(Inflector::singularize('doves'), 'dove');
		$this->assertEqual(Inflector::singularize('lives'), 'life');
		$this->assertEqual(Inflector::singularize('leaves'), 'leaf');
		$this->assertEqual(Inflector::singularize('knives'), 'knife');
		$this->assertEqual(Inflector::singularize('wolves'), 'wolf');
		$this->assertEqual(Inflector::singularize('shelves'), 'shelf');
		$this->assertEqual(Inflector::singularize('causes'), 'cause');
		$this->assertEqual(Inflector::singularize(''), '');
	}

	/**
	 * Tests pluralization inflection rules
	 *
	 * @return void
	 */
	public function testPluralize() {
		$this->assertEqual(Inflector::pluralize('categoria'), 'categorias');
		$this->assertEqual(Inflector::pluralize('house'), 'houses');
		$this->assertEqual(Inflector::pluralize('powerhouse'), 'powerhouses');
		$this->assertEqual(Inflector::pluralize('Bus'), 'Buses');
		$this->assertEqual(Inflector::pluralize('bus'), 'buses');
		$this->assertEqual(Inflector::pluralize('menu'), 'menus');
		$this->assertEqual(Inflector::pluralize('news'), 'news');
		$this->assertEqual(Inflector::pluralize('food_menu'), 'food_menus');
		$this->assertEqual(Inflector::pluralize('Menu'), 'Menus');
		$this->assertEqual(Inflector::pluralize('FoodMenu'), 'FoodMenus');
		$this->assertEqual(Inflector::pluralize('quiz'), 'quizzes');
		$this->assertEqual(Inflector::pluralize('matrix_row'), 'matrix_rows');
		$this->assertEqual(Inflector::pluralize('matrix'), 'matrices');
		$this->assertEqual(Inflector::pluralize('vertex'), 'vertices');
		$this->assertEqual(Inflector::pluralize('index'), 'indices');
		$this->assertEqual(Inflector::pluralize('Alias'), 'Aliases');
		$this->assertEqual(Inflector::pluralize('Aliases'), 'Aliases');
		$this->assertEqual(Inflector::pluralize('Media'), 'Media');
		$this->assertEqual(Inflector::pluralize('alumnus'), 'alumni');
		$this->assertEqual(Inflector::pluralize('bacillus'), 'bacilli');
		$this->assertEqual(Inflector::pluralize('cactus'), 'cacti');
		$this->assertEqual(Inflector::pluralize('focus'), 'foci');
		$this->assertEqual(Inflector::pluralize('fungus'), 'fungi');
		$this->assertEqual(Inflector::pluralize('nucleus'), 'nuclei');
		$this->assertEqual(Inflector::pluralize('octopus'), 'octopuses');
		$this->assertEqual(Inflector::pluralize('radius'), 'radii');
		$this->assertEqual(Inflector::pluralize('stimulus'), 'stimuli');
		$this->assertEqual(Inflector::pluralize('syllabus'), 'syllabi');
		$this->assertEqual(Inflector::pluralize('terminus'), 'termini');
		$this->assertEqual(Inflector::pluralize('virus'), 'viri');
		$this->assertEqual(Inflector::pluralize('person'), 'people');
		$this->assertEqual(Inflector::pluralize('people'), 'people');
		$this->assertEqual(Inflector::pluralize('glove'), 'gloves');
		$this->assertEqual(Inflector::pluralize('leaf'), 'leaves');
		$this->assertEqual(Inflector::pluralize('ContactPeople'), 'ContactPeople');
		$this->assertEqual(Inflector::pluralize(''), '');

		$result = Inflector::pluralize('errata');
		$this->assertNull(Inflector::rules('plural', array('/rata/' => '\1ratum')));
		$this->assertEqual(Inflector::pluralize('errata'), $result);

		Inflector::reset();
		$this->assertNotEqual(Inflector::pluralize('errata'), $result);
	}

	/**
	 * testInflectorSlug method
	 *
	 * @return void
	 */
	public function testSlug() {
		$result = Inflector::slug('Foo Bar: Not just for breakfast any-more');
		$expected = 'Foo-Bar-Not-just-for-breakfast-any-more';
		$this->assertEqual($expected, $result);

		$result = Inflector::slug('Foo Bar: Not just for breakfast any-more', '_');
		$expected = 'Foo_Bar_Not_just_for_breakfast_any_more';
		$this->assertEqual($expected, $result);

		$result = Inflector::slug('this/is/a/path', '_');
		$expected = 'this_is_a_path';
		$this->assertEqual($expected, $result);

		$result = Inflector::slug('Foo Bar: Not just for breakfast any-more', "+");
		$expected = 'Foo+Bar+Not+just+for+breakfast+any+more';
		$this->assertEqual($expected, $result);

		$result = Inflector::slug('Äpfel Über Öl grün ärgert groß öko');
		$expected = 'Aepfel-Ueber-Oel-gruen-aergert-gross-oeko';
		$this->assertEqual($expected, $result);

		$result = Inflector::slug('The truth - and- more- news');
		$expected = 'The-truth-and-more-news';
		$this->assertEqual($expected, $result);

		$result = Inflector::slug('The truth: and more news');
		$expected = 'The-truth-and-more-news';
		$this->assertEqual($expected, $result);

		$message = 'La langue française est un attribut de souveraineté en France';
		$result = Inflector::slug($message, '-');
		$expected = 'La-langue-francaise-est-un-attribut-de-souverainete-en-France';
		$this->assertEqual($expected, $result);

		$result = Inflector::slug('!@$#exciting stuff! - what !@-# was that?');
		$expected = 'exciting-stuff-what-was-that';
		$this->assertEqual($expected, $result);

		$result = Inflector::slug('20% of profits went to me!');
		$expected = '20-of-profits-went-to-me';
		$this->assertEqual($expected, $result);

		$result = Inflector::slug('#this melts your face1#2#3');
		$expected = 'this-melts-your-face1-2-3';
		$this->assertEqual($expected, $result);

		$result = Inflector::slug('ThisMeltsYourFace');
		$expected = 'This-Melts-Your-Face';
		$this->assertEqual($expected, $result);

		$result = Inflector::slug('DŽip Džip džip');
		$expected = 'Dzip-Dzip-dzip';
		$this->assertEqual($expected, $result);

		$result = Inflector::slug('Šuma šuma');
		$expected = 'Suma-suma';
		$this->assertEqual($expected, $result);

		$result = Inflector::slug('Čamac čamac');
		$expected = 'Camac-camac';
		$this->assertEqual($expected, $result);

		$result = Inflector::slug('Ćasa ćasa');
		$expected = 'Casa-casa';
		$this->assertEqual($expected, $result);

		$result = Inflector::slug('Život život');
		$expected = 'Zivot-zivot';
		$this->assertEqual($expected, $result);

		$result = Inflector::slug('Đorđe đorđe');
		$expected = 'Djordje-djordje';
		$this->assertEqual($expected, $result);
	}

	public function testAddingInvalidRules() {
		$before = array(
			Inflector::rules('singular'),
			Inflector::rules('plural'),
			Inflector::rules('transliteration')
		);
		$this->assertNull(Inflector::rules('foo'));
		$this->assertIdentical($before, array(
			Inflector::rules('singular'),
			Inflector::rules('plural'),
			Inflector::rules('transliteration')
		));
	}

	public function testAddingSingularizationRules() {
		$before = Inflector::rules('singular');
		$result = Inflector::singularize('errata');
		$this->assertNull(Inflector::rules('singular', array('/rata/' => '\1ratus')));
		$this->assertEqual(Inflector::singularize('errata'), $result);

		Inflector::reset();
		$this->assertNotEqual(Inflector::singularize('errata'), $result);

		$after = Inflector::rules('singular');
		$expected = array(
			'rules', 'irregular', 'uninflected', 'regexUninflected', 'regexIrregular'
		);
		$this->assertEqual(array_keys($before), $expected);
		$this->assertEqual(array_keys($after), $expected);

		$result = array_diff($after['rules'], $before['rules']);
		$this->assertEqual($result, array('/rata/' => '\1ratus'));

		foreach (array('irregular', 'uninflected', 'regexUninflected', 'regexIrregular') as $key) {
			$this->assertIdentical($before[$key], $after[$key]);
		}

		$this->assertNull(Inflector::rules('singular', array('rules' => array(
			'/rata/' => '\1ratus'
		))));
		$this->assertIdentical(Inflector::rules('singular'), $after);
	}

	/**
	 * Tests that rules for uninflected singular words are kept in sync with the plural, and vice
	 * versa.
	 *
	 * @return void
	 */
	public function testIrregularWords() {
		$expectedPlural = Inflector::rules('plural');
		$this->assertFalse(isset($expectedPlural['irregular']['bar']));

		$expectedSingular = Inflector::rules('singular');
		$this->assertFalse(isset($expectedSingular['irregular']['foo']));

		Inflector::rules('singular', array('irregular' => array('foo' => 'bar')));

		$resultSingular = Inflector::rules('singular');
		$this->assertEqual($resultSingular['irregular']['foo'], 'bar');
		unset($resultSingular['irregular']['foo']);

		$this->assertEqual($resultSingular, $expectedSingular);

		$resultPlural = Inflector::rules('plural');
		$this->assertEqual($resultPlural['irregular']['bar'], 'foo');
		unset($resultPlural['irregular']['bar']);

		$this->assertEqual($resultPlural, $expectedPlural);
	}

	/**
	 * testVariableNaming method
	 *
	 * @return void
	 */
	public function testCamelize() {
		$this->assertEqual(Inflector::camelize('test-field'), 'TestField');
		$this->assertEqual(Inflector::camelize('test_field'), 'TestField');
		$this->assertEqual(Inflector::camelize('test_fieLd', false), 'testFieLd');
		$this->assertEqual(Inflector::camelize('test field', false), 'testField');
		$this->assertEqual(Inflector::camelize('Test_field', false), 'testField');
	}

	/**
	 * testClassNaming method
	 *
	 * @return void
	 */
	public function testClassify() {
		$this->assertEqual(Inflector::classify('artists_genres'), 'ArtistsGenre');
		$this->assertEqual(Inflector::classify('file_systems'), 'FileSystem');
		$this->assertEqual(Inflector::classify('news'), 'News');
	}

	/**
	 * testTableNaming method
	 *
	 * @return void
	 */
	public function testTabelize() {
		$this->assertEqual(Inflector::tableize('ArtistsGenre'), 'artists_genres');
		$this->assertEqual(Inflector::tableize('FileSystem'), 'file_systems');
		$this->assertEqual(Inflector::tableize('News'), 'news');
	}

	/**
	 * testHumanization method
	 *
	 * @return void
	 */
	public function testHumanize() {
		$this->assertEqual(Inflector::humanize('posts'), 'Posts');
		$this->assertEqual(Inflector::humanize('posts_tags'), 'Posts Tags');
		$this->assertEqual(Inflector::humanize('file_systems'), 'File Systems');
		$this->assertEqual(Inflector::humanize('the-post-title', '-'), 'The Post Title');
	}

	/**
	 * Tests adding transliterated characters to the map used in `Inflector::slug()`.
	 *
	 * @return void
	 */
	public function testAddTransliterations() {
		$this->assertEqual(Inflector::slug('Montréal'), 'Montreal');
		$this->assertNotEqual(Inflector::slug('Écaussines'), 'Ecaussines');

		Inflector::rules('transliteration', array('/É|Ê/' => 'E'));
		$this->assertEqual(Inflector::slug('Écaussines-d\'Enghien'), 'Ecaussines-d-Enghien');

		$this->assertNotEqual(Inflector::slug('JØRGEN'), 'JORGEN');
		Inflector::rules('transliteration', array('/Ø/' => 'O'));
		$this->assertEqual(Inflector::slug('JØRGEN'), 'JORGEN');

		$this->assertNotEqual(Inflector::slug('ÎÍ'), 'II');
		Inflector::rules('transliteration', array('/Î|Í/' => 'I'));
		$this->assertEqual(Inflector::slug('ÎÍ'), 'II');

		$this->assertEqual(Inflector::slug('ABc'), 'ABc');
		Inflector::rules('transliteration', array('AB' => 'a'));
		$this->assertEqual(Inflector::slug('ABc'), 'aac');
	}

	public function testAddingUninflectedWords() {
		$this->assertEqual(Inflector::pluralize('bord'), 'bords');
		Inflector::rules('uninflected', 'bord');
		$this->assertEqual(Inflector::pluralize('bord'), 'bord');
	}

	/**
	 * Tests the storage mechanism for `$_underscored`, `$_camelized`,
	 *  `$_humanized` and `$_pluralized`.
	 *
	 * @return void
	 */
	public function testStorageMechanism() {
		Inflector::reset();

		$expected = array('TestField' => 'test_field');
		$this->assertFalse($this->_getProtectedValue('$_underscored'));
		$this->assertEqual(Inflector::underscore('TestField'), 'test_field');
		$this->assertEqual($expected, $this->_getProtectedValue('$_underscored'));
		$this->assertEqual(Inflector::underscore('TestField'), 'test_field');

		$expected = array('test_field' => 'TestField');
		$this->assertFalse($this->_getProtectedValue('$_camelized'));
		$this->assertEqual(Inflector::camelize('test_field', true), 'TestField');
		$this->assertEqual($expected, $this->_getProtectedValue('$_camelized'));
		$this->assertEqual(Inflector::camelize('test_field', true), 'TestField');

		$expected = array('test_field:_' => 'Test Field');
		$this->assertFalse($this->_getProtectedValue('$_humanized'));
		$this->assertEqual(Inflector::humanize('test_field'), 'Test Field');
		$this->assertEqual($expected, $this->_getProtectedValue('$_humanized'));
		$this->assertEqual(Inflector::humanize('test_field'), 'Test Field');

		$expected = array('field' => 'fields');
		$this->assertFalse($this->_getProtectedValue('$_pluralized'));
		$this->assertEqual(Inflector::pluralize('field'), 'fields');
		$this->assertEqual($expected, $this->_getProtectedValue('$_pluralized'));
		$this->assertEqual(Inflector::pluralize('field'), 'fields');
	}

	/**
	 * This is a helper method for testStorageMechanism to fetch a private
	 * property of the Inflector class.
	 *
	 * @param string $property
	 * @return string The value of the property.
	 */
	protected function _getProtectedValue($property) {
		$info = Inspector::info("lithium\util\Inflector::{$property}");
		return $info['value'];
	}
}

?>