<?php

/* vim: set expandtab tabstop=4 shiftwidth=4 softtabstop=4: */

namespace eYaf;


/**
 * Layout class used for render layouts and views.
 *
 * Layout class allows to use of a base layout skeleton and render views inside
 * this layout template.
 * The concept is to not render and display the view template directly but storing it
 * in {@link $content} property. Then will render the skeleton layout and
 * pass the {@link $content} property to it.
 * <code>
 *  application/views/layouts/front.phtml
 *
 *  <html>
 *      <head>
 *          <title><?php echo $title ?></title>
 *      </head>
 *      <body>
 *          <?= $_content_ // This is where your view from each action
 *          will be displayed ?>
 *      </body>
 *  </html>
 * </code>
 *
 * If no layout is defined then returns the renderd action view template.
 *
 * Also allows to set variable to views and access them from the base
 * skeleton layout. In above layout $title variable is set to title tag.
 * Then in a view template we can set the value for $title variable.
 * <code>
 *  application/views/index/index.phtml
 *
 *  <?php $this->title = "Index Action" ?>
 *  <h1>Index Action</h1>
 * </code>
 *
 * @author Andreas Kollaros <mail@dot.com>
 *
 */
class Layout implements \Yaf\View_Interface
{

    /**
     * The template engine to render views and layout templates.
     *
     * Default engine is Yaf\View\Simple
     *
     * @var Yaf\View\Simple
     */
    public $engine;

    /**
     * Options to be passed to template engine.
     *
     * @var array
     */
    protected $options=array();

    /**
     *
     */
    protected $layout_path;

    /**
     * The name of layout file without extension.
     *
     * @var string
     */
    protected $layout = 'layout';

    /**
     * Handles the rendered action view data.
     *
     * @var string
     */
    protected $content;

    /**
     * Array with assigned template variables.
     *
     * @var array
     */
    protected $tpl_vars = array();

    /**
     * Template directory.
     *
     * @var string
     */
    protected $tpl_dir;

    /**
     * Constructor
     *
     * @param array $options key/value pair of options to be assigned to
     *                       template engine.
     *
     * @return void
     */
    public function __construct($path, $options=array())
    {
        $this->layout_path = $path;
        $this->options = $options;


    }

    /**
     * Return the instance of a template engine.
     *
     * @return Yaf\View\Simple
     */
    protected function engine()
    {
        $this->engine =  $this->engine ?: new \Yaf\View\Simple(
            $this->tpl_dir,
            $this->options
        );

        return $this->engine;
    }

    /**
     * Create engine instance and set the path of views and layout templates.
     *
     * Layout path is set by default to layout directory inside views path.
     *
     * @param string $path The directory to set as the path.
     *
     * @return void
     */
    public function setScriptPath($path)
    {
        if (is_readable($path)) {
            $this->tpl_dir = $path;
            $this->engine()->setScriptPath($path);

            // Overwirte layouts path by setting it where views path is.
            // This will force layout in modules to be placed in
            // modules/views/layouts directory
            $this->layout_path = $path . "/layouts";

            return true;
        }

        throw new \Exception("Invalid path: {$path}");
    }

    /**
     * Getter method for views path.
     *
     * @return string
     */
    public function getScriptPath()
    {
        return $this->engine()->getScriptPath();
    }

    /**
     * Setter for Layout::layout variable
     *
     * @param string $name the name of layout file without extension.
     *
     * @return void
     */
    public function setLayout($name)
    {
        $this->layout = $name;
    }

    /**
     * Getter for Layout::layout variable
     *
     * @return string the name of layout file without extension
     */
    public function getLayout()
    {
        return $this->layout;
    }


    public function setLayoutPath($path)
    {
        $this->layout_path = $path;
    }

    /**
     * Get full layout path with filename and extension.
     *
     * @return string
     */
    public function getLayoutPath()
    {
        $config = \Yaf\Application::app()->getConfig()->get('application');
        return $this->layout_path . "/" . $this->layout . ".{$config->view->ext}";
    }

    /**
     * Assign a variable to the template
     *
     * @param string $name  The variable name.
     * @param mixed  $value The variable value.
     *
     * @return void
     */
    public function __set($name, $value)
    {
        $this->assign($name, $value);
    }

    /**
     * Allows testing with empty() and isset() to work
     *
     * @param string $name
     *
     * @return boolean
     */
    public function __isset($name)
    {
        return (null !== $this->engine()->$name);
    }

    /**
     * Allows unset() on object properties to work
     *
     * @param string $name
     *
     * @return void
     */
    public function __unset($name)
    {
        $this->engine()->clear($name);
    }

    /**
     * Assign variables to the template
     *
     * Allows setting a specific key to the specified value, OR passing
     * an array of key => value pairs to set en masse.
     *
     * @see __set()
     *
     * @param string|array $name  The assignment strategy to use (key or
     *                            array of key => value pairs)
     * @param mixed        $value (Optional) If assigning a named variable,
     *                            use this as the value.
     *
     * @return void
     */
    public function assign($name, $value = null)
    {

        $this->tpl_vars[$name] = $value;

        $this->engine()->assign($name, $value);
    }

    /**
     * Assign variables by reference to the template
     *
     */
    public function assignRef($name, &$value)
    {
        $this->tpl_vars[$name] = $value;

        $this->engine()->assignRef($name, $value);
    }

    /**
     * Clear all assigned variables
     *
     * Clears all variables assigned to Yaf\View either via
     * {@link assign()} or property overloading
     * ({@link __get()}/{@link __set()}).
     *
     * @return void
     */
    public function clearVars() {
        $this->tpl_vars = array();
        $this->engine()->clear();
    }

    /**
     * Processes a view and returns the output.
     *
     * This method called once from controller to render the given view.
     * So render the view at $this->content property and then render the
     * layout template.
     *
     * @param string $tpl      The template to process.
     * @param array  $tpl_vars Additional variables to be assigned to template.
     *
     * @return string The view or layout template output.
     */
    public function render($tpl, $tpl_vars=array()) {

        $tpl_vars = array_merge($this->tpl_vars, $tpl_vars);

        $this->content = $this->engine()->render($tpl, $tpl_vars);

        // if no layout is defined,
        // return the rendered view template
        if (null == $this->layout) {

            return $this->content;
        }

        // If we assign some variables into view template, then maybe we need
        // them to layout view.
        // Hack??
        // Get template vars from view via Reflection class and assign them to
        // layout view
        $ref = new \ReflectionClass($this->engine());
        $prop = $ref->getProperty('_tpl_vars');
        $prop->setAccessible(true);
        $view_vars = $prop->getValue($this->engine());

        $tpl_vars = array_merge($tpl_vars, $view_vars);
        $tpl_vars['_content_'] = $this->content;

        return $this->engine()->render(
            $this->getLayoutPath(),
            $tpl_vars
        );
    }

    /**
     * Directly display the constens of a view / layout template.
     *
     * @param string $tpl      The template to process.
     * @param array  $tpl_vars Additional variables to be assigned to template.
     *
     * @return void
     */
    public function display($tpl, $tpl_vars=array()) {
        echo $this->render($tpl, $tpl_vars);
    }

}
