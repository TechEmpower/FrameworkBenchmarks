<?php

/* products\view.html.twig */
class __TwigTemplate_e1ac6a8ba0b67ccf3fb715d54e24ec57 extends Twig_Template
{
    public function __construct(Twig_Environment $env)
    {
        parent::__construct($env);

        $this->parent = $this->env->loadTemplate("layout/main/base.html.twig");

        $this->blocks = array(
            'title' => array($this, 'block_title'),
            'content' => array($this, 'block_content'),
            'footer' => array($this, 'block_footer'),
        );
    }

    protected function doGetParent(array $context)
    {
        return "layout/main/base.html.twig";
    }

    protected function doDisplay(array $context, array $blocks = array())
    {
        $this->parent->display($context, array_merge($this->blocks, $blocks));
    }

    // line 3
    public function block_title($context, array $blocks = array())
    {
        // line 4
        echo "    Cygnite Framework - Simple Crud Operation
";
    }

    // line 7
    public function block_content($context, array $blocks = array())
    {
        // line 8
        echo "
    ";
        // line 10
        echo "
    <div style=\"float:right;margin-right:47px; margin-bottom: 10px;margin-top: 10px;padding-bottom:30px;\">
        ";
        // line 12
        echo call_user_func_array($this->env->getFunction('addLink')->getCallable(), array("products", "Back", $this->getAttribute((isset($context["buttonAttributes"]) ? $context["buttonAttributes"] : null), "primary")));
        echo "
    </div>



    <div class=\"form\" style=\"\">
        <h2>Showing Product #";
        // line 18
        echo twig_escape_filter($this->env, $this->getAttribute((isset($context["product"]) ? $context["product"] : null), "id"), "html", null, true);
        echo "</h2>

        <div class=\"form-group\">
            <div class='form-label control-label'>Name</div>
            <div class=\"col-sm-10\">
                <p class=\"form-control-static\"><span>";
        // line 23
        echo twig_escape_filter($this->env, $this->getAttribute((isset($context["product"]) ? $context["product"] : null), "product_type"), "html", null, true);
        echo " </span></p>
            </div>
        </div>

        <div class=\"form-group\">
            <div class=\"form-label control-label\">Product Name</div>
            <div class=\"col-sm-10\">
                <p class=\"form-control-static\"><span>";
        // line 30
        echo twig_escape_filter($this->env, $this->getAttribute((isset($context["product"]) ? $context["product"] : null), "name"), "html", null, true);
        echo " </span></p>
            </div>
        </div>

        <div class=\"form-group\">
            <div class=\"form-label control-label\">Product Price</div>
            <div class=\"col-sm-10\">
                <p class=\"form-control-static\"><span>";
        // line 37
        echo twig_escape_filter($this->env, $this->getAttribute((isset($context["product"]) ? $context["product"] : null), "price"), "html", null, true);
        echo " </span></p>
            </div>
        </div>
    </div>


";
    }

    // line 45
    public function block_footer($context, array $blocks = array())
    {
        // line 46
        echo "
";
    }

    public function getTemplateName()
    {
        return "products\\view.html.twig";
    }

    public function isTraitable()
    {
        return false;
    }

    public function getDebugInfo()
    {
        return array (  99 => 46,  96 => 45,  85 => 37,  75 => 30,  65 => 23,  57 => 18,  48 => 12,  44 => 10,  41 => 8,  38 => 7,  33 => 4,  30 => 3,);
    }
}
