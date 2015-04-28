<?php

/* products\index.html.twig */
class __TwigTemplate_7a3d34ed48763f29a6b083ebf2594c74 extends Twig_Template
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
        <div style=\"margin-left: 79%;margin-bottom: 10px;margin-top: 10px;\">
            ";
        // line 12
        echo call_user_func_array($this->env->getFunction('addLink')->getCallable(), array("products.type", "Add Products", $this->getAttribute((isset($context["buttonAttributes"]) ? $context["buttonAttributes"] : null), "primary")));
        echo "
        </div>

        <table cellspacing=\"0\" id=\"dataTable\" cellpadding=\"0\" style=\"width:890px;margin:0px auto;\" class=\"tablesorter data-grid\">
            <thead>
                <tr>
                    <th>Sl No.</th>
                    <th>Product Type</th>
                    <th>Name</th>
                    <th>Price</th>
                    <th class=\"sorter-false\">Action</th>
                </tr>
            </thead>

            <tbody>
                ";
        // line 27
        if ((twig_length_filter($this->env, (isset($context["products"]) ? $context["products"] : null)) > 0)) {
            // line 28
            echo "                    ";
            // line 29
            echo "                    ";
            $context['_parent'] = (array) $context;
            $context['_seq'] = twig_ensure_traversable((isset($context["products"]) ? $context["products"] : null));
            $context['loop'] = array(
              'parent' => $context['_parent'],
              'index0' => 0,
              'index'  => 1,
              'first'  => true,
            );
            if (is_array($context['_seq']) || (is_object($context['_seq']) && $context['_seq'] instanceof Countable)) {
                $length = count($context['_seq']);
                $context['loop']['revindex0'] = $length - 1;
                $context['loop']['revindex'] = $length;
                $context['loop']['length'] = $length;
                $context['loop']['last'] = 1 === $length;
            }
            foreach ($context['_seq'] as $context["key"] => $context["product"]) {
                // line 30
                echo "
                        ";
                // line 31
                if ((($this->getAttribute((isset($context["loop"]) ? $context["loop"] : null), "index") % 2) == 0)) {
                    // line 32
                    echo "                            ";
                    $context["row"] = "even";
                    // line 33
                    echo "                        ";
                } else {
                    // line 34
                    echo "                            ";
                    $context["row"] = "odd";
                    // line 35
                    echo "                        ";
                }
                // line 36
                echo "
                        <tr class='";
                // line 37
                echo twig_escape_filter($this->env, (isset($context["row"]) ? $context["row"] : null), "html", null, true);
                echo "'>
                            <td> ";
                // line 38
                echo twig_escape_filter($this->env, $this->getAttribute((isset($context["loop"]) ? $context["loop"] : null), "index"), "html", null, true);
                echo "</td>
                            <td>";
                // line 39
                echo twig_escape_filter($this->env, $this->getAttribute((isset($context["product"]) ? $context["product"] : null), "product_type"));
                echo "</td>
                            <td>";
                // line 40
                echo twig_escape_filter($this->env, $this->getAttribute((isset($context["product"]) ? $context["product"] : null), "name"));
                echo "</td>
                            <td>";
                // line 41
                echo twig_escape_filter($this->env, $this->getAttribute((isset($context["product"]) ? $context["product"] : null), "price"));
                echo "</td>
                            <td>
                                ";
                // line 43
                echo call_user_func_array($this->env->getFunction('addLink')->getCallable(), array(((("products.show." . $this->getAttribute((isset($context["product"]) ? $context["product"] : null), "id")) . "/") . (isset($context["pageNumber"]) ? $context["pageNumber"] : null)), twig_upper_filter($this->env, "View"), $this->getAttribute((isset($context["buttonAttributes"]) ? $context["buttonAttributes"] : null), "primary")));
                echo "
                                ";
                // line 44
                echo call_user_func_array($this->env->getFunction('addLink')->getCallable(), array(((("products.type." . $this->getAttribute((isset($context["product"]) ? $context["product"] : null), "id")) . "/") . (isset($context["pageNumber"]) ? $context["pageNumber"] : null)), twig_upper_filter($this->env, "Edit"), $this->getAttribute((isset($context["buttonAttributes"]) ? $context["buttonAttributes"] : null), "primary")));
                echo "
                                ";
                // line 45
                echo call_user_func_array($this->env->getFunction('addLink')->getCallable(), array(((("products.delete." . $this->getAttribute((isset($context["product"]) ? $context["product"] : null), "id")) . "/") . (isset($context["pageNumber"]) ? $context["pageNumber"] : null)), twig_upper_filter($this->env, "Delete"), $this->getAttribute((isset($context["buttonAttributes"]) ? $context["buttonAttributes"] : null), "delete")));
                echo "

                            </td>
                        </tr>

                    ";
                ++$context['loop']['index0'];
                ++$context['loop']['index'];
                $context['loop']['first'] = false;
                if (isset($context['loop']['length'])) {
                    --$context['loop']['revindex0'];
                    --$context['loop']['revindex'];
                    $context['loop']['last'] = 0 === $context['loop']['revindex0'];
                }
            }
            $_parent = $context['_parent'];
            unset($context['_seq'], $context['_iterated'], $context['key'], $context['product'], $context['_parent'], $context['loop']);
            $context = array_intersect_key($context, $_parent) + $_parent;
            // line 51
            echo "                ";
        } else {
            // line 52
            echo "                    No records found !
                ";
        }
        // line 54
        echo "            </tbody>


        </table>

        <div style=\" margin-left: 797px;\">";
        // line 59
        echo (isset($context["links"]) ? $context["links"] : null);
        echo " </div>


";
    }

    // line 64
    public function block_footer($context, array $blocks = array())
    {
        // line 65
        echo "
";
    }

    public function getTemplateName()
    {
        return "products\\index.html.twig";
    }

    public function isTraitable()
    {
        return false;
    }

    public function getDebugInfo()
    {
        return array (  182 => 65,  179 => 64,  171 => 59,  164 => 54,  160 => 52,  157 => 51,  137 => 45,  133 => 44,  129 => 43,  124 => 41,  120 => 40,  116 => 39,  112 => 38,  108 => 37,  105 => 36,  102 => 35,  99 => 34,  96 => 33,  93 => 32,  91 => 31,  88 => 30,  70 => 29,  68 => 28,  66 => 27,  48 => 12,  44 => 10,  41 => 8,  38 => 7,  33 => 4,  30 => 3,);
    }
}
