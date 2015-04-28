<?php

/* records\index.html.twig */
class __TwigTemplate_a4b346bc2879d2f304e9aa8ee36b97b4 extends Twig_Template
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
        echo call_user_func_array($this->env->getFunction('addLink')->getCallable(), array("records.type", "Add Records", $this->getAttribute((isset($context["buttonAttributes"]) ? $context["buttonAttributes"] : null), "primary")));
        echo "
    </div>

    <table cellspacing=\"0\" id=\"dataTable\" cellpadding=\"0\" style=\"width:890px;margin:0px auto;\" class=\"tablesorter data-grid\">
        <thead>
        <tr>
            <th>Sl No.</th>

            \t\t\t<th>Product Type</th>
\t\t\t<th>Name</th>
\t\t\t<th>Price</th>
\t\t\t<th>Image</th>
\t\t\t<th>Created Date</th>



            <th>Action</th>
        </tr>
        </thead>

        <tbody>
        ";
        // line 33
        if ((twig_length_filter($this->env, (isset($context["records"]) ? $context["records"] : null)) > 0)) {
            // line 34
            echo "
            ";
            // line 35
            $context['_parent'] = (array) $context;
            $context['_seq'] = twig_ensure_traversable((isset($context["records"]) ? $context["records"] : null));
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
            foreach ($context['_seq'] as $context["key"] => $context["row"]) {
                // line 36
                echo "
                ";
                // line 37
                if ((($this->getAttribute((isset($context["loop"]) ? $context["loop"] : null), "index") % 2) == 0)) {
                    // line 38
                    echo "                    ";
                    $context["rowType"] = "even";
                    // line 39
                    echo "                ";
                } else {
                    // line 40
                    echo "                    ";
                    $context["rowType"] = "odd";
                    // line 41
                    echo "                ";
                }
                // line 42
                echo "
                <tr class='";
                // line 43
                echo twig_escape_filter($this->env, (isset($context["rowType"]) ? $context["rowType"] : null), "html", null, true);
                echo "'>
                    <td> ";
                // line 44
                echo twig_escape_filter($this->env, $this->getAttribute((isset($context["loop"]) ? $context["loop"] : null), "index"), "html", null, true);
                echo "</td>

                    \t\t\t<td>";
                // line 46
                echo twig_escape_filter($this->env, $this->getAttribute((isset($context["row"]) ? $context["row"] : null), "product_type"), "html", null, true);
                echo "</td>
\t\t\t<td>";
                // line 47
                echo twig_escape_filter($this->env, $this->getAttribute((isset($context["row"]) ? $context["row"] : null), "name"), "html", null, true);
                echo "</td>
\t\t\t<td>";
                // line 48
                echo twig_escape_filter($this->env, $this->getAttribute((isset($context["row"]) ? $context["row"] : null), "price"), "html", null, true);
                echo "</td>
\t\t\t<td>";
                // line 49
                echo twig_escape_filter($this->env, $this->getAttribute((isset($context["row"]) ? $context["row"] : null), "image"), "html", null, true);
                echo "</td>
\t\t\t<td>";
                // line 50
                echo twig_escape_filter($this->env, $this->getAttribute((isset($context["row"]) ? $context["row"] : null), "created_date"), "html", null, true);
                echo "</td>



                    <td>
                        ";
                // line 55
                echo call_user_func_array($this->env->getFunction('addLink')->getCallable(), array(((("records.show." . $this->getAttribute((isset($context["row"]) ? $context["row"] : null), "id")) . "/") . (isset($context["pageNumber"]) ? $context["pageNumber"] : null)), twig_upper_filter($this->env, "View"), $this->getAttribute((isset($context["buttonAttributes"]) ? $context["buttonAttributes"] : null), "primary")));
                echo "
                        ";
                // line 56
                echo call_user_func_array($this->env->getFunction('addLink')->getCallable(), array(((("records.type." . $this->getAttribute((isset($context["row"]) ? $context["row"] : null), "id")) . "/") . (isset($context["pageNumber"]) ? $context["pageNumber"] : null)), twig_upper_filter($this->env, "Edit"), $this->getAttribute((isset($context["buttonAttributes"]) ? $context["buttonAttributes"] : null), "primary")));
                echo "
                        ";
                // line 57
                echo call_user_func_array($this->env->getFunction('addLink')->getCallable(), array(((("records.delete." . $this->getAttribute((isset($context["row"]) ? $context["row"] : null), "id")) . "/") . (isset($context["pageNumber"]) ? $context["pageNumber"] : null)), twig_upper_filter($this->env, "Delete"), $this->getAttribute((isset($context["buttonAttributes"]) ? $context["buttonAttributes"] : null), "delete")));
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
            unset($context['_seq'], $context['_iterated'], $context['key'], $context['row'], $context['_parent'], $context['loop']);
            $context = array_intersect_key($context, $_parent) + $_parent;
            // line 63
            echo "        ";
        } else {
            // line 64
            echo "            No records found !
        ";
        }
        // line 66
        echo "        </tbody>


    </table>

    <div style=\" margin-left: 797px;\">";
        // line 71
        echo (isset($context["links"]) ? $context["links"] : null);
        echo " </div>


";
    }

    // line 76
    public function block_footer($context, array $blocks = array())
    {
        // line 77
        echo "
";
    }

    public function getTemplateName()
    {
        return "records\\index.html.twig";
    }

    public function isTraitable()
    {
        return false;
    }

    public function getDebugInfo()
    {
        return array (  200 => 77,  197 => 76,  189 => 71,  182 => 66,  178 => 64,  175 => 63,  155 => 57,  151 => 56,  147 => 55,  139 => 50,  135 => 49,  131 => 48,  127 => 47,  123 => 46,  118 => 44,  114 => 43,  111 => 42,  108 => 41,  105 => 40,  102 => 39,  99 => 38,  97 => 37,  94 => 36,  77 => 35,  74 => 34,  72 => 33,  48 => 12,  44 => 10,  41 => 8,  38 => 7,  33 => 4,  30 => 3,);
    }
}
