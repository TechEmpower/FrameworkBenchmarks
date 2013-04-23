{% extends "templates/base.volt" %}

{% block title %} Fortunes {% endblock %}

{% block content %}
    <table>
        <tr>
            <th>id</th>
            <th>message</th>
        </tr>

        {% for fortune in fortunes %}
            <tr>
                <td>{{ fortune['id'] }}</td>
                <td>{{ fortune['message'] | e }}</td>
            </tr>
        {% endfor %}

    </table>
{% endblock %}