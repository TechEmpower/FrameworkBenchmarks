<ul>
{% each $users as $user %}
	<li>{{$user.name}}</li>
{% endeach %}
</ul>