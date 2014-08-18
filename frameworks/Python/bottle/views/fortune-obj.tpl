<table>
<tr>
<th>id</th>
<th>message</th>
</tr>
% for fortune in fortunes:
<tr>
<td>{{ fortune.id }}</td>
<td>{{ fortune.message }}</td>
</tr>
% end
</table>
%rebase layout
