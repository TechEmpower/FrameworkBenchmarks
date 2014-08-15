<table>
<tr>
<th>id</th>
<th>message</th>
</tr>
% for fortune in fortunes:
<tr>
<td>{{ fortune[0] }}</td>
<td>{{ fortune[1] }}</td>
</tr>
% end
</table>
%rebase layout
