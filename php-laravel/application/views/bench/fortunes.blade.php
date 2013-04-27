<table>
    <tr>
        <th>id</th>
        <th>message</th>
    </tr>

    @foreach ($fortunes as $fortune)
    <tr>
        <td>{{{ $fortune->id }}}</td>
        <td>{{{ $fortune->message }}}</td>
    </tr>
    @endforeach

</table>