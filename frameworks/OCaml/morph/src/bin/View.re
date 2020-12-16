open Tyxml;

module FortuneRow = {
  let createElement = (~fortune: Models.fortune, ()) => {
    <tr>
      <td> {Html.txt(string_of_int(fortune.id))} </td>
      <td> {Html.txt(fortune.message)} </td>
    </tr>;
  };
};

module FortunesTable = {
  let table_header = <tr> <th> "id" </th> <th> "message" </th> </tr>;

  let createElement = (~fortunes: list(Models.fortune), ()) => {
    let table_content:
      Html.list_wrap(Html.elt([< Html_types.table_content_fun])) = [
      table_header,
      ...List.map(fortune => <FortuneRow fortune />, fortunes),
    ];
    Html.table(table_content);
  };
};

module Layout = {
  let createElement = (~title, ~children, ()) =>
    <html>
      <head> <title> {Html.txt(title)} </title> </head>
      <body> ...children </body>
    </html>;
};

let fortunes_page = fortunes =>
  <Layout title="Fortunes"> <FortunesTable fortunes /> </Layout>;
