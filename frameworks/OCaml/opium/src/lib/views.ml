open Tyxml

let fortunes_table fortunes =
  let open Html in
  let table_header = tr [ th [ txt "id" ] ; th [ txt "message" ] ] in
  let table_row ({id;message}: Models.Fortune.t) = tr [ td [ txt (string_of_int id)] ; td [ txt message ] ] in
  table
    (table_header::List.map table_row fortunes)

let fortunes_page fortunes =
  let open Html in
  let fortunes_head =
    head
      (title (txt "Fortunes"))
      [] in
  html fortunes_head (body [ fortunes_table fortunes ])

