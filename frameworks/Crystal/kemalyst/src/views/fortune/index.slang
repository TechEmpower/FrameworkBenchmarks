table
  tr
    th id
    th message
  - fortunes.each do |fortune|
    tr
      td = fortune.id
      td = fortune.message
