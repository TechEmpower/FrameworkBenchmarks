import json, strformat
import allographer/schema_builder
import allographer/query_builder

proc migration0001sample*() =
  # Create table schema
  schema([
    table("sample_users", [
      Column().increments("id"),
      Column().string("name"),
      Column().string("email")
    ])
  ])

  # Seeder
  var users: seq[JsonNode]
  for i in 1..10:
    users.add(%*{
      "id": i,
      "name": &"user{i}",
      "email": &"user{i}@nim.com"
    })
  RDB().table("sample_users").insert(users)
