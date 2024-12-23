import
  std/[os, random],
  happyx,
  norm/[model, postgres]


type
  World* = ref object of Model


const
  dbHost = getEnv("DB_HOST", "127.0.0.1")
  dbUser = getEnv("DB_USER", "root")
  dbPassword = getEnv("DB_PASSWORD", "123456")
  dbDatabase = getEnv("DB_DATABASE", "hello_world")


randomize()


serve "0.0.0.0", 5000:
  setup:
    let dbConn = open(dbHost, dbUser, dbPassword, dbDatabase)

  get "/json":
    return {"message": "Hello, World!"}

  get "/plaintext":
    return "Hello, World!"
  
  get "/db":
    let rowId = rand(1..10_000)
    var row = World(id: 0)
    dbConn.select(row, "id = $1", rowId.int64)
    return {"id": row.id, "randomNumber": rowId}
