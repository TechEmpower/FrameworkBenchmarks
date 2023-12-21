Repositories
===

Repository is a functions to instantiate and persisted `aggregate` model to access file or extrnal web API.  
`Repository` should be created in correspondence with the `aggregate`, a top of `Domain Model`.  
The task of `Repository` is CRUD for `aggregate`.

```nim
type UserRepository* = ref object

proc new*(_:type UserRepository):UserRdbRepository =
  return UserRdbRepository()

implements UserRepository, IUserRepository:
  proc getUserById*(self:UserRdbRepository, id:UserId):Future[User] {.async.} =
    let userOpt = await rdb.table("users").find(id.get)
    if not userOpt.isSome:
      raise newException(Exception, "user is not found")
    let user = userOpt.get
    return User.new(
      id,
      Name.new(user["name"].getStr),
      Email.new(user["email"].getStr),
      Password.new(user["password"].getStr),
    )

  proc insert*(self:UserRdbRepository, user:User):Future[int] {.async.} =
    return await rdb.table("users").insertId(%*{
      "id": user.id.get,
      "name": $user.name,
      "email": $user.email,
      "password": $user.password
    })

  proc save*(self:UserRdbRepository, user:User):Future[void] {.async.} =
    await rdb.table("users")
      .where("id", "=", user.id.get)
      .update(%*{
        "name": $user.name,
        "email": $user.email,
        "password": $user.password
      })

  proc delete*(self:UserRdbRepository, user:User):Future[void] {.async.}
    await rdb.table("users").delete(user.id.get)
```
