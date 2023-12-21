Domain Model
===

Domain model consists `Entity`, `Service` , `RepositoryInterface` and `Repository`.  
You can create domain model by command `ducere make model {domain name}`

```
user
├── user_entity.nim
├── user_repository_interface.nim
├── user_service.nim
└── user_value_objects.nim
```

## Value Object
Value object difines a behaviour of value.

```nim
type UserName* = ref object
  value:string

proc new*(_:type UserName, value:string):UserName =
  if isEmptyOrWhitespace(value):
    raise newException(Exception, "Name can't be blank")
  if value.len == 0:
    raise newException(Exception, "Name can't be blank")
  if value.len > 11:
    raise newException(Exception, "Name should be shorter than 10")
  return UserName(value:value)

proc `$`*(self:UserName):string =
  return this.value
```

## Entity
Entity is object which is a substance of business logic. In a simple application it is the same of a database table, but in a complex application it is represented as multiple tables joined together.

```nim
import ../value_objects

type User* = ref object
  id*: UserId
  name*: UserName
  email*: Email
  password*: Password

proc new*(typ:type User, id:UserId, name:UserName, email:Email, password:Password):User =
  return User(
    id:id,
    name:name,
    email:email,
    password:password,
    auth:auth
  )
```

## Repository Interface
The Repository Interface prevents the Repository knowledge from leaking to Service by executing the Repository's methods through the Di Container.

```nim
import asyncdispatch
import user_value_objects
import user_entity

type IUserRepository* = tuple
  getUserById: proc(id:UserId):Future[User]
  insert: proc(user:User):Future[int]
  save: proc(user:User):Future[void]
  delete: proc(user:User):Future[void]
```
