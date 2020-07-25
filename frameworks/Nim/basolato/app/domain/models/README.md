Value object
===

Value object difines a behaviour of value.

```nim
type UserName* = ref object
  value:string

proc newUserName*(value:string):UserName =
  if isEmptyOrWhitespace(value):
    raise newException(Exception, "Name can't be blank")
  if value.len == 0:
    raise newException(Exception, "Name can't be blank")
  if value.len > 11:
    raise newException(Exception, "Name should be shorter than 10")
  return UserName(value:value)

proc get*(this:UserName):string =
  return this.value
```

---

Usase
===

Usecase create instance of `Value Object`, `Entity` and `Service` and call these methods to realize bussiness logic.

```nim
let userName = newUserName("") # Error raised
let userName = newUserName("abcdefghij") # Error raised
let userName = newUserName("John") # Success
echo username.get() # >> "John"
```

---

Di Container
===
Di Container provide Repository Impl for Repository Interface.　Passing the dependency of the Repository to the Service through the Di Container prevents the Service and Repository from becoming tightly coupled.

```nim
import user/repositories/user_rdb_repository
import user/repositories/user_json_repository

type DiContainer* = tuple
  userRepository: UserRdbRepository
  # userRepository: UserJsonRepository
```

In this example, `Repository Interface` call `UserRdbRepository` by resolving as `userRepository`.

---

Domain Model
===

Domain model consists `Entity`, `Service` , `RepositoryInterface` and `Repository`.  
You can create domain model by command `ducere make model {domain name}`

```
├── user
│   ├── repositories
│   │   ├── user_json_repository.nim
│   │   └── user_rdb_repository.nim
│   ├── user_entity.nim
│   ├── user_repository_interface.nim
│   └── user_service.nim
└── value_objects.nim
```

## Entity
Entity is object which is a substance of business logic. In a simple application it is the same of a database table, but in a complex application it is represented as multiple tables joined together.

```nim
import ../value_objects

type User* = ref object
  id:Id
  name:UserName
  email:Email
  password:Password

proc getId*(this:User):int =
  return this.id.get

proc getName*(this:User):string =
  return this.name.get

proc getEmail*(this:User):string =
  return this.email.get

proc getPassword*(this:User):string =
  return this.password.get

proc getHashedPassword*(this:User):string =
  return this.password.getHashed


# =============================================================================
proc newUser*(id:Id):User =
  return User(id:id)

proc newUser*(name:UserName, email:Email, password:Password):User =
  # signin
  if not email.isUnique():
    raise newException(Exception, "email should unique")

  return User(
    name:name,
    email:email,
    password:password
  )

proc newUser*(email:Email, password:Password):User =
  # Login
  return User(
    email:email,
    password:password
  )
```

## Repository Interface
The Repository Interface prevents the Repository knowledge from leaking to Service by executing the Repository's methods through the Di Container.

```nim
include ../di_container

type IUserRepository* = ref object

proc newIUserRepository*():IUserRepository =
  return IUserRepository()

proc find*(this:IUserRepository, email:Email):Option[User] =
  return DiContainer.userRepository().find(email)

proc save*(this:IUserRepository, user:User):int =
  return DiContainer.userRepository().save(user)
```

## Repository
Repository is a functions to access database, file or extrnal web API.

```nim
type UserRdbRepository* = ref object

proc newUserRdbRepository*():UserRdbRepository =
  return UserRdbRepository()


proc show*(this:UserRdbRepository, user:User):JsonNode =
  return newUser().find(user.getId)

proc store*(this:UserRdbRepository, user:User):int =
  newUser().insertID(%*{
    "name": user.getName(),
    "email": user.getEmail(),
    "password": user.getHashedPassword()
  })
```