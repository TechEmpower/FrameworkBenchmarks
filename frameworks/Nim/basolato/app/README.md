Di Container
===
Di Container provide repository implement of Interface. Passing the dependency of the Repository to the Service through the Di Container prevents the Service and Repository from becoming tightly coupled.

```nim
# user
import models/user/user_repository_interface
import data_stores/repositories/user/user_repository
# todo
import usecases/todo/todo_query_interface
import data_stores/query_services/todo/todo_query

type DiContainer* = tuple
  userRepository: IUserRepository
  todoQuery: ITodoQuery

proc newDiContainer():DiContainer =
  return (
    userRepository: UserRepository.new().toInterface(),
    todoQuery: TodoQuery.new().toInterface(),
  )

let di* = newDiContainer()
```

In this example, `Repository Interface` call `UserRdbRepository` by resolving as `userRepository`.
