Usecases
===

The duty of Usecase is
- the difinition of bussiness logic.
- It calls `value object`, `Entity` and `domain service`.

## Example

```nim
import ../models/value_objects
import ../models/user/user_entity
import ../models/user/user_service

type LoginUsecase* = ref object

proc newLoginUsecase*():LoginUsecase =
  return LoginUsecase()


proc login*(this:LoginUsecase, email, password:string):int =
  let email = newEmail(email)
  let password = newPassword(password)
  let userService = newUserService()
  let user = userService.find(email)
  userService.checkPasswordValid(user, password)
  return user.id.get()
```
