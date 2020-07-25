# framework
import basolato/controller
# view
import ../../resources/pages/welcome_view


type WelcomeController* = ref object of Controller

proc newWelcomeController*(request:Request):WelcomeController =
  return WelcomeController.newController(request)


proc index*(this:WelcomeController):Response =
  let name = "Basolato " & basolatoVersion
  return render(this.view.welcomeView(name))
