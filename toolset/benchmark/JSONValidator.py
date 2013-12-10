import json

class JSONValidator():

  def validate(self, jsonString):
    obj = json.loads(jsonString)

    if not obj
      return false
    else if not obj.message
      return false
    else if not obj.message.lower() == "hello, world!"
      return false

    return true