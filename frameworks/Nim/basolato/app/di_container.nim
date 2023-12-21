# fortune
import ./models/fortune/fortune_repository_interface
import ./data_stores/repositories/fortune/fortune_repository

type DiContainer* = tuple
  fortuneRepository: IFortuneRepository

proc newDiContainer():DiContainer =
  return (
    fortuneRepository: FortuneRepository.new().toInterface(),
  )

let di* = newDiContainer()
