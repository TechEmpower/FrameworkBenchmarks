import std/asyncdispatch
import interface_implements
import allographer/query_builder
from ../../../../config/database import rdb
import ../../../models/fortune/fortune_value_objects
import ../../../models/fortune/fortune_entity
import ../../../models/fortune/fortune_repository_interface


type FortuneRepository* = ref object

proc new*(_:type FortuneRepository):FortuneRepository =
  return FortuneRepository()

implements FortuneRepository, IFortuneRepository:
  discard
