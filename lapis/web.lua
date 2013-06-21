local lapis = require("lapis")
local db = require("lapis.db")
local Model
do
  local _table_0 = require("lapis.db.model")
  Model = _table_0.Model
end
local config
do
  local _table_0 = require("lapis.config")
  config = _table_0.config
end
local insert = table.insert
local sort = table.sort
local random = math.random
local Fortune
do
  local _parent_0 = Model
  local _base_0 = { }
  _base_0.__index = _base_0
  if _parent_0 then
    setmetatable(_base_0, _parent_0.__base)
  end
  local _class_0 = setmetatable({
    __init = function(self, ...)
      if _parent_0 then
        return _parent_0.__init(self, ...)
      end
    end,
    __base = _base_0,
    __name = "Fortune",
    __parent = _parent_0
  }, {
    __index = function(cls, name)
      local val = rawget(_base_0, name)
      if val == nil and _parent_0 then
        return _parent_0[name]
      else
        return val
      end
    end,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  if _parent_0 and _parent_0.__inherited then
    _parent_0.__inherited(_parent_0, _class_0)
  end
  Fortune = _class_0
end
local World
do
  local _parent_0 = Model
  local _base_0 = { }
  _base_0.__index = _base_0
  if _parent_0 then
    setmetatable(_base_0, _parent_0.__base)
  end
  local _class_0 = setmetatable({
    __init = function(self, ...)
      if _parent_0 then
        return _parent_0.__init(self, ...)
      end
    end,
    __base = _base_0,
    __name = "World",
    __parent = _parent_0
  }, {
    __index = function(cls, name)
      local val = rawget(_base_0, name)
      if val == nil and _parent_0 then
        return _parent_0[name]
      else
        return val
      end
    end,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  if _parent_0 and _parent_0.__inherited then
    _parent_0.__inherited(_parent_0, _class_0)
  end
  World = _class_0
end
return lapis.serve((function()
  local Benchmark
  do
    local _parent_0 = lapis.Application
    local _base_0 = {
      ["/"] = function(self)
        return {
          json = {
            message = "Hello, World!"
          }
        }
      end,
      ["/db"] = function(self)
        local num_queries = tonumber(self.params.queries) or 1
        local worlds = { }
        for i = 1, num_queries do
          insert(worlds, World:find(random(1, 10000)))
        end
        return {
          json = worlds
        }
      end,
      ["/fortunes"] = function(self)
        self.fortunes = Fortune:select("")
        insert(self.fortunes, {
          id = 0,
          message = "Additional fortune added at request time."
        })
        sort(self.fortunes, function(a, b)
          return a.message < b.message
        end)
        self.title = "Fortunes"
        return self:html(function()
          return element("table", function()
            tr(function()
              th(function()
                return text("id")
              end)
              return th(function()
                return text("message")
              end)
            end)
            local _list_0 = self.fortunes
            for _index_0 = 1, #_list_0 do
              local fortune = _list_0[_index_0]
              tr(function()
                td(function()
                  return text(fortune.id)
                end)
                return td(function()
                  return text(fortune.message)
                end)
              end)
            end
          end)
        end)
      end,
      ["/update"] = function(self)
        local num_queries = tonumber(self.params.queries) or 1
        local worlds = { }
        for i = 1, num_queries do
          local wid = random(1, 10000)
          local world = World:find(wid)
          world.randomnumber = random(1, 10000)
          world:update("randomnumber")
          insert(worlds, world)
        end
        return {
          json = worlds
        }
      end,
      ["/plaintext"] = function(self)
        return {
          content_type = "text/plain",
          layout = false
        }, "Hello, World!"
      end
    }
    _base_0.__index = _base_0
    if _parent_0 then
      setmetatable(_base_0, _parent_0.__base)
    end
    local _class_0 = setmetatable({
      __init = function(self, ...)
        if _parent_0 then
          return _parent_0.__init(self, ...)
        end
      end,
      __base = _base_0,
      __name = "Benchmark",
      __parent = _parent_0
    }, {
      __index = function(cls, name)
        local val = rawget(_base_0, name)
        if val == nil and _parent_0 then
          return _parent_0[name]
        else
          return val
        end
      end,
      __call = function(cls, ...)
        local _self_0 = setmetatable({}, _base_0)
        cls.__init(_self_0, ...)
        return _self_0
      end
    })
    _base_0.__class = _class_0
    if _parent_0 and _parent_0.__inherited then
      _parent_0.__inherited(_parent_0, _class_0)
    end
    Benchmark = _class_0
    return _class_0
  end
end)())
