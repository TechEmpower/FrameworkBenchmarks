export type JSONPrimitive = nil | number | string | boolean
export type Object = { [string]: Value }
export type Array = { Value }

export type Value = JSONPrimitive | Array | Object

return {
	NULL = newproxy() :: nil,
}
