package config

import (
	"github.com/bytedance/sonic"
	"github.com/goravel/framework/contracts/foundation"
	"github.com/goravel/framework/facades"
)

func init() {
	facades.App().SetJson(NewJson())
}

type Json struct {
	marshal   func(any) ([]byte, error)
	unmarshal func([]byte, any) error
}

func NewJson() foundation.Json {
	return &Json{
		marshal:   sonic.Marshal,
		unmarshal: sonic.Unmarshal,
	}
}

func (j *Json) Marshal(v any) ([]byte, error) {
	return j.marshal(v)
}

func (j *Json) Unmarshal(data []byte, v any) error {
	return j.unmarshal(data, v)
}
