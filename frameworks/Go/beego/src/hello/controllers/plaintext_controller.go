package controllers

type PlaintextController struct {
	Base
}

const helloWorldString = "Hello, World!"

var (
	helloWorldBytes = []byte(helloWorldString)
)

func (c *PlaintextController) Get() {
	c.Ctx.Output.Header("Content-Type", "text/plain")
	c.Ctx.Output.Body(helloWorldBytes)
}
