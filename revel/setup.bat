set GOPATH=C:\FrameworkBenchmarks\revel
go get -u github.com/robfig/revel
go build -o bin\revel.exe github.com/robfig/revel/cmd
bin\revel.exe benchmark prod 8080
