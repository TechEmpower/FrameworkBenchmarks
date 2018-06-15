module Program

open App
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Diagnosers
open System.Text

type FastAndDirty() as self =
    inherit ManualConfig()
    do 
        let job = new Job("", RunMode.Short, InfrastructureMode.InProcess)
        self.Add(job)
        self.Add(DefaultConfig.Instance.GetLoggers() |> Array.ofSeq)
        self.Add(DefaultConfig.Instance.GetColumnProviders() |> Array.ofSeq)
        self.Add(DefaultConfig.Instance.GetAnalysers() |> Array.ofSeq)
        self.Add(DefaultConfig.Instance.GetDiagnosers() |> Array.ofSeq)
        self.Add([|MemoryDiagnoser() :> IDiagnoser|])

let node () = HtmlViews.fortunes (
    [
        { id = 1; message = "Hello world! Hello world! Hello world!" }
        { id = 1; message = "Привет мир! Привет мир! Привет мир!" }
        { id = 1; message = "Hello world! Hello world! Hello world!" }
        { id = 1; message = "Привет мир! Привет мир! Привет мир!" }
        { id = 1; message = "Hello world! Hello world! Hello world!" }
        { id = 1; message = """<script>alert("This should not be displayed in a browser alert box.");</script>""" }
        { id = 1; message = "Hello world! Hello world! Hello world!" }
        { id = 1; message = "Привет мир! Привет мир! Привет мир!" }
        { id = 1; message = "Hello world! Hello world! Hello world!" }
        { id = 1; message = "Привет мир! Привет мир! Привет мир!" }
        { id = 1; message = "Hello world! Hello world! Hello world!" }
        { id = 1; message = """<script>alert("This should not be displayed in a browser alert box.");</script>""" }
        { id = 1; message = "Hello world! Hello world! Hello world!" }
        { id = 1; message = "Привет мир! Привет мир! Привет мир!" }
        { id = 1; message = "Hello world! Hello world! Hello world!" }
        { id = 1; message = "Привет мир! Привет мир! Привет мир!" }
        { id = 1; message = "Hello world! Hello world! Hello world!" }
        { id = 1; message = """<script>alert("This should not be displayed in a browser alert box.");</script>""" }
    ]) 

let node' = node()

type HtmlBench () =

    [<Benchmark()>]
    member self.Standard () = 
        let bytes = Giraffe.GiraffeViewEngine.renderHtmlDocument node' |> Encoding.UTF8.GetBytes
        ()

    [<Benchmark>]
    member self.Custom () = 
        let stream = StetefullRendering.renderHtmlToStream Encoding.UTF8 node'
        ()


    [<Benchmark(Baseline = true)>]
    member self.StandardWithView () = 
        let bytes = Giraffe.GiraffeViewEngine.renderHtmlDocument (node()) |> Encoding.UTF8.GetBytes
        ()


    [<Benchmark>]
    member self.CustomWithView () = 
        let stream = StetefullRendering.renderHtmlToStream Encoding.UTF8 (node())
        ()

[<EntryPoint>]
let Main args =
    let _ = BenchmarkRunner.Run<HtmlBench>(FastAndDirty())
    0