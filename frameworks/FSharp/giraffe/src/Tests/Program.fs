module Program

open App
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Diagnosers
open System.Text
open System.IO
open Microsoft.IO
open System
open Custom

type FastAndDirty() as self =
    inherit ManualConfig()
    do 
        let job = new Job("", RunMode.Default, InfrastructureMode.InProcess)
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

type MemoryPoolBench () =
    let pool = new RecyclableMemoryStreamManager();    

    [<Benchmark(Baseline = true)>]
    member self.NewMemoryStream () = 
        let start = new MemoryStream()
        let stream = StetefullRendering.renderHtmlToStream start node'
        ()

    [<Benchmark>]
    member self.CustomPool() = 
        let stream = MemoryStreamCache.Get()
        StetefullRendering.renderHtmlToStream stream node'
        MemoryStreamCache.Release stream
        ()

    [<Benchmark>]
    member self.MSPool () = 
        use start = pool.GetStream()
        let stream = StetefullRendering.renderHtmlToStream start node'
        ()

type HtmlBench () =

    [<Benchmark()>]
    member self.Standard () = 
        let bytes = Giraffe.GiraffeViewEngine.renderHtmlDocument node' |> Encoding.UTF8.GetBytes
        ()

    [<Benchmark>]
    member self.Custom () = 
        let start = new MemoryStream()
        let stream = StetefullRendering.renderHtmlToStream start node'
        ()

    [<Benchmark(Baseline = true)>]
    member self.StandardWithView () = 
        let bytes = Giraffe.GiraffeViewEngine.renderHtmlDocument (node()) |> Encoding.UTF8.GetBytes
        ()

    [<Benchmark>]
    member self.CustomWithView () = 
        let start = new MemoryStream()
        let stream = StetefullRendering.renderHtmlToStream start (node())
        ()

[<EntryPoint>]
let Main args =
    let _ = BenchmarkRunner.Run<MemoryPoolBench>(FastAndDirty())
    0