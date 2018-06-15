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
        let job = new Job("", RunMode.Medium, InfrastructureMode.InProcess)
        //self.Add(job.WithLaunchCount(1).WithInvocationCount(32 * 32))
        self.Add(job)
        self.Add(DefaultConfig.Instance.GetLoggers() |> Array.ofSeq)
        self.Add(DefaultConfig.Instance.GetColumnProviders() |> Array.ofSeq)
        self.Add(DefaultConfig.Instance.GetAnalysers() |> Array.ofSeq)
        self.Add(DefaultConfig.Instance.GetDiagnosers() |> Array.ofSeq)
        self.Add([|MemoryDiagnoser() :> IDiagnoser|])

let node = HtmlViews.fortunes (
    [
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
        { Id = 1; Message = "Привет мир! Привет мир! Привет мир!" }
    ]) 

type HtmlBench () =

    [<Benchmark(Baseline = true)>]
    member self.Standard () = 
        let bytes = Giraffe.GiraffeViewEngine.renderHtmlDocument node |> Encoding.UTF8.GetBytes
        ()

    [<Benchmark>]
    member self.Custom () = 
        let stream = StetefullRendering.renderHtmlToStream Encoding.UTF8 node
        ()

[<EntryPoint>]
let Main args =
    let _ = BenchmarkRunner.Run<HtmlBench>(FastAndDirty())
    0