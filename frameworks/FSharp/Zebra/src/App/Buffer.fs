module StreamBuffer
open System
open System .IO

let private DefaultCapacity = 1386
let private MaxBuilderSize = DefaultCapacity * 3

type MemoryStreamCache = 
    
    [<ThreadStatic>]
    [<DefaultValue>]
    static val mutable private instance: MemoryStream

    static member Get() = MemoryStreamCache.Get(DefaultCapacity)
    static member Get(capacity:int) = 
        
        if capacity <= MaxBuilderSize then
            let ms = MemoryStreamCache.instance;
            let capacity = max capacity DefaultCapacity
            
            if ms <> null && capacity <= ms.Capacity then
                MemoryStreamCache.instance <- null;
                ms.SetLength 0L
                ms
            else
                new MemoryStream(capacity)
        else
            new MemoryStream(capacity)

    static member Release(ms:MemoryStream) = 
        if ms.Capacity <= MaxBuilderSize then
            MemoryStreamCache.instance <- ms
