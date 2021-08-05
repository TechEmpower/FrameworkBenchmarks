export type ConcurrencyLevel = { [index:string] : TimePoint };

export type TimePoint = {
    'total cpu usage': RawCpuUsageStats
    'memory usage': RawMemoryUsageStats
}

export type GenericStats = {
    min: number | undefined
    max: number | undefined
    avg: number | undefined
    stddev: number | undefined
}

export type ProcessedCpuUsageStats = {
    sys: GenericStats
    usr: GenericStats
}

export type ProcessedMemoryUsageStats = {
    used: GenericStats
    free: GenericStats
}

export type RawCpuUsageStats = {
    sys: number
    stl: number
    idl: number
    usr: number
    wai: number
}

export type RawMemoryUsageStats = {
    used: number
    cach: number
    free: number
    buff: number
}

export type ProcessedStats = {
    concurrencyLevel: number
    rawCpuUsageStats: Array<RawCpuUsageStats>
    rawMemoryUsageStats: Array<RawMemoryUsageStats>
    processedCpuUsageStats?: ProcessedCpuUsageStats
    processedMemoryUsageStats?: ProcessedMemoryUsageStats
    latencyStats?: GenericStats
    totalRequests?: number
}

export type ResultsData = {
    latencyAvg: string
    latencyMax: string
    latencyStdev: string
    totalRequests: number
}
