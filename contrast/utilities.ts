import { mean, std } from 'mathjs';
import type { ConcurrencyLevel, GenericStats, ProcessedStats, ResultsData } from './types';

export function calculateStats(arr: Array<number>, filterOutliers: boolean = true): GenericStats{
    // Remove the high and the low
    if(filterOutliers){
        arr.sort();
        arr.pop();
        arr.shift();
    }

    return arr.length ? {
        min: Math.min(... arr),
        max: Math.max(... arr),
        avg: mean(arr),
        stddev: std(arr)
    } : {
        min: undefined,
        max: undefined,
        avg: undefined,
        stddev: undefined
    };
}

export function processLatencyString(latency: string): number {
    return latency.endsWith('ms') ? parseFloat(latency.slice(0,-2)) : parseFloat(latency.slice(0,-1)) * 1000;
}

export function processStatsFile(rawStats: Array<any>, concurrencyLevels: Array<number>, resultsData: Array<ResultsData>): Array<ProcessedStats> {
    return rawStats.map((concurrencyLevel: ConcurrencyLevel, index: number) => {
        const processedStats: ProcessedStats = {
            concurrencyLevel: concurrencyLevels[index],
            rawCpuUsageStats: [],
            rawMemoryUsageStats: []
        };
    
        Object.values(concurrencyLevel).forEach(timepoint => {
            processedStats.rawCpuUsageStats.push(timepoint['total cpu usage']);
            processedStats.rawMemoryUsageStats.push(timepoint['memory usage']);
        });
    
        processedStats.processedCpuUsageStats  = {
            sys: calculateStats(processedStats.rawCpuUsageStats.map(stats => stats.sys)),
            usr: calculateStats(processedStats.rawCpuUsageStats.map(stats => stats.usr))
        };
    
        processedStats.processedMemoryUsageStats = {
            used: calculateStats(processedStats.rawMemoryUsageStats.map(stats => stats.used)),
            free: calculateStats(processedStats.rawMemoryUsageStats.map(stats => stats.free))
        };

        processedStats.latencyStats = {
            min: undefined,
            max: processLatencyString(resultsData[index].latencyMax),
            avg: processLatencyString(resultsData[index].latencyAvg),
            stddev: processLatencyString(resultsData[index].latencyStdev)
        }

        processedStats.totalRequests = resultsData[index].totalRequests;
    
        return processedStats;
    });
}
