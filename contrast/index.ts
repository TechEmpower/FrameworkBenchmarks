import { readFileSync, writeFileSync } from 'fs';

import type { ProcessedStats } from './types';
import {processStatsFile} from './utilities';

const argv = process.argv;

if (argv.length != 3) {
    throw "Usage: npm run start <results directory>";
}

let techEmpowerResultsDirectory: string = argv[2];

// Parse reults file for needed information
let results = JSON.parse(readFileSync(`${techEmpowerResultsDirectory}/results.json`, 'utf8'));

const concurrencyLevels: Array<number> = results.concurrencyLevels;

// Get list of stats.txt.json files
const statsFiles: Array<string> = [];

Object.keys(results.succeeded).forEach((testType: string) => {
    results.succeeded[testType].forEach((framework: string) => {
        statsFiles.push(`${framework}/${testType}/stats.txt.json`);
    });
});

// Process stats.txt.json files
const output = statsFiles.map((filePath: string) => {
    const rawStats = JSON.parse(readFileSync(`${techEmpowerResultsDirectory}/${filePath}`, 'utf8'));
    const [framework, test] = filePath.split('/')

    return { key: filePath, value: processStatsFile(rawStats, concurrencyLevels, results.rawData[test][framework]) };
}).reduce((accumulator: any, currentValue: { key: string, value: Array<ProcessedStats> }) => {
    accumulator[currentValue.key] = currentValue.value;
    return accumulator;
}, {});

writeFileSync(`${techEmpowerResultsDirectory}/processed-stats.json`, JSON.stringify(output));
