# -*- coding: UTF-8 -*-
import json as js
import os, getopt, argparse
from prettytable import PrettyTable

workload_lists = ["fortune", "plaintext", "db", "update", "json", "query"]
result_dict = {}
# result_dict[workload][test_name + file_name]
# result_dict[update][netty-20220409082258/results.json] = [ {'latencyAvg': '90.89us', 'latencyMax': '9.24ms'}, ... ]
connection_level = []
pipeline = []
queryIntervals = []


def parse_argument():
    parser = argparse.ArgumentParser(
        description="Analyse json resluts from FrameWork BenchMark.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        '--files',
        default=[],
        nargs='+',
        help='json result files'
    )
    parser.add_argument(
        '--datas',
        default=['latencyAvg'],
        nargs='+',
        help='interested datas'
    )
    args = parser.parse_args()
    return args


def read_files(args):
    global connection_level
    global queryIntervals
    global pipeline
    for f in args.files:
        result = open(f)
        json_text = result.read()
        json_obj = js.loads(json_text)
        raw_data = json_obj["rawData"]
        connection_level = json_obj["concurrencyLevels"]
        queryIntervals = json_obj["queryIntervals"]
        pipeline = json_obj["pipelineConcurrencyLevels"]
        for workload in workload_lists:
            if (raw_data[workload]):
                for test_name, test_result in raw_data[workload].items():
                    update_result(test_name, f, workload, test_result);


def update_result(test_name, file_name, workload, test_result):
    name = test_name + '-' + file_name
    if workload not in result_dict:
        result_dict[workload] = {}
    if name not in result_dict[workload]:
        result_dict[workload][name] = test_result


def print_table(args):
    for interested_data in args.datas:
        for k, workload_results in result_dict.items():
            pt = PrettyTable()
            pt.title = "Type: " + k + ", Result: " + interested_data
            pt.field_names = [map_workload_to_field(k)] + list(result_dict[k].keys())
            for i in range(len(workload_results[next(iter(workload_results))])):
                data_row = [v[i].get(interested_data) for v in workload_results.values()]
                pt.add_row([map_workload_to_value(k)[i]] + data_row)
            print(pt)

def map_workload_to_field(workload):
    if (workload == 'query'):
        return 'queryIntervals'
    elif (workload == 'plaintext'):
        return 'pipeline'
    else:
        return 'concurrencyLevel'


def map_workload_to_value(workload):
    if (workload == 'query'):
        return queryIntervals
    elif (workload == 'plaintext'):
        return pipeline
    else:
        return connection_level


def toms(s):
    if s is None:
        return '0';
    elif s.find('ms') != -1:
        return str(s.replace('ms', ''))
    elif s.find('us') != -1:
        return str(float(s.replace('us', '')) / 1000)
    elif s.find('s') != -1:
        return str(float(s.replace('s', "")) * 1000)


if __name__ == "__main__":
    args = parse_argument()
    read_files(args)
    print_table(args)
