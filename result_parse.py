import json
import sys

def load_json(file_path):
    """Load JSON data from a file."""
    try:
        with open(file_path, 'r') as file:
            return json.load(file)
    except FileNotFoundError:
        print(f"Error: File '{file_path}' not found.")
        sys.exit(1)
    except json.JSONDecodeError:
        print(f"Error: Invalid JSON format in file '{file_path}'.")
        sys.exit(1)

def extract_data(json_data):
    """Extract the required data from the JSON structure."""
    if 'rawData' not in json_data:
        print("Error: 'rawData' key not found in the JSON file.")
        sys.exit(1)

    rawData = json_data['rawData']
    results = []

    # Define mapping for test names to their respective arrays
    test_array_mapping = {
        "db": json_data.get("concurrencyLevels", []),
        "fortune": json_data.get("concurrencyLevels", []),
        "json": json_data.get("concurrencyLevels", []),
        "plaintext": json_data.get("pipelineConcurrencyLevels", []),
        "query": json_data.get("queryIntervals", []),
        "update": json_data.get("queryIntervals", []),
        "cached-query": json_data.get("cachedQueryIntervals", [])
    }

    # Extract all frameworks dynamically
    frameworks = set()
    for test_name, test_data in rawData.items():
        # Skip tests not in test_array_mapping
        if test_name not in test_array_mapping:
            continue
        for framework in test_data.keys():
            frameworks.add(framework)

    frameworks = sorted(list(frameworks))  # Sort frameworks alphabetically

    # Process each test_name and test_array_index
    for test_name, test_data in rawData.items():
        # Skip tests not in test_array_mapping
        if test_name not in test_array_mapping:
            continue

        # Get the corresponding array for this test_name
        test_array = test_array_mapping[test_name]

        for framework in frameworks:
            if framework in test_data:
                # Ensure test_data[framework] is a list
                if isinstance(test_data[framework], list):
                    for test_array_index, test_info in enumerate(test_data[framework]):
                        total_requests = test_info.get('totalRequests', None)

                        # Use the value from the mapped array instead of the raw index
                        if test_array_index < len(test_array):
                            test_value = test_array[test_array_index]
                        else:
                            test_value = f"Index {test_array_index} out of bounds"

                        # Format totalRequests with thousand separators
                        formatted_total_requests = f"{total_requests:,}" if isinstance(total_requests, int) else '-'

                        results.append({
                            'test_name': test_name,
                            'test_value': test_value,
                            'framework': framework,
                            'totalRequests': formatted_total_requests
                        })

    return frameworks, results

def generate_markdown_table(frameworks, results):
    """Generate a markdown table from the extracted data."""
    headers = ['Test Name', 'Parameter'] + frameworks
    rows = []

    # Organize data by test_name and test_value
    organized_data = {}
    for result in results:
        key = (result['test_name'], result['test_value'])
        if key not in organized_data:
            organized_data[key] = {framework: '-' for framework in frameworks}
        organized_data[key][result['framework']] = result['totalRequests']

    # Generate rows
    for (test_name, test_value), values in organized_data.items():
        row = [test_name, str(test_value)] + [str(values[framework]) for framework in frameworks]
        rows.append(row)

    # Format the markdown table
    markdown = '| ' + ' | '.join(headers) + ' |\n'
    markdown += '| ' + ' | '.join(['---'] * len(headers)) + ' |\n'
    for row in rows:
        markdown += '| ' + ' | '.join(row) + ' |\n'

    return markdown

def main():
    if len(sys.argv) != 2:
        print("Usage: python script.py <path_to_json_file>")
        sys.exit(1)

    file_path = sys.argv[1]
    json_data = load_json(file_path)
    frameworks, results = extract_data(json_data)
    markdown_table = generate_markdown_table(frameworks, results)

    print(markdown_table)

if __name__ == '__main__':
    main()
