import sys
import re

def parse_markdown_table(content):
    """Parse a markdown table into a list of dictionaries"""
    lines = [line.strip() for line in content.split('\n') if line.strip()]
    if not lines:
        return []

    # Extract headers
    headers = [h.strip() for h in lines[0].strip('|').split('|')]

    # Process rows
    rows = []
    for line in lines[2:]:  # Skip header and separator lines
        values = [v.strip() for v in line.strip('|').split('|')]
        if len(values) == len(headers):
            rows.append(dict(zip(headers, values)))

    return headers, rows

def merge_tables(table1, table2):
    """Merge two tables based on Test Name and Parameter"""
    headers1, rows1 = table1
    headers2, rows2 = table2

    # Create a dictionary for table2 for easy lookup
    table2_dict = {}
    for row in rows2:
        key = (row['Test Name'], row['Parameter'])
        table2_dict[key] = row

    # Prepare merged headers (all headers from table1 + new headers from table2)
    new_headers = [h for h in headers2 if h not in ['Test Name', 'Parameter']]
    merged_headers = headers1 + new_headers

    # Merge rows
    merged_rows = []
    for row1 in rows1:
        key = (row1['Test Name'], row1['Parameter'])
        merged_row = row1.copy()

        if key in table2_dict:
            for header in new_headers:
                merged_row[header] = table2_dict[key].get(header, '-')
        else:
            for header in new_headers:
                merged_row[header] = '-'

        merged_rows.append(merged_row)

    # Add any rows from table2 that weren't in table1
    table1_keys = {(row['Test Name'], row['Parameter']) for row in rows1}
    for row2 in rows2:
        key = (row2['Test Name'], row2['Parameter'])
        if key not in table1_keys:
            merged_row = {'Test Name': row2['Test Name'], 'Parameter': row2['Parameter']}
            for header in headers1[2:]:  # Skip Test Name and Parameter
                merged_row[header] = '-'
            for header in new_headers:
                merged_row[header] = row2.get(header, '-')
            merged_rows.append(merged_row)

    return merged_headers, merged_rows

def write_markdown_table(headers, rows, output_file):
    """Write a markdown table to a file"""
    # Write headers
    output_file.write('| ' + ' | '.join(headers) + ' |\n')

    # Write separator
    output_file.write('| ' + ' | '.join(['---'] * len(headers)) + ' |\n')

    # Write rows
    for row in rows:
        output_file.write('| ' + ' | '.join(row.get(h, '-') for h in headers) + ' |\n')

def main():
    if len(sys.argv) != 4:
        print("Usage: python merge_markdown_tables.py <file1.md> <file2.md> <output.md>")
        sys.exit(1)

    file1_path, file2_path, output_path = sys.argv[1], sys.argv[2], sys.argv[3]

    try:
        with open(file1_path, 'r') as f:
            table1_content = f.read()
        with open(file2_path, 'r') as f:
            table2_content = f.read()

        table1 = parse_markdown_table(table1_content)
        table2 = parse_markdown_table(table2_content)

        if not table1 or not table2:
            print("Error: One or both input files don't contain valid markdown tables")
            sys.exit(1)

        merged_headers, merged_rows = merge_tables(table1, table2)

        with open(output_path, 'w') as f:
            write_markdown_table(merged_headers, merged_rows, f)

        print(f"Merged table written to {output_path}")

    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
