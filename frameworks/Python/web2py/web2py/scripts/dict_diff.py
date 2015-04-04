#!/usr/bin/env python
# -*- coding: utf-8 -*-

'''
@author: Pierre Thibault (pierre.thibault1 -at- gmail.com)
@license: MIT
@since: 2011-06-17

Usage: dict_diff [OPTION]... dict1 dict2
Show the differences for two dictionaries.

  -h,  --help          Display this help message.

dict1 and dict2 are two web2py dictionary files to compare. These are the files
located in the "languages" directory of a web2py app. The tools show the
differences between the two files.
'''

__docformat__ = "epytext en"

import getopt
import os.path
import sys


def main(argv):
    """Parse the arguments and start the main process."""
    try:
        opts, args = getopt.getopt(argv, "h", ["help"])
    except getopt.GetoptError:
        exit_with_parsing_error()
    for opt, arg in opts:
        arg = arg  # To avoid a warning from Pydev
        if opt in ("-h", "--help"):
            usage()
            sys.exit()
    if len(args) == 2:
        params = list(get_dicts(*args))
        params.extend(get_dict_names(*args))
        compare_dicts(*params)
    else:
        exit_with_parsing_error()


def exit_with_parsing_error():
    """Report invalid arguments and usage."""
    print("Invalid argument(s).")
    usage()
    sys.exit(2)


def usage():
    """Display the documentation"""
    print(__doc__)


def get_dicts(dict_path1, dict_path2):
    """
    Parse the dictionaries.
    @param dict_path1: The path to the first dictionary.
    @param dict_path2: The path to the second dictionary.
    @return: The two dictionaries as a sequence.
    """

    return eval(open(dict_path1).read()), eval(open(dict_path2).read())


def get_dict_names(dict1_path, dict2_path):
    """
    Get the name of the dictionaries for the end user. Use the base name of the
    files. If the two base names are identical, returns "dict1" and "dict2."
    @param dict1_path: The path to the first dictionary.
    @param dict2_path: The path to the second dictionary.
    @return: The two dictionary names as a sequence.
    """

    dict1_name = os.path.basename(dict1_path)
    dict2_name = os.path.basename(dict2_path)
    if dict1_name == dict2_name:
        dict1_name = "dict1"
        dict2_name = "dict2"
    return dict1_name, dict2_name


def compare_dicts(dict1, dict2, dict1_name, dict2_name):
    """
    Compare the two dictionaries. Print out the result.
    @param dict1: The first dictionary.
    @param dict1: The second dictionary.
    @param dict1_name: The name of the first dictionary.
    @param dict2_name: The name of the second dictionary.
    """

    dict1_keyset = set(dict1.keys())
    dict2_keyset = set(dict2.keys())
    print_key_diff(dict1_keyset - dict2_keyset, dict1_name, dict2_name)
    print_key_diff(dict2_keyset - dict1_keyset, dict2_name, dict1_name)
    print "Value differences:"
    has_value_differences = False
    for key in dict1_keyset & dict2_keyset:
        if dict1[key] != dict2[key]:
            print "  %s:" % (key,)
            print "    %s: %s" % (dict1_name, dict1[key],)
            print "    %s: %s" % (dict2_name, dict2[key],)
            print
            has_value_differences = True
    if not has_value_differences:
        print "  None"


def print_key_diff(key_diff, dict1_name, dict2_name):
    """
    Prints the keys in the first dictionary and are in the second dictionary.
    @param key_diff: Keys in dictionary 1 not in dictionary 2.
    @param dict1_name: Name used for the first dictionary.
    @param dict2_name: Name used for the second dictionary.
    """

    print "Keys in %s not in %s:" % (dict1_name, dict2_name)
    if len(key_diff):
        for key in key_diff:
            print "  %s" % (key,)
    else:
        print "  None"
    print

if __name__ == "__main__":
    main(sys.argv[1:])  # Start the process (without the application name)
