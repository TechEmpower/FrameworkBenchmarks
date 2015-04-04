#!/usr/bin/env python
# -*- coding: utf8 -*-
# Plural-Forms for sl (Slovenian)

nplurals=4 # Slovenian language has 4 forms:
           # 1 singular and 3 plurals

# Determine plural_id for number *n* as sequence of positive
# integers: 0,1,...
# NOTE! For singular form ALWAYS return plural_id = 0
get_plural_id = lambda n: (0 if n % 100 == 1 else
                           1 if n % 100 == 2 else
                           2 if n % 100 in (3,4) else
                           3)

# Construct and return plural form of *word* using
# *plural_id* (which ALWAYS>0). This function will be executed
# for words (or phrases) not found in plural_dict dictionary
# construct_plural_form = lambda word, plural_id: (word + 'suffix')

