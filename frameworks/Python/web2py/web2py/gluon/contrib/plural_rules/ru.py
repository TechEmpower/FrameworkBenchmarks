#!/usr/bin/env python
# -*- coding: utf8 -*-
# Plural-Forms for ru (Russian)

nplurals=3 # Russian language has 3 forms:
           # 1 singular and 2 plurals

# Determine plural_id for number *n* as sequence of positive
# integers: 0,1,...
# NOTE! For singular form ALWAYS return plural_id = 0
get_plural_id = lambda n: (0 if n % 10 == 1 and n % 100 != 11 else
                           1 if n % 10 >= 2 and n % 10 <= 4 and
                               (n % 100 < 10 or n % 100 >= 20) else
                           2)

# construct_plural_form() is not used now because of complex
# rules of Russian language. Default version of
# this function is used to simple insert new words into
# plural_dict dictionary)
# construct_plural_form = lambda word, plural_id: word
