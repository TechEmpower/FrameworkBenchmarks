import os
import pyuca

unicode_collator = None

def set_unicode_collator(file):
    global unicode_collator
    unicode_collator = pyuca.Collator(file)

set_unicode_collator(os.path.join(os.path.dirname(__file__), 'allkeys.txt'))
