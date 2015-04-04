#!/usr/bin/env python
# -*- coding: utf-8 -*-

"FPDF for python"

__license__ = "LGPL 3.0"
__version__ = "1.7"

from fpdf import *
try:
    from html import HTMLMixin
except ImportError:
    import warnings
    warnings.warn("web2py gluon package not installed, required for html2pdf")

from template import Template
