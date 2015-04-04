#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Caller will hand this library a buffer and ask it to either convert
it or auto-detect the type.

Based on http://code.activestate.com/recipes/52257/

Licensed under the PSF License
"""

import codecs

# None represents a potentially variable byte. "##" in the XML spec...
autodetect_dict = {  # bytepattern     : ("name",
                                         (0x00, 0x00, 0xFE, 0xFF): ("ucs4_be"),
    (0xFF, 0xFE, 0x00, 0x00): ("ucs4_le"),
    (0xFE, 0xFF, None, None): ("utf_16_be"),
    (0xFF, 0xFE, None, None): ("utf_16_le"),
    (0x00, 0x3C, 0x00, 0x3F): ("utf_16_be"),
    (0x3C, 0x00, 0x3F, 0x00): ("utf_16_le"),
    (0x3C, 0x3F, 0x78, 0x6D): ("utf_8"),
    (0x4C, 0x6F, 0xA7, 0x94): ("EBCDIC")
}


def autoDetectXMLEncoding(buffer):
    """ buffer -> encoding_name
    The buffer should be at least 4 bytes long.
    Returns None if encoding cannot be detected.
    Note that encoding_name might not have an installed
    decoder (e.g. EBCDIC)
    """
    # a more efficient implementation would not decode the whole
    # buffer at once but otherwise we'd have to decode a character at
    # a time looking for the quote character...that's a pain

    encoding = "utf_8"    # according to the XML spec, this is the default
                          # this code successively tries to refine the default
                          # whenever it fails to refine, it falls back to
                          # the last place encoding was set.
    if len(buffer) >= 4:
        bytes = (byte1, byte2, byte3, byte4) = tuple(map(ord, buffer[0:4]))
        enc_info = autodetect_dict.get(bytes, None)
        if not enc_info:  # try autodetection again removing potentially
            # variable bytes
            bytes = (byte1, byte2, None, None)
            enc_info = autodetect_dict.get(bytes)
    else:
        enc_info = None

    if enc_info:
        encoding = enc_info  # we've got a guess... these are
                             #the new defaults

        # try to find a more precise encoding using xml declaration
        secret_decoder_ring = codecs.lookup(encoding)[1]
        (decoded, length) = secret_decoder_ring(buffer)
        first_line = decoded.split("\n")[0]
        if first_line and first_line.startswith(u"<?xml"):
            encoding_pos = first_line.find(u"encoding")
            if encoding_pos != -1:
                # look for double quote
                quote_pos = first_line.find('"', encoding_pos)

                if quote_pos == -1:                 # look for single quote
                    quote_pos = first_line.find("'", encoding_pos)

                if quote_pos > -1:
                    quote_char, rest = (first_line[quote_pos],
                                        first_line[quote_pos + 1:])
                    encoding = rest[:rest.find(quote_char)]

    return encoding


def decoder(buffer):
    encoding = autoDetectXMLEncoding(buffer)
    return buffer.decode(encoding).encode('utf8')
