# -*- coding: utf-8 -*-

"""
Developed by Massimo Di Pierro, optional component of web2py, BSDv3 license.
"""

import re
import pickle
import copy
import simplejson


def quote(text):
    return str(text).replace('\\', '\\\\').replace("'", "\\'")


class Node:
    def __init__(self, name, value, url='.', readonly=False, active=True,
                 onchange=None, **kwarg):
        self.url = url
        self.name = name
        self.value = str(value)
        self.computed_value = ''
        self.incoming = {}
        self.outcoming = {}
        self.readonly = readonly
        self.active = active
        self.onchange = onchange
        self.size = 4
        self.locked = False

    def xml(self):
        return """<input name="%s" id="%s" value="%s" size="%s"
        onkeyup="ajax('%s/keyup',['%s'], ':eval');"
        onfocus="ajax('%s/focus',['%s'], ':eval');"
        onblur="ajax('%s/blur',['%s'], ':eval');" %s/>
        """ % (self.name, self.name, self.computed_value, self.size,
               self.url, self.name, self.url, self.name, self.url, self.name,
               (self.readonly and 'readonly ') or '')

    def __repr__(self):
        return '%s:%s' % (self.name, self.computed_value)


class Sheet:
    """
    Basic class for creating web spreadsheets

    New features:

    -dal spreadsheets:
        It receives a Rows object instance and presents
    the selected data in a cell per field basis (table rows
    are sheet rows as well)
    Input should be short extension data as numeric data
    or math expressions but can be anything supported by
    unicode.

    -row(), column() and matrix() class methods:
        These new methods allow to set bulk data sets
    without calling .cell() for each node
    Example::

         # controller
         from gluon.contrib.spreadsheet import Sheet

         def callback():
             return cache.ram('sheet1', lambda: None, None).process(request)

         def index():
             # standard spreadsheet method
             sheet = cache.ram('sheet1',
                 lambda: Sheet(10, 10, URL(r=request, f='callback')), 0)
             #sheet.cell('r0c3', value='=r0c0+r0c1+r0c2', readonly=True)
             return dict(sheet=sheet)

         def index():
             # database spreadsheet method
             sheet = cache.ram('sheet1',
                 lambda: Sheet(10, 10, URL(r=request, f='callback'),
                 data=db(db.mydata).select()), 0)
             return dict(sheet=sheet)

         # view
         {{extend 'layout.html'}}
         {{=sheet}}

         or insert invidivual cells via

         {{=sheet.nodes['r0c0']}}

    Sheet stores a JavaScript w2p_spreadsheet_data object
    for retrieving data updates from the client.

    The data structure of the js object is as follows:
        # columns: a dict with colname, column index map
        # colnames: a dict with column index, colname map
        # id_columns: list with id columns
        # id_colnames: dict with id colname: column index map
        # cells: dict of "rncn": value pairs
        # modified: dict of modified cells for client-side

    Also, there is a class method Sheet.update(data) that
    processes the json data as sent by the client and
    returns a set of db modifications (see the method help for
    more details)

    client JavaScript objects:

    -var w2p_spreadsheet_data
    Stores cell updates by key and
    Used for updated cells control

    -var w2p_spreadsheet_update_button
    Stores the id of the update command
    Used for event binding (update click)

    var w2p_spreadsheet_update_result
      object attributes:
        modified - n updated records
        errors - n errors
        message - a message for feedback and errors

    Stores the ajax db update call returned stats
    and the db_callback string js
    Used after calling w2p_spreadsheet_update_db()

    -function w2p_spreadsheet_update_cell(a)
    Used for responding to normal cell events
    (encapsulates old behavior)

    -function w2p_spreadsheet_update_db_callback(result)
    Called after a background db update

    -function w2p_spreadsheet_update_db()
    Called for updating the database with
    client data

    First method: Sending data trough a form helper:
    (the data payload must be inserted in a form field before
    submission)

    -Applying db changes made client-side

    Example controller:
    ...
    # present a submit button with the spreadsheet
    form = SQLFORM.factory(Field("<name>",
                                 "text",
                                 readable=False, writable=False,
                                 formname="<formname>"))
    # submit button label
    form.elements("input [type=submit]").attributes["_value"] = \
    T("Update database")
    form.elements("textarea")[0].attributes["_style"] = "display: none;"

    w2p_spreadsheet_update_script = SCRIPT('''
      jQuery(
        function(){
          jQuery("<formname>").submit(function(){
            jQuery("[name=<name>]").val(JSON.stringify(
              w2p_spreadsheet_data)
              );
          });
        }
      );
    ''')

    # retrieve changes
    if form.process().accepted:
        data = form.vars.<name>
        changes = Sheet.updated(data)

        # Do db I/O:
        for table, rows in changes.iteritems():
            for row, values in rows.iteritems():
                db[table][row].update_record(**values)

    # the action view should expose {{=form}}, {{=sheet}}, {{=myscript}}
    return dict(form=form, sheet=sheet,
                myscript=w2p_spreadseet_update_script)

    Second method: Sending data updates with .ajax()

    -spreadsheet page's view:

    {{
    =INPUT(_type="button", _value="update data",
             _id="w2p_spreadsheet_update_data")
    }}

    {{=SCRIPT('''
    jQuery(function(){
    jQuery("#w2p_spreadsheet_update_data").click(
        function(){
          jQuery.ajax({url: "%s",
                    type: "POST",
                    data:
                      {data:
                        JSON.stringify(w2p_spreadsheet_data)}
                    }
          );
        }
    );
    });
    ''' % URL(c="default", f="modified"))}}

    -controller: modified function
    def modified():
        data = request.vars.data
        changes = Sheet.updated(data)
        # (for db I/O see first method)
        return "ok"


    Third method:
    When creating a Sheet instance, pass a kwarg update_button=<button id>
    This step will auto process the updated data with db I/O (requires calling
    .process() with db=<DAL instance>). You must expose an html element which
    supports the .click() event, i.e. a normal button.

    # TODO:
    # -¿SIGNED URLS?
    # -Delete checkbox columns for each table and default
    # -Deletable=True option for showing/hiding delete checkboxes
    # -process() method support for db I/O

    """

    regex = re.compile('(?<!\w)[a-zA-Z_]\w*')
    pregex = re.compile('\d+')
    re_strings = re.compile(r'(?P<name>'
                            + r"[uU]?[rR]?'''([^']+|'{1,2}(?!'))*'''|"
                            + r"'([^'\\]|\\.)*'|"
                            + r'"""([^"]|"{1,2}(?!"))*"""|'
                            + r'"([^"\\]|\\.)*")', re.DOTALL)

    def dumps(self):
        dump = pickle.dumps(self)
        return dump

    @staticmethod
    def position(key):
        """ Returns int row, int col for a 'rncn' formatted key'"""
        try:
            r, c = Sheet.pregex.findall(key)
            r, c = int(r), int(c)
        except (ValueError, IndexError, TypeError), e:
            error = "%s. %s" % \
                ("Unexpected position parameter",
                 "Must be a key of type 'rncn'")
            raise ValueError(error)
        return r, c

    @staticmethod
    def loads(data):
        sheet = pickle.loads(data)
        return sheet

    @staticmethod
    def updated(data):
        """ Reads spreadsheet update information sent client-side.

        Returns a dict with updated database rows/fields.
        Structure:
        {<tablename>:{
                    <id>:{<fieldname>:<new value>,
                            <fieldname>:<new value>,
                            ...
                    },
                    ...
                    }
        }

        data dict argument:

        # columns: (a dict with colname, column index map)
        # colnames: (a dict with column index, colname map)
        # id_columns: list with id columns
        # id_colnames: dict with id colname: column index map
        # cells: dict of "rncn": value pairs
        # modified: dict of modified cells for client-side

        """
        data = simplejson.loads(data)

        # record update dict
        changes = {}

        # read column index per table
        # table, id column map
        tablenames = {}
        for colname, i in data["id_colnames"].iteritems():
            tablenames[colname.split(".")[0]] = i

        # Separate multitable rows
        # Identify each row id (old data)
        # Build a dict with table/row/field
        # update information.

        # collect new data by row (modified):
        for key, value in data["modified"].iteritems():
            r, c = Sheet.position(key)

            # don't apply repeated values
            if data["cells"][key] != value:
                # read tablename
                tablename, fieldname = data["colnames"][str(c)].split(".")

                # read db record id
                row_id_key = "r%sc%s" % (r, tablenames[tablename])
                row_id = data["cells"][row_id_key]

                changes.setdefault(tablename, {})
                changes[tablename].setdefault(row_id, {})
                changes[tablename][row_id][fieldname] = value

        return changes

    def process(self, request, db=None, db_callback=None):
        """
        call this in action that creates table, it will handle ajax callbacks
        optional db (a DAL instance). It's required for db I/O
        optional callback string. js commands to call after successful
        ajax db update.
        db_callback string format keys:
          modified (number of rows updated)
        """

        if not request.args(0) == "data":
            # normal cell processing
            cell = request.vars.keys()[0]

            if request.args(0) == 'focus':
                return "jQuery('#%(cell)s').val('%(value)s');" % \
                    dict(cell=cell, value=quote(self[cell].value))

            value = request.vars[cell]
            self[cell] = value

            if request.args(0) == 'blur':
                return "jQuery('#%(cell)s').val('%(value)s');" % \
                    dict(cell=cell, value=quote(self[cell].computed_value))

            elif request.args(0) == 'keyup':
                jquery = ''
                for other_key in self.modified:
                    if other_key != cell:
                        jquery += "jQuery('#%(other_key)s').val('%(value)s');" % \
                            dict(other_key=other_key,
                                 value=quote(self[other_key].computed_value))

        else:
            # spreadsheet db update
            result = dict(modified=0,
                          errors=0,
                          message="",
                          db_callback="")

            if db is not None:
                data = request.vars["data"]
                changes = self.updated(data)

                # Do db I/O:
                for table, rows in changes.iteritems():
                    for row, values in rows.iteritems():
                        db[table][row].update_record(**values)
                        result["modified"] += 1

                if db_callback is not None:
                    result["db_callback"] = db_callback
            else:
                result["message"] = "Sheet.process Error. No db found."
            return simplejson.dumps(result)

        return jquery

    def get_attributes(self, data):
        attributes = {}
        for k in data.keys():
            if k.startswith("_"):
                attributes[k] = data[k]
        return attributes

    def even_or_odd(self, v):
        """ Used for table row stripping """
        if v % 2 == 0:
            return "even"
        else:
            return "odd"

    def __init__(self, rows, cols, url='.', readonly=False,
                 active=True, onchange=None, value=None, data=None,
                 headers=None, update_button="", **kwarg):

        """
        Arguments:
        headers: a dict with "table.fieldname": name values
        value: common value for all spreadsheet
        (can be a lambda x, y: z or function reference)

        Rows and cols values will be updated automatically to fit
        the data boundaries when the data argument is a Rows object.

        self.client: for storing sheet data client side
        columns: a dict with colname, column index map
        colnames: a dict with column index, colname map
        id_columns: list with id columns
        id_colnames: dict with id colname: column index map
        cells: dict of "rncn": value pairs
        modified: dict of modified cells for client-side
        edition.
        """

        self.rows = rows
        self.cols = cols
        self.url = url
        self.nodes = {}
        self.error = 'ERROR: %(error)s'
        self.allowed_keywords = ['for', 'in', 'if', 'else', 'and', 'or', 'not',
                                 'i', 'j', 'k', 'x', 'y', 'z', 'sum']
        self.value = value
        self.environment = {}
        self.attributes = self.get_attributes(kwarg)
        self.tr_attributes = {}
        self.td_attributes = {}

        self.data = data
        self.readonly = readonly

        self.update_button = update_button

        self.client = {
            "columns": {},
            "colnames": {},
            "id_columns": [],
            "id_colnames": {},
            "cells": {},
            "modified": {},
            "headers": headers
        }

        # if db and query:
        if self.data is not None:
            # retrieve row columns length
            self.rows = len(self.data)
            # retrieve rows length
            self.cols = len(self.data.colnames)

            # map row data to rncn values
            for x, colname in enumerate(self.data.colnames):
                self.client["columns"][colname] = x
                self.client["colnames"][x] = colname

            for x, row in enumerate(self.data):
                for colname, y in self.client["columns"].iteritems():
                    key = "r%sc%s" % (x, y)
                    tablename, fieldname = colname.split(".")
                    try:
                        value = row[tablename][fieldname]
                    except (KeyError, AttributeError):
                        # single table query
                        value = row[fieldname]
                    self.client["cells"][key] = str(value)
                    # TODO: support different id keys
                    if ".id" in colname:
                        self.client["id_columns"].append(y)
                        self.client["id_colnames"][colname] = y

        for k in xrange(self.rows * self.cols):
            key = 'r%sc%s' % (k / self.cols, k % self.cols)
            r, c = self.position(key)
            if key in self.client["cells"]:
                value = self.client["cells"][key]
                # readonly id values
                if c in self.client["id_columns"]:
                    readonly = True
                else:
                    readonly = self.readonly
            elif self.value is not None:
                if callable(self.value):
                    value = self.value(r, c)
                else:
                    value = self.value
            else:
                value = '0.00'
            self.cell(key, value,
                      readonly, active, onchange)

        exec('from math import *', {}, self.environment)

    def delete_from(self, other_list):
        indices = [k for (k, node) in enumerate(other_list) if k == node]
        if indices:
            del other_list[indices[0]]

    def changed(self, node, changed_nodes=[]):
        for other_node in node.outcoming:
            if not other_node in changed_nodes:
                changed_nodes.append(other_node)
                self.changed(other_node, changed_nodes)
        return changed_nodes

    def define(self, name, obj):
        self.environment[name] = obj

    def cell(self, key, value, readonly=False, active=True,
             onchange=None, **kwarg):
        """
        key is the name of the cell
        value is the initial value of the cell. It can be a formula "=1+3"
        a cell is active if it evaluates formulas

        Value can be a function(r, c) which returns a string
        """

        if not self.regex.match(key):
            raise SyntaxError, "Invalid cell name: %s" % key
        else:
            attributes = self.get_attributes(kwarg)
            if attributes is not None:
                self.td_attributes[key] = attributes

        key = str(key)
        r, c = self.position(key)

        if callable(value):
            value = value(r, c)

        node = Node(key, value, self.url, readonly, active,
                    onchange, **kwarg)
        self.nodes[key] = node
        self[key] = value

    def get_cell_arguments(self, data, default=None):
        """Reads cell arguments from a dict object"""
        active = True
        onchange = None
        readonly = False
        value = ""
        if default is not None:
            data.update(default)
        if "active" in data:
            active = data["active"]
        if "readonly" in data:
            readonly = data["readonly"]
        if "onchange" in data:
            onchange = data["onchange"]
        if "value" in data:
            value = data["value"]
        return active, onchange, readonly, value

    def row(self, row, cells, value=None, **kwarg):
        # row: row index (0, 1, ...)
        # cells: a sequence of values or a dict of dict with
        # arg: value pairs
        # one column example:
        # {"0": {"value":1.0, "readonly":False, "active":True, "onchange":None}}
        # value: common value for all cells

        attributes = self.get_attributes(kwarg)
        if attributes is not None:
            self.tr_attributes[str(row)] = attributes
        if isinstance(cells, dict):
            for col, data in cells.iteritems():
                key = "r%sc%s" % (row, col)
                active, onchange, readonly, cell_value = \
                    self.get_cell_arguments(data, default=kwarg)
                if value is None:
                    v = cell_value
                else:
                    v = value
                self.cell(key, v, active=active,
                          readonly=readonly,
                          onchange=onchange, **attributes)
        else:
            active, onchange, readonly, all_value = \
                self.get_cell_arguments(kwarg)
            for col, cell_value in enumerate(cells):
                key = "r%sc%s" % (row, col)
                if value is None:
                    v = cell_value
                else:
                    v = value
                self.cell(key, v, active=active,
                          onchange=onchange,
                          readonly=readonly, **attributes)

    def column(self, col, cells, value=None, **kwarg):
        """
        # col: column index (0, 1, ...)
        # cells: a sequence of values or a dict of dict with
        # arg: value pairs
        # one row example:
        # {"0": {"value":1.0, "readonly":False, "active":True, "onchange":None}}
        # value: common value for all cells
        """
        attributes = self.get_attributes(kwarg)

        if isinstance(cells, dict):
            for row, data in cells.iteritems():
                key = "r%sc%s" % (row, col)
                active, onchange, readonly, cell_value = \
                    self.get_cell_arguments(data, default=kwarg)
                if value is None:
                    v = cell_value
                else:
                    v = value
                self.cell(key, v, active=active, readonly=readonly,
                          onchange=onchange, **attributes)
        else:
            active, onchange, readonly, all_value = \
                self.get_cell_arguments(kwarg)
            for row, cell_value in enumerate(cells):
                key = "r%sc%s" % (row, col)
                if value is None:
                    v = cell_value
                else:
                    v = value
                self.cell(key, v, active=active,
                          onchange=onchange, readonly=readonly,
                          **attributes)

    def matrix(self, cells, starts="r0c0", ends=None, value=None, **kwarg):
        """
        Insert a n x n matrix or a set of cells
        # starts: upper left cell
        # ends: lower right cell

        # cells: a sequence of value sequences
        # or a dict with "rncn" keys
        # Example 1 cells:
        # ((v11, v12, ... v1n),
           (vn2, vn2, ... vnn))
        # Example 2 cells:
        # {"r0c0": {...}, ... "rncn": {...}}
        # value: common value for all cells
        """
        attributes = self.get_attributes(kwarg)

        starts_r, starts_c = self.position(starts)
        ends_r, ends_c = None, None
        if ends is not None:
            ends_r, ends_c = self.position(ends)

        if isinstance(cells, dict):
            for key, data in cells.iteritems():
                r, c = self.position(key)
                key = "r%sc%s" % (r + starts_r, c + starts_c)
                active, onchange, readonly, cell_value = \
                    self.get_cell_arguments(data, default=kwarg)
                if value is None:
                    v = cell_value
                else:
                    v = value
                if (ends is None) or ((ends_r >= r + starts_r) and
                                      (ends_c >= c + starts_c)):
                    self.cell(key, v, active=active,
                              readonly=readonly,
                              onchange=onchange, **attributes)
        else:
            active, onchange, readonly, all_value = \
                self.get_cell_arguments(kwarg)
            for r, row in enumerate(cells):
                for c, cell_value in enumerate(row):
                    if value is None:
                        v = cell_value
                    else:
                        v = value
                    key = "r%sc%s" % (r + starts_r, c + starts_c)
                    if (ends is None) or \
                       ((ends_r >= r + starts_r) and
                            (ends_c >= c + starts_c)):
                        self.cell(key, v,
                                  active=active,
                                  onchange=onchange,
                                  readonly=readonly,
                                  **attributes)

    def __setitem__(self, key, value):
        key = str(key)
        value = str(value)
        node = self.nodes[key]
        node.value = value
        if value[:1] == '=' and node.active:
            # clear all edges involving current node
            for other_node in node.incoming:
                del other_node.outcoming[node]
            node.incoming.clear()
            # build new edges
            command = self.re_strings.sub("''", value[1:])
            node.locked = False
            for match in self.regex.finditer(command):
                other_key = match.group()
                if other_key == key:
                    self.computed_value = self.error % dict(error='cycle')
                    self.modified = {}
                    break
                if other_key in self.nodes:
                    other_node = self.nodes[other_key]
                    other_node.outcoming[node] = True
                    node.incoming[other_node] = True
                elif not other_key in self.allowed_keywords and \
                        not other_key in self.environment:
                    node.locked = True
                    node.computed_value = \
                        self.error % dict(
                            error='invalid keyword: ' + other_key)
                    self.modified = {}
                    break
            self.compute(node)
        else:
            try:
                node.computed_value = int(node.value)
            except:
                try:
                    node.computed_value = float(node.value)
                except:
                    node.computed_value = node.value
            self.environment[key] = node.computed_value
            if node.onchange:
                node.onchange(node)
        self.modified = self.iterate(node)

    def compute(self, node):
        if node.value[:1] == '=' and not node.locked:
            try:
                exec('__value__=' + node.value[1:], {}, self.environment)
                node.computed_value = self.environment['__value__']
                del self.environment['__value__']
            except Exception, e:
                node.computed_value = self.error % dict(error=str(e))
        self.environment[node.name] = node.computed_value
        if node.onchange:
            node.onchange(node)

    def iterate(self, node):
        output = {node.name: node.computed_value}
        changed_nodes = self.changed(node)
        while changed_nodes:
            ok = False
            set_changed_nodes = set(changed_nodes)
            for (k, other_node) in enumerate(changed_nodes):
                #print other_node, changed_nodes
                if not set(other_node.incoming.keys()).\
                        intersection(set_changed_nodes):
                    #print 'ok'
                    self.compute(other_node)
                    output[other_node.name] = other_node.computed_value
                    #print other_node
                    del changed_nodes[k]
                    ok = True
                    break
            if not ok:
                return {}
        return output

    def __getitem__(self, key):
        return self.nodes[str(key)]

    def get_computed_values(self):
        d = {}
        for key in self.nodes:
            node = self.nodes[key]
            if node.value[:1] != '=' or not node.active:
                d[key] = node.computed_value
        return d

    def set_computed_values(self, d):
        for key in d:
            if not key in self.nodes:
                continue
            node = self.nodes[key]
            if node.value[:1] != '=' or not node.active:
                node.value = d[key]

    def sheet(self):
        import gluon.html
        (DIV, TABLE, TR, TD, TH, BR, SCRIPT) = \
            (gluon.html.DIV, gluon.html.TABLE, gluon.html.TR, gluon.html.TD,
             gluon.html.TH, gluon.html.BR, gluon.html.SCRIPT)
        regex = re.compile('r\d+c\d+')

        header = TR(TH(), *[TH('c%s' % c)
                            for c in range(self.cols)])
        rows = []
        for r in range(self.rows):
            tds = [TH('r%s' % r), ]
            for c in range(self.cols):
                key = 'r%sc%s' % (r, c)
                attributes = {"_class": "w2p_spreadsheet_col_%s" %
                              self.even_or_odd(c)}
                if key in self.td_attributes:
                    attributes.update(self.td_attributes[key])
                td = TD(self.nodes[key], **attributes)
                tds.append(td)
            attributes = {"_class": "w2p_spreadsheet_row_%s" %
                          self.even_or_odd(r)}
            if str(r) in self.tr_attributes:
                attributes.update(self.tr_attributes[str(r)])
            rows.append(TR(*tds, **attributes))

        attributes = {"_class": "w2p_spreadsheet"}
        attributes.update(self.attributes)

        table = TABLE(header, *rows, **self.attributes)

        if len(self.client["cells"]) >= 1:
            data = SCRIPT(
                """
            var w2p_spreadsheet_data = %(data)s;
            var w2p_spreadsheet_update_button = "%(update_button)s";
            var w2p_spreadsheet_update_result = null;
            function w2p_spreadsheet_update_cell(a){
              // update data
              w2p_spreadsheet_data.modified[this.id] = this.value;
            }
            function w2p_spreadsheet_update_db_callback(result){
              w2p_spreadsheet_update_result = result;
              eval(w2p_spreadsheet_update_result.db_callback);
            }
            function w2p_spreadsheet_update_db(){
              // ajax background db update
              jQuery.ajax({url: "%(url)s/data",
                           type: "POST",
                           data:
                             {data: JSON.stringify(w2p_spreadsheet_data)},
                           dataType: "json",
                           success: w2p_spreadsheet_update_db_callback
                           });
            }
            // add onchange cell update event
            jQuery(function(){
              jQuery(".%(name)s input").change(w2p_spreadsheet_update_cell);
            });

            if (w2p_spreadsheet_update_button != ""){
              jQuery(function(){
                jQuery("#" + w2p_spreadsheet_update_button).click(
                    w2p_spreadsheet_update_db);
              });
            }
            """ % dict(data=simplejson.dumps(self.client),
                       name=attributes["_class"],
                       url=self.url,
                       update_button=self.update_button))

            # extra row for fieldnames
            unsorted_headers = []
            if self.client["headers"] is not None:
                for fieldname, name in self.client["headers"].iteritems():
                    unsorted_headers.append((self.client["columns"][fieldname],
                                             name))
            else:
                for fieldname, c in self.client["columns"].iteritems():
                    unsorted_headers.append((c, fieldname))

            sorted_headers = [TH(), ] + \
                [TH(header[1]) for header in sorted(unsorted_headers)]
            table.insert(0, TR(*sorted_headers,
                                **{'_class': "%s_fieldnames" %
                                   attributes["_class"]}))
        else:
            data = SCRIPT(""" // web2py Spreadsheets: no db data.""")

        return DIV(table,
                   BR(),
                   TABLE(*[TR(TH(key), TD(self.nodes[key]))
                           for key in self.nodes if not regex.match(key)]),
                   data, **attributes)

    def xml(self):
        return self.sheet().xml()


if __name__ == '__main__':
    s = Sheet(0, 0)
    s.cell('a', value="2")
    s.cell('b', value="=sin(a)")
    s.cell('c', value="=cos(a)**2+b*b")
    print s['c'].computed_value
