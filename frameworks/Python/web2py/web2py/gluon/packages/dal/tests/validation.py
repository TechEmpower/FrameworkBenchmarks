import re
from pydal import DAL, Field
from ._compat import unittest
from ._adapt import DEFAULT_URI, NOSQL, IS_IMAP, drop, IS_GAE


regex_isint = re.compile('^[+-]?\d+$')


def range_error_message(error_message, what_to_enter, minimum, maximum):
    "build the error message for the number range validators"
    if error_message is None:
        error_message = 'Enter ' + what_to_enter
        if minimum is not None and maximum is not None:
            error_message += ' between %(min)g and %(max)g'
        elif minimum is not None:
            error_message += ' greater than or equal to %(min)g'
        elif maximum is not None:
            error_message += ' less than or equal to %(max)g'
    if type(maximum) in [int, long]:
        maximum -= 1
    return error_message % dict(min=minimum, max=maximum)


class IS_INT_IN_RANGE(object):
    def __init__(self, minimum=None, maximum=None, error_message=None):
        self.minimum = int(minimum) if minimum is not None else None
        self.maximum = int(maximum) if maximum is not None else None
        self.error_message = range_error_message(
            error_message, 'an integer', self.minimum, self.maximum)

    def __call__(self, value):
        if regex_isint.match(str(value)):
            v = int(value)
            if ((self.minimum is None or v >= self.minimum) and
                    (self.maximum is None or v < self.maximum)):
                return (v, None)
        return (value, self.error_message)


@unittest.skipIf(IS_IMAP, "TODO: IMAP test")
class TestValidateAndInsert(unittest.TestCase):
    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('val_and_insert',
                        Field('aa'),
                        Field('bb', 'integer',
                              requires=IS_INT_IN_RANGE(1, 5)))
        rtn = db.val_and_insert.validate_and_insert(aa='test1', bb=2)
        if NOSQL:
            self.assertEqual(isinstance(rtn.id, long), True)
        else:
            self.assertEqual(rtn.id, 1)
        #errors should be empty
        self.assertEqual(len(rtn.errors.keys()), 0)
        #this insert won't pass
        rtn = db.val_and_insert.validate_and_insert(bb="a")
        #the returned id should be None
        self.assertEqual(rtn.id, None)
        #an error message should be in rtn.errors.bb
        self.assertNotEqual(rtn.errors.bb, None)
        #cleanup table
        drop(db.val_and_insert)

@unittest.skipIf(IS_IMAP, "TODO: IMAP test")
class TestValidateUpdateInsert(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        t1 = db.define_table('t1', Field('int_level', 'integer', requires=IS_INT_IN_RANGE(1, 5)))
        i_response = t1.validate_and_update_or_insert((t1.int_level == 1), int_level=1)
        u_response = t1.validate_and_update_or_insert((t1.int_level == 1), int_level=2)
        e_response = t1.validate_and_update_or_insert((t1.int_level == 1), int_level=6)
        self.assertTrue(i_response.id != None)
        self.assertTrue(u_response.id != None)
        self.assertTrue(e_response.id == None and len(e_response.errors.keys()) != 0)
        self.assertEqual(len(db(t1).select()), 1)
        self.assertEqual(db(t1).count(), 1)
        self.assertEqual(db(t1.int_level == 1).count(), 0)
        self.assertEqual(db(t1.int_level == 6).count(), 0)
        self.assertEqual(db(t1.int_level == 2).count(), 1)
        drop(db.t1)
        return
