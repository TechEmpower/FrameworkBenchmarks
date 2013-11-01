# encoding: utf-8

import unittest
import json
import sys

class FunctionalTests(unittest.TestCase):
    def setUp(self):
        from frameworkbenchmarks import main
        app = main({})
        from webtest import TestApp
        self.testapp = TestApp(app)
        self.py3k = sys.version_info >= (3, 0)

    def _get(self, url, content_type='application/json'):
        res = self.testapp.get(url, status=200)
        self.assertTrue('Content-Length' in res.headers)
        # apparently these are all set by waitress, and so not
        # available for testing here...
        # self.assertTrue('Server' in res.headers)
        # self.assertTrue('Date' in res.headers)
        # self.assertTrue(content_type in res.headers['Content-Type'])
        return res

    def _str_compat(self, obj):
        if self.py3k:
            return obj.decode('utf-8')
        return obj


    def _test_obj(self, obj):
        self.assertTrue('id' in obj)
        self.assertTrue('randomNumber' in obj)
        self.assertTrue(1 <= obj['randomNumber'] <= 10000)

    def test_json(self):
        """
        /json
        """
        res = self._get('/json')
        self.assertEqual(self._str_compat(res.body), """{"message": "Hello, World!"}""")

    def test_db(self):
        """
        /db
        """
        res = self._get('/db')
        obj = json.loads(self._str_compat(res.body))
        self._test_obj(obj)

    def test_queries_0(self):
        """
        /queries?queries=0
        """
        res = self._get('/queries?queries=0')
        self.assertEqual(len(json.loads(self._str_compat(res.body))), 1)

    def test_queries_999(self):
        """
        /queries?queries=999
        """
        res = self._get('/queries?queries=999')
        self.assertEqual(len(json.loads(self._str_compat(res.body))), 500)

    def test_queries_999(self):
        """
        /queries?queries=10 objects
        """
        res = self._get('/queries?queries=10')
        objset = json.loads(self._str_compat(res.body))
        for obj in objset:
            self._test_obj(obj)

    def test_fortunes(self):
        """
        /fortunes
        """
        res = self._get('/fortunes')
        self.assertEqual(self._str_compat(res.body).strip(), fortunes.strip())

    def test_updates(self):
        """
        /updates?queries=10
        """
        res = self._get('/updates?queries=10')
        objset = json.loads(self._str_compat(res.body))
        # don't bother with more...
        for obj in objset:
            self._test_obj(obj)

    def test_plaintext(self):
        """
        /plaintext
        """
        res = self._get('/plaintext', content_type='text/plain')
        self.assertEqual(self._str_compat(res.body), "Hello, World!")


fortunes = """
<!DOCTYPE html>
<html>
<head>
    <title>Fortunes</title>
</head>
<body>
    <table>
    <tr>
        <th>id</th>
        <th>message</th>
    </tr>
    <tr>
        <td>11</td>
        <td>&lt;script&gt;alert("This should not be displayed in a browser alert box.");&lt;/script&gt;</td>
    </tr>
    <tr>
        <td>4</td>
        <td>A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1</td>
    </tr>
    <tr>
        <td>5</td>
        <td>A computer program does what you tell it to do, not what you want it to do.</td>
    </tr>
    <tr>
        <td>2</td>
        <td>A computer scientist is someone who fixes things that aren't broken.</td>
    </tr>
    <tr>
        <td>8</td>
        <td>A list is only as strong as its weakest link. â€” Donald Knuth</td>
    </tr>
    <tr>
        <td>0</td>
        <td>Additional fortune added at request time.</td>
    </tr>
    <tr>
        <td>3</td>
        <td>After enough decimal places, nobody gives a damn.</td>
    </tr>
    <tr>
        <td>7</td>
        <td>Any program that runs right is obsolete.</td>
    </tr>
    <tr>
        <td>10</td>
        <td>Computers make very fast, very accurate mistakes.</td>
    </tr>
    <tr>
        <td>6</td>
        <td>Emacs is a nice operating system, but I prefer UNIX. â€” Tom Christaensen</td>
    </tr>
    <tr>
        <td>9</td>
        <td>Feature: A bug with seniority.</td>
    </tr>
    <tr>
        <td>1</td>
        <td>fortune: No such file or directory</td>
    </tr>
    <tr>
        <td>12</td>
        <td>フレームワークのベンチマーク</td>
    </tr>
    </table>
</body>
</html>
"""
