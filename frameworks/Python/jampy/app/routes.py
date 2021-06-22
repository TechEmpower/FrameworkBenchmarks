# -*- coding: utf-8 -*-

routes_in = [
    ('/standard/$anything', '/standard/default/index/$anything'),
    # Disable sessions on the /optimized route for additional speed-up.
    ('/optimized/$anything', '/optimized/default/$anything', dict(web2py_disable_session=True, web2py_disable_garbage_collect=True))
]
