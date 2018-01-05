# -*- coding: utf-8 -*-

N = 1000
THINNING = 1
ALPHA = 1
ISOURCE_RESULTS = 'isource_results'

speedy = True
if speedy:
    import logging
    N = 4
    logging.error('iSource N is set to %s', N)
