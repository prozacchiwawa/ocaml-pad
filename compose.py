import os
import json

compose = open('compose.html').read()
javascript = json.dumps(open('bundle.js').read()).replace('</','<\\/')
output = compose.replace('@@bundle@@','eval(%s)' % javascript)
bf = open('bundle.html','w')
bf.write(output)
bf.close()
