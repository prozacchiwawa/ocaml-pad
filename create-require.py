import os
import os.path
import sys
import hashlib
import json

def computehash(s):
    m = hashlib.sha256()
    m.update(s)
    return 'module_' + m.hexdigest()

def process_dir(source, target, assigns, results):
    files = os.listdir(target)
    for f in files:
        if f.endswith('.js'):
            text = open(os.path.join(os.getcwd(),target,f)).read()
            hashed_text = computehash(text)
            assigns[hashed_text] = text
            
            results[f] = hashed_text
    return results

if __name__ == '__main__':
    assigns = {}
    results = {}
    for d in sys.argv[1:]:
        halves = d.split('=')
        filepath = halves[0].split('/')
        filename = filepath[-1]
        process_dir(filename, halves[1], assigns, results)

    output = ['var imports = imports || {};'];
    for k in assigns.keys():
        output.append('var %s = %s;' % (k, json.dumps(assigns[k])))
    for k in results.keys():
        output.append('imports[%s] = %s;' % (json.dumps(k),results[k]))

    print 'let jslibs = %s' % json.dumps('\n'.join(output))
