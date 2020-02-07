import os
import os.path
import sys
import json

def process_dir(prefix, target, results):
    files = os.listdir(target)
    for f in files:
        if f.endswith('.js'):
            text = open(os.path.join(os.getcwd(),target,f)).read()
            results[os.path.join(prefix,f)] = text
    return results

if __name__ == '__main__':
    results = {}
    for d in sys.argv[1:]:
        halves = d.split('=')
        process_dir(halves[0], halves[1], results)

    output = ['var imports = imports || {};'];
    for k in results.keys():
        output.append('console.log(%s); imports[%s] = %s;' % (json.dumps(k),json.dumps(k),json.dumps(results[k])))

    print 'let jslibs = %s' % json.dumps('\n'.join(output))
