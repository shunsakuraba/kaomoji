import json
import urllib2
import time
import sys

SITE='http://icfpc2013.cloudapp.net/'
AUTH='AUTHKEYvpsH1H'

if __name__ == '__main__':
    while True:
        request = json.dumps({"size": 6})

        f = urllib2.urlopen(SITE + '/train?auth=' + AUTH,
                            request)
        print f.read()
        sys.stdout.flush()

        time.sleep(5)
