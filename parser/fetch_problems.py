import urllib2

SITE='http://icfpc2013.cloudapp.net/'
AUTH='AUTHKEYvpsH1H'

if __name__ == '__main__':
    f = urllib2.urlopen(SITE + '/myproblems?auth=' + AUTH)
    print f.read()
