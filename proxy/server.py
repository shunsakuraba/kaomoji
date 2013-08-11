from BaseHTTPServer import HTTPServer, BaseHTTPRequestHandler
from SocketServer import ThreadingMixIn
import threading
import datetime
import time
import urllib2

def now():
  return datetime.datetime.now()

MIN_WAIT = 5
LAST_REQUEST = 'last_request'
PRIORITY_COUNT = 'priority_count'
SITE='http://icfpc2013.cloudapp.net/'
AUTH='AUTHKEYvpsH1H'

dic_lock = threading.Lock()
global_dic = {}

class Handler(BaseHTTPRequestHandler):
  def get_lock_and_wait(self, is_priority_request):
    wait = datetime.timedelta(seconds = MIN_WAIT)
    while True:
      dic_lock.acquire()
      if is_priority_request or global_dic[PRIORITY_COUNT] == 0:
        if (now() - global_dic[LAST_REQUEST]) > wait:
          global_dic[LAST_REQUEST] = now()
          if is_priority_request:
            global_dic[PRIORITY_COUNT] -= 1
          print global_dic
          dic_lock.release()
          break
        print "waiting for request: " + self.path
        print global_dic
      dic_lock.release()
      time.sleep(0.5)

  def process(self, is_post):
    # check if it's prirority request
    is_priority_request = False
    if "/guess" in self.path:
      is_priority_request = True
      dic_lock.acquire()
      global_dic[PRIORITY_COUNT] += 1
      dic_lock.release()

    # get lock and wait
    self.get_lock_and_wait(is_priority_request)

    # send request
    try:
      path = self.path
      if "auth=" not in path:
        path = path + "?auth=" + AUTH

      dic_lock.acquire()
      global_dic[LAST_REQUEST] = now()
      dic_lock.release()

      req = None
      if is_post:
        content_len = int(self.headers.getheader('content-length'))
        data = self.rfile.read(content_len)
        req = urllib2.Request(SITE + path, data = data, headers = self.headers)
      else:
        req = urllib2.Request(SITE + path, headers = self.headers)

      f = urllib2.urlopen(req)
      self.send_response(200)
      self.end_headers()
      self.wfile.write(f.read())
    except urllib2.HTTPError, e:
      self.send_error(e.code, e.msg)
    except IOError, e:
      print "IOError:" + str(e)
    except Exception, e:
      print str(e)

    return

  def do_GET(self):
    self.process(False)

  def do_POST(self):
    self.process(True)


class ThreadedHTTPServer(ThreadingMixIn, HTTPServer):
  pass

if __name__ == '__main__':
  server = ThreadedHTTPServer(('', 8080), Handler)

  global_dic[LAST_REQUEST] = now()
  global_dic[PRIORITY_COUNT] = 0

  server.serve_forever()
