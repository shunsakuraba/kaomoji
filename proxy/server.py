from BaseHTTPServer import HTTPServer, BaseHTTPRequestHandler
from SocketServer import ThreadingMixIn
import threading
import datetime
import time
import urllib2

def now():
  return datetime.datetime.now()

MIN_WAIT = 5
SITE='http://icfpc2013.cloudapp.net/'
AUTH='AUTHKEYvpsH1H'

request_lock = threading.Lock()
dic_lock = threading.Lock()
global_dic = {}

class Handler(BaseHTTPRequestHandler):
  def get_lock_and_wait(self, is_priority_request):
    # get lock and wait until I can send request
    while True:
      request_lock.acquire()
      if is_priority_request:
        dic_lock.acquire()
        global_dic['priority_request_count'] -= 1
        dic_lock.release()
        break
      else:
        dic_lock.acquire()
        priority_request_count = global_dic['priority_request_count']
        dic_lock.release()
        if priority_request_count == 0:
          break;
      request_lock.release()

    dic_lock.acquire()
    last_request = global_dic['last_request']
    dic_lock.release()

    wait = datetime.timedelta(seconds = MIN_WAIT)
    while (now() - last_request) < wait:
      print "waiting for request: " + self.path
      dic_lock.acquire()
      print global_dic
      dic_lock.release()
      time.sleep(0.5)

  def process(self, is_post):
    # check if it's prirority request
    is_priority_request = False
    if "/guess" in self.path:
      is_priority_request = True
      dic_lock.acquire()
      global_dic['priority_request_count'] += 1
      dic_lock.release()

    # get lock and wait
    self.get_lock_and_wait(is_priority_request)

    # send request
    try:
      path = self.path
      if "auth=" not in path:
        path = path + "?auth=" + AUTH

      req = None
      if is_post:
        content_len = int(self.headers.getheader('content-length'))
        data = self.rfile.read(content_len)
        req = urllib2.Request(SITE + path, data = data, headers = self.headers)
      else:
        req = urllib2.Request(SITE + path, headers = self.headers)

      dic_lock.acquire()
      global_dic['last_request'] = now()
      dic_lock.release()

      f = urllib2.urlopen(req)
      self.send_response(200)
      self.end_headers()
      self.wfile.write(f.read())
    except urllib2.HTTPError, e:
      self.send_error(e.code, e.msg)
    except IOError, e:
      print "IOError:" + str(e)
    # except Exception, e:
    #  print str(e)
    request_lock.release()
    return

  def do_GET(self):
    self.process(False)

  def do_POST(self):
    self.process(True)


class ThreadedHTTPServer(ThreadingMixIn, HTTPServer):
  pass

if __name__ == '__main__':
  server = ThreadedHTTPServer(('', 80), Handler)

  global_dic['last_request'] = now()
  global_dic['priority_request_count'] = 0

  server.serve_forever()
