import sqlite3
import threading

class SQLDatabase(object):
  def __init__(self, path):
    self.path = path
    self._dbs = {}

  def open(self):
    thread = threading.currentThread()
    if thread in self._dbs:
      return self._dbs[thread]
    self._dbs[thread] = sqlite3.connect(self.path)
    db = self._dbs[thread]
    v = self._version()

    while True:
      v = self.update(v)
      if v == self._version():
        break
      with self as c:
        c.execute("PRAGMA user_version = %i"%(v))
    return db

  def _version(self):
    with self as c:
      c.execute("PRAGMA user_version")
      return c.fetchone()[0]

  def __enter__(self):
    db = self.open()
    db.__enter__()
    return db.cursor()

  def __exit__(self, *args):
    db = self.open()
    db.__exit__(*args)

  def update(self, current):
    return 0
