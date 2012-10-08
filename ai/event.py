import time

class Event(object):
  def __init__(self):
    self._meta = {}
    self.start = time.time()
    self._seenBy = []

  def seen(self, module):
    return module in self._seenBy

  def markSeen(self, module):
    if not self.seen(module):
      self._seenBy.append(module)

  def addMeta(self, key, value):
    if key not in self._meta:
      self._meta[key] = []
    self._meta[key].append(value)

  def getMeta(self, key):
    if key not in self._meta:
      return [None]
    return self._meta[key]

  def __getitem__(self, key):
    return self.getMeta(key)[0]

  def __setitem__(self, key, value):
    self.addMeta(key, value)

  def __getattr__(self, name):
    if name.startswith("_"):
      return self.__dict__[name]
    return self.getMeta(name)[0]

  def __contains__(self, key):
    if key.startswith("_"):
      return key in self.__dict__
    return key in self._meta

  def __setattr__(self, name, value):
    if name.startswith("_"):
      self.__dict__[name] = value
    else:
      self.addMeta(name, value)

  def __str__(self):
    meta = {}
    for m in self._meta:
      if m.startswith("_"):
        continue
      meta[m] = self._meta[m][0]
    return "%s(%r)"%(self.__class__.__name__, meta)

class InternalEvent(Event):
  pass
class IncomingEvent(Event):
  pass
class OutgoingEvent(Event):
  pass

class StartEvent(Event):
  pass

class StopEvent(Event):
  pass

class IdleEvent(Event):
  pass

class InstrumentationEvent(Event):
  pass
