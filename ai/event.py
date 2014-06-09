import time
import weakref
import traceback

class Event(object):
  def __init__(self, *args, **kwargs):
    self._meta = {}
    self.start = time.time()
    self._seenBy = []
    self._parents = []
    self._stack = traceback.extract_stack()
    for p in args:
      if isinstance(p, Event):
        self._parents.append(weakref.ref(p, self._parentUnref))
      else:
        raise TypeError, "Event() only accepts other Event objects"

    for k,v in kwargs.iteritems():
      self.addMeta(k, v)

  def _parentUnref(self, ref):
    e = ref()
    for k,v in e._meta.iteritems():
      if k not in self._meta:
        self._meta[k] = []
      self._meta[k].insert(v, 0)

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
    if key in self._meta:
      return self._meta[key]
    for p in map(lambda x:x(), self._parents):
      if p and p.hasMeta(key):
        return p.getMeta(key)
    return [None]

  def hasMeta(self, key):
    if key in self._meta:
      return True
    for p in map(lambda x:x(), self._parents):
      if p and p.hasMeta(key):
        return True
    return False

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
    return self.hasMeta(key)

  def __setattr__(self, name, value):
    if name.startswith("_"):
      self.__dict__[name] = value
    else:
      self.addMeta(name, value)

  def __repr__(self):
    return self.__str__()

  def __str__(self):
    meta = {}
    for p in map(lambda x:x(), self._parents)+[self,]:
      if p:
        for m in p._meta:
          if m.startswith("_"):
            continue
          meta[m] = p._meta[m][0]
    frame = self._stack[-2]
    frameinfo = "%s:%s"%(frame[0], frame[1])
    return "%s(parents=%s, %r, __frame__=%s)"%(self.__class__.__name__,
        self._parents, meta, frameinfo)

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
