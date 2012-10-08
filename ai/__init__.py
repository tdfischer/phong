import threading
import os
from event import StopEvent, StartEvent, Event, IdleEvent
import logging
import time
import Queue

class Module(object):
  def __init__(self, ai):
    super(Module, self).__init__()
    self._ai = ai
    self._log = logging.getLogger("phong.ai.modules.%s"%(self.__class__.__name__))
    self._processors = []

  def addProcessor(self, p):
    self._processors.append(p)

  @property
  def processors(self):
    return self._processors

class Processor(object):
  def __init__(self, module):
    super(Processor, self).__init__()
    self._log = logging.getLogger("phong.ai.processors.%s"%(self.__class__.__name__))
    self._next = None
    self._module = module

  def enqueueEvent(self, event):
    self._module._ai.processEvent(event)

  def priority(self, event):
    return 0

  def process(self, event):
    pass

  def stop(self, event):
    pass

  def start(self, event):
    pass

class Personality(object):
  def __init__(self, name, datadir):
    self.name = name
    self.datadir = datadir

  def filename(self, module, name):
    dir = '/'.join((self.datadir, module.__class__.__name__))
    if not os.path.exists(dir):
      os.makedirs('/'.join((self.datadir, module.__class__.__name__)))
    return '/'.join((dir, name))

class AI(object):
  def __init__(self, personality, threadCount=3):
    assert(isinstance(personality, Personality))
    self._log = logging.getLogger("phong.ai")
    self._personality = personality
    self._events = Queue.Queue()
    self._threads = []
    for i in xrange(0, 3):
      self._threads.append(threading.Thread(target=self._run, name="Processor Dispatcher %i"%(i)))
    self._modules = []

  @property
  def personality(self):
    return self._personality

  def loadModule(self, m):
    self._modules.append(m)

  def _run(self):
    while True:
      e = self._events.get()
      self._log.debug("Got event %s", e)
      if isinstance(e, StartEvent):
        for m in self._modules:
          for p in m.processors:
            try:
              p.start(e)
            except:
              logging.exception("Error during startup of %s", p)
        continue
      if isinstance(e, StopEvent):
        self._log.info("Got StopEvent, quitting!")
        for m in self._modules:
          for p in m.processors:
            try:
              p.stop(e)
            except:
              logging.exception("Error while processing stop event in %s", p)
        self.processEvent(e)
        return
      self._process(e)

  def _process(self, event):
    while time.time()-event.start < 3:
      runOrder = []
      for m in self._modules:
        for p in m.processors:
          priority = None
          try:
            priority = p.priority(event)
          except:
            self._log.exception("Error while determining priority with %s/%s", m, p)
          if priority is not None:
            runOrder.append((priority, p))
      if len(runOrder) == 0:
        self._log.debug("Lost interest in %s.", event)
        return
      runOrder.sort(key=lambda x:x[0])
      for p in runOrder:
        self._log.debug("Calling %s", p)
        ret = None
        try:
          ret = p[1].process(event)
          event.markSeen(p[1])
        except:
          self._log.exception("Error while processing with %s", p[1])
          pass
        if ret is True:
          self._log.debug("Got True back from %s, halting processing.", p)
          return
    self._log.debug("Got bored of %s.", event)

  def run(self):
    self.processEvent(StartEvent())
    for t in self._threads:
      t.start()

  def join(self):
    for t in self._threads:
      t.join()

  def stop(self):
    self.processEvent(StopEvent())

  def processEvent(self, event):
    self._log.debug("Starting to think about %s", event)
    self._events.put(event)
