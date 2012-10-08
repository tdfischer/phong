from . import Module, Processor
from event import InstrumentationEvent
import threading

class ThreadCount(Processor):
  def priority(self, event):
    if isinstance(event, InstrumentationEvent) and not event.seen(self):
      return -100

  def process(self, event):
    self._log.debug("Thread count: %d", threading.activeCount())
    self._log.debug("Threads: %s", threading.enumerate())

class Debug(Module):
  def __init__(self, ai):
    super(Debug, self).__init__(ai)
    self.addProcessor(ThreadCount())
