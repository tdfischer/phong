#!/usr/bin/env python
import logging
import sys
from ai import AI, Personality, Module, Processor
from ai.event import IncomingEvent, InstrumentationEvent, OutgoingEvent, InternalEvent, IdleEvent
from ai.debug import Debug
from ai.markov import Markov
from ai.context import Context

class PrintProcessor(Processor):
  def priority(self, event):
    if isinstance(event, OutgoingEvent) and not event.seen(self):
      return 1000

  def process(self, event):
    print 'I say:', event['text']

class ConsoleFeedback(Module):
  def __init__(self, ai):
    super(ConsoleFeedback, self).__init__(ai)
    self.addProcessor(PrintProcessor(self))

def main():
  personality = Personality('Test', '/tmp/test-ai/')
  ai = AI(personality)
  ai.loadModule(Context(ai))
  ai.loadModule(Markov(ai))
  ai.loadModule(ConsoleFeedback(ai))
  ai.run()
  while True:
    ai.processEvent(InstrumentationEvent())
    if sys.stdin.isatty():
      sys.stdout.write("> ")
    try:
      line = sys.stdin.readline()
    except KeyboardInterrupt:
      ai.stop()
      return
    event = IncomingEvent()
    event['text'] = line.strip()
    ai.processEvent(event)

if __name__ == "__main__":
  logging.basicConfig(level=logging.INFO)
  main()
