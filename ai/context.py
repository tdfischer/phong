from . import Module, Processor
from event import InternalEvent, OutgoingEvent
import wordnet
import nltk

class Word(object):
  def __init__(self, word, previous, next, data):
    self.word = word
    self.prev = previous
    self.next = next
    self.data = data

  def __str__(self):
    return self.word

  def __repr__(self):
    return "Word(%s, %r, %s)"%(self.prev, self.word, self.next)

class NLTK(Processor):
  def priority(self, event):
    if 'text' in event and not event.seen(self):
      return -1

  def start(self, event):
    grammar = "NP: {<DT>?<JJ>*<NN>}"
    self._cp = nltk.RegexpParser(grammar)

  def process(self, event):
    tokens = nltk.word_tokenize(event['text'])
    event['words'] = tokens
    tags = nltk.pos_tag(tokens)
    event['structure'] = self._cp.parse(tags)
    if not isinstance(event, OutgoingEvent):
      for phrase in event['structure'].subtrees():
        for word in phrase:
          if len(word) == 2 and word[1] == 'NN':
            self.enqueueEvent(InternalEvent(event, subject=word[0]))

class Questions(Processor):
  def priority(self, event):
    if 'text' in event and not event.seen(self):
      return 0

  def process(self, event):
    if 'text' in event:
      if event['text'].endswith('?'):
        event['is-question'] = True
        self._log.debug(event['structure'])

class DirectedAtSelf(Processor):
  def priority(self, event):
    if 'recipient' in event and not event.seen(self):
      return 0

  def process(self, event):
    if event['recipient'] == self._module._ai.personality.name:
      event['to-self'] = True
    else:
      event['to-self'] = False

class StopRepeating(Processor):
  def start(self, event):
    self._subjects= []

  def priority(self, event):
    if isinstance(event, OutgoingEvent):
      return 0

  def process(self, event):
    if not event['subject'] in self._subjects:
      self._subjects.append(event['subject'])
      while len(self._subjects) > 7:
        self._subjects.pop()
    else:
      return True

class Wordnet(Processor):
  def priority(self, event):
    if 'text' in event and not event.seen(self):
      return -1

  def process(self, event):
    words = []
    prev = None
    for word in event['text'].split():
      wordDef = []
      for d in wordnet.Dictionaries:
        try:
          wordDef.append(d[str(word)].getSenses())
        except KeyError:
          pass
      cur = Word(word, prev, None, wordDef)
      if prev:
        prev.next = cur
      words.append(cur)
      prev = cur
    event['wordnet'] = words

class Context(Module):
  def __init__(self, ai):
    super(Context, self).__init__(ai)
    self.addProcessor(Wordnet(self))
    self.addProcessor(NLTK(self))
    self.addProcessor(StopRepeating(self))
    self.addProcessor(Questions(self))
    self.addProcessor(DirectedAtSelf(self))
