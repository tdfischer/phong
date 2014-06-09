from . import Module, Processor
from event import InternalEvent, IncomingEvent, OutgoingEvent
from util import SQLDatabase

class MarkovDB(SQLDatabase):
  def update(self, current):
    if current == 0:
      with self as c:
        c.execute("CREATE TABLE Markov (word TEXT KEY, nextWord TEXT KEY, frequency INTEGER)")
        c.execute("CREATE UNIQUE INDEX wordPair ON Markov (word, nextWord)")
      return 1
    return current

  def addPair(self, first, second):
    with self as c:
      if (first is None):
        c.execute("SELECT frequency FROM Markov WHERE word IS NULL and nextWord = ?", (second,))
      elif (second is None):
        c.execute("SELECT frequency FROM Markov WHERE word = ? AND nextWord IS NULL", (first,))
      else:
        c.execute("SELECT frequency FROM Markov WHERE word = ? AND nextWord = ?", (first, second))
      res = c.fetchone()
      if (res == None):
        c.execute("INSERT INTO Markov (word, nextWord, frequency) VALUES (?, ?, 0)", (first, second))
      if (first is None):
        c.execute("UPDATE Markov SET frequency = frequency+1 WHERE word IS NULL AND nextWord = ?", (second,))
      elif (second is None):
        c.execute("UPDATE Markov SET frequency = frequency+1 WHERE word = ? AND nextWord IS NULL", (first,))
      else:
        c.execute("UPDATE Markov SET frequency = frequency + 1 WHERE word = ? AND nextWord = ?", (first, second))

  def nextWord(self, current):
    with self as c:
      if (current is None):
        c.execute("SELECT nextWord FROM Markov WHERE word IS NULL ORDER BY RANDOM() * frequency LIMIT 1")
      else:
        c.execute("SELECT nextWord FROM Markov WHERE word = ? ORDER BY RANDOM() * frequency LIMIT 1", (current,))
      res = c.fetchone()
      if (res == None):
          return None
      return res[0]

  def buildReply(self, word):
    if (word != None):
      phrase = (word,)
    else:
      phrase = ()
    current = self.nextWord(word)
    while(current != None):
      phrase += (current,)
      current = self.nextWord(current)
    return ' '.join(phrase)

class Analyzer(Processor):
  def __init__(self, db, module):
    super(Analyzer, self).__init__(module)
    self._db = db

  def priority(self, event):
    if 'words' in event and not event.seen(self) and isinstance(event, IncomingEvent):
      return 0
  
  def process(self, event):
    self._log.debug("Saving chains from %s", event)
    prev = None
    for word in event['words']:
      self._db.addPair(prev, word)
      prev = word
    self._db.addPair(word, None)

class Answerer(Processor):
  def __init__(self, db, module):
    super(Answerer, self).__init__(module)
    self._db = db

  def priority(self, event):
    self._log.debug("Testing %s", event)
    if 'subject' in event and isinstance(event, IncomingEvent) and not event.seen(self) and event['to-self'] and event['is-question']:
      return 100

  def process(self, event):
    self._log.info("Producing response to question %s", event)
    reply = self._db.buildReply(str(event['subject']))
    self.enqueueEvent(OutgoingEvent(event, text=reply))

class Producer(Processor):
  def __init__(self, db, module):
    super(Producer, self).__init__(module)
    self._db = db

  def priority(self, event):
    if 'subject' in event and isinstance(event, InternalEvent) and not event.seen(self):
      return 1

  def process(self, event):
    self._log.debug("Producing phrase for %s", event)
    reply = self._db.buildReply(str(event['subject']))
    self.enqueueEvent(OutgoingEvent(event, text=reply))

class Markov(Module):
  def __init__(self, ai):
    super(Markov, self).__init__(ai)
    self._db = MarkovDB(self._ai.personality.filename(self, 'markov.sqlite3'))
    self.addProcessor(Analyzer(self._db, self))
    self.addProcessor(Producer(self._db, self))
    self.addProcessor(Answerer(self._db, self))
