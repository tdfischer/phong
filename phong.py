from ircbot import SingleServerIRCBot

class Phong(SingleServerIRCBot):
  def __init__(self):
    super(self, Phong).__init__([('chat.freenode.net', 6667)], 'Phoo', 'Phoo')

  def on_nicknameinuse(self, c, e):
    c.nick(c.get_nickname() + "_")

  def on_welcome(self, c, e):
    c.join('#phong')

