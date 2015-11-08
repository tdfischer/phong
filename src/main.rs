extern crate irc;
extern crate markov;
extern crate rand;
extern crate regex;

use irc::client::prelude::*;
use markov::Chain;
use rand::Rng;
use std::fs::File;
use std::io::BufReader;
use std::io::BufRead;
use regex::Regex;

fn main() {
  let config = Config {
    nickname: Some(format!("gnohp")),
    server: Some(format!("chat.freenode.net")),
    channels: Some(vec![format!("#phong")]),
    use_ssl: Some(true),
    port: Some(6697),
    .. Default::default()
  };
  let server = IrcServer::from_config(config).unwrap();
  let mut rng = rand::thread_rng();
  let mut chain = Chain::new();
  chain.order(2);

  let f = File::open("corpus/#the-oob.01-Fri-2015.log").ok().unwrap();
  let reader = BufReader::new(f);
  let re = Regex::new(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.+<.+> (.*)").unwrap();
  for line in reader.lines() {
    let l = line.unwrap();
    if let Some(cap) = re.captures(&l) {
      chain.feed_str(cap.at(1).unwrap());
    }
  }

  server.identify().unwrap();

  for message in server.iter() {
    let message = message.unwrap();
    print!("{}", message.into_string());
    if &message.command[..] == "PRIVMSG" {
      if let Some(msg) = message.suffix {
        let tokens: Vec<&str> = msg.split_whitespace().collect();
        if tokens[0] == "gnohp:" {
          if tokens[1] == "markov" {
            server.send_privmsg(&message.args[0], &chain.generate_str()).unwrap();
          } else {
            server.send_privmsg(&message.args[0], "idk what you mean! :(").unwrap();
          }
        } else {
          chain.feed_str(&msg);
          if rng.gen_weighted_bool(50) {
            server.send_privmsg(&message.args[0], &chain.generate_str()).unwrap();
          }
        }
      }
    }
  }
}
