use std::io;
use std::io::stdout;
use std::sync::Arc;

use crossterm::execute;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};
use tokio::sync::RwLock;

use crate::ast::{Rule, Term};
use crate::parser::parse_term;
use crate::view::{State, View};

type Sender = UnboundedSender<Vec<Term>>;
type Receiver = UnboundedReceiver<Vec<Term>>;

pub struct Interactive {
  view:View,
  should_quit:bool,
  state:State,
}

fn die(e:&str) -> ! {
  std::mem::drop(execute!(stdout(),
                          crossterm::terminal::Clear(crossterm::terminal::ClearType::All)));
  std::mem::drop(crossterm::terminal::disable_raw_mode());
  panic!("{}", e)
}

impl Interactive {
  /// # Errors
  ///
  /// Will return `Err` if there was an error constructing a terminal
  pub fn new() -> io::Result<Self> {
    crossterm::terminal::enable_raw_mode()?;
    let should_quit = false;
    let rule = Arc::new(RwLock::new(Rule { pat:vec![], rep:vec![] }));
    let state = State::AtLeft;
    let default_status = "mlatu interface".to_string();
    let view = View::new(Arc::clone(&rule),
                         ("| Input |".to_string(), "| Output |".to_string()),
                         default_status)?;
    Ok(Self { view, should_quit, state })
  }

  /// # Panics
  ///
  /// Panics if there was an error displaying to the screen, an error processing
  /// keypresses, or an error handling the Prolog interface.
  pub async fn run(&mut self, sender:Sender, mut receiver:Receiver) {
    loop {
      if let Err(error) = self.view.refresh_screen(&self.state, self.should_quit).await {
        die(&error.to_string());
      }
      if self.should_quit {
        let _result = crossterm::terminal::disable_raw_mode();
        break
      }
      tokio::select! {
        res = receiver.recv() => {
            if let Some(res) = res {
              let mut guard = self.view.write().await;
              guard.rep = res;
            } else {
              die("erlang handler unexpectedly disconnected")
            }
        }
        res = self.process_keypress(sender.clone()) => {
          // is taken
          if let Err(error) = res {
            die(&error.to_string());
          }
        }
      }
    }
  }

  async fn remove(&mut self, sender:Sender, index:usize) {
    let mut guard = self.view.write().await;
    guard.pat.remove(index);
    self.state = if guard.pat.is_empty() {
      State::AtLeft
    } else {
      State::InLeft(index.min(guard.pat.len() - 1))
    };
    sender.send(guard.pat.clone()).expect("send terms");
  }

  async fn quote(&mut self, sender:Sender, index:usize) {
    let mut guard = self.view.write().await;
    guard.pat[index] = Term::new_quote(vec![guard.pat[index].clone()]);
    sender.send(guard.pat.clone()).expect("send terms");
  }

  async fn swap(&mut self, sender:Sender, index:usize) {
    let mut guard = self.view.write().await;
    if index > 0 {
      guard.pat.swap(index, index - 1);
      sender.send(guard.pat.clone()).expect("send terms");
    }
  }

  async fn dup(&mut self, sender:Sender, index:usize) {
    let mut guard = self.view.write().await;
    let term = guard.pat[index].clone();
    guard.pat.insert(index, term);
    sender.send(guard.pat.clone()).expect("send terms");
  }

  async fn concat(&mut self, sender:Sender, index:usize) {
    let mut guard = self.view.write().await;
    if let Term::Quote(mut terms) = guard.pat[index].clone() {
      if let Some(Term::Quote(other_terms)) = guard.pat.get(index + 1) {
        terms.extend(other_terms.clone());
        guard.pat.remove(index);
        guard.pat[index] = Term::new_quote(terms);
        sender.send(guard.pat.clone()).expect("send terms");
      }
    }
  }

  async fn unquote(&mut self, sender:Sender, index:usize) {
    let mut guard = self.view.write().await;
    if let Term::Quote(terms) = guard.pat[index].clone() {
      guard.pat.remove(index);
      let mut i = index;
      for term in terms {
        guard.pat.insert(i, term);
        i += 1;
      }
      self.state = if guard.pat.is_empty() {
        State::AtLeft
      } else {
        State::InLeft(index.min(guard.pat.len() - 1))
      };
      sender.send(guard.pat.clone()).expect("send terms");
    }
  }

  async fn process_keypress(&mut self, sender:Sender) -> io::Result<()> {
    use crossterm::event::KeyCode::{Backspace, Char, Delete, Down, Esc, Up};

    let event = self.view.read_key().await?;
    match (event.code, event.modifiers) {
      | (Esc, _) => self.should_quit = true,
      | (Char(c), _) => match (c, self.state.clone()) {
        | (' ', State::Editing(s, state)) =>
          if let Ok(term) = parse_term(&s) {
            let mut guard = self.view.write().await;
            match *state {
              | State::AtLeft => {
                guard.pat = vec![term];
                self.state = State::InLeft(0);
                sender.send(guard.pat.clone()).expect("send terms");
              },
              | State::InLeft(index) => {
                guard.pat.insert(index, term);
                self.state = State::InLeft(index);
                sender.send(guard.pat.clone()).expect("send terms");
              },
              | _ => {},
            }
          },
        | (c, State::Editing(s, state)) => {
          let mut s = s;
          s.push(c);
          self.state = State::Editing(s, state);
        },
        | ('r', State::InLeft(index)) => self.remove(sender, index).await,
        | ('q', State::InLeft(index)) => self.quote(sender, index).await,
        | ('s', State::InLeft(index)) => self.swap(sender, index).await,
        | ('d', State::InLeft(index)) => self.dup(sender, index).await,
        | ('c', State::InLeft(index)) => self.concat(sender, index).await,
        | ('u', State::InLeft(index)) => self.unquote(sender, index).await,
        | (' ', State::InLeft(index)) =>
          self.state = State::Editing(String::new(), Box::new(State::InLeft(index))),
        | (' ', State::AtLeft) =>
          self.state = State::Editing(String::new(), Box::new(State::AtLeft)),
        | _ => {},
      },
      | (Backspace | Delete, _) => match self.state.clone() {
        | State::Editing(s, state) => {
          let mut s = s;
          s.pop();
          self.state = State::Editing(s, state);
        },
        | _ => {},
      },
      | (Up, _) => match self.state {
        | State::InLeft(index) => {
          let guard = self.view.read().await;
          self.state = if guard.pat.is_empty() {
            State::AtLeft
          } else {
            State::InLeft((index + 1).min(guard.pat.len() - 1))
          };
        },
        | _ => {},
      },
      | (Down, _) => match self.state {
        | State::InLeft(index) => {
          let guard = self.view.read().await;
          self.state = if guard.pat.is_empty() {
            State::AtLeft
          } else {
            State::InLeft((index.saturating_sub(1)).min(guard.pat.len() - 1))
          };
        },
        | _ => {},
      },
      | _ => {},
    }
    Ok(())
  }
}
