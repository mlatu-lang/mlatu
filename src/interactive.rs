use std::io;
use std::sync::Arc;

use tokio::sync::mpsc::error::TryRecvError;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};
use tokio::sync::RwLock;

use crate::ast::Term;
use crate::parser::parse_term;
use crate::view::{State, View};

pub struct Interactive {
  view:View,
  should_quit:bool,
  state:State,
  tx:UnboundedSender<Vec<Term>>,
}

fn die(e:&io::Error) -> ! {
  let _result = crossterm::terminal::Clear(crossterm::terminal::ClearType::All);
  let _result = crossterm::terminal::disable_raw_mode();
  panic!("{}", e)
}

impl Interactive {
  /// # Errors
  ///
  /// Will return `Err` if there was an error constructing a terminal
  pub fn new(tx:UnboundedSender<Vec<Term>>) -> io::Result<Self> {
    crossterm::terminal::enable_raw_mode()?;
    let should_quit = false;
    let rule = Arc::new(RwLock::new((vec![], vec![])));
    let state = State::AtLeft;
    let default_status = "mlatu interface".to_string();
    let view = View::new(Arc::clone(&rule),
                         ("| Input |".to_string(), "| Output |".to_string()),
                         default_status)?;
    Ok(Self { view, should_quit, state, tx })
  }

  async fn handle_prolog(rx:&mut UnboundedReceiver<Result<Vec<Term>, String>>)
                         -> Option<Vec<Term>> {
    match rx.try_recv() {
      | Ok(Ok(terms)) => Some(terms),
      | Ok(Err(_error)) => {
        // TODO: proper error reporting here, dying is painful
        // die(&io::Error::new(io::ErrorKind::Other, error));
        None
      },
      | Err(TryRecvError::Empty) => None,
      | Err(TryRecvError::Disconnected) => die(&io::Error::new(io::ErrorKind::Other,
                                                               "prolog handler thread \
                                                                unexpectedly disconnected")),
    }
  }

  /// # Panics
  ///
  /// Panics if there was an error displaying to the screen, an error processing
  /// keypresses, or an error handling the Prolog interface.
  pub async fn run(&mut self, rx:&mut UnboundedReceiver<Result<Vec<Term>, String>>) {
    loop {
      Self::handle_prolog(rx).await;
      if let Err(error) = self.view.refresh_screen(&self.state, self.should_quit).await {
        die(&error);
      }
      if self.should_quit {
        let _result = crossterm::terminal::disable_raw_mode();
        break
      }

      tokio::select! {
        res = self.process_keypress() => {
          if let Err(error) = res {
            die(&error);
          }
        }
        res = Self::handle_prolog(rx) => {
          if let Some(terms) = res {
            let mut guard = self.view.write().await;
            guard.1 = terms;
          }
        }
      };
    }
  }

  async fn remove(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    guard.0.remove(index);
    self.state =
      if guard.0.is_empty() { State::AtLeft } else { State::InLeft(index.min(guard.0.len() - 1)) };
    self.tx.send(guard.0.clone()).expect("send terms");
  }

  async fn quote(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    guard.0[index] = Term::new_quote(vec![guard.0[index].clone()]);
    self.tx.send(guard.0.clone()).expect("send terms");
  }

  async fn swap(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    guard.0.swap(index, index - 1);
    self.tx.send(guard.0.clone()).expect("send terms");
  }

  async fn dup(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    let term = guard.0[index].clone();
    guard.0.insert(index, term);
    self.tx.send(guard.0.clone()).expect("send terms");
  }

  async fn concat(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    if let Term::Quote(mut terms) = guard.0[index - 1].clone() {
      if let Term::Quote(other_terms) = guard.0[index].clone() {
        terms.extend(other_terms);
        guard.0.remove(index);
        guard.0[index] = Term::new_quote(terms);
        self.tx.send(guard.0.clone()).expect("send terms");
      }
    }
  }

  async fn unquote(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    if let Term::Quote(terms) = guard.0[index].clone() {
      let mut index = index;
      guard.0.remove(index);
      for term in terms {
        guard.0.insert(index, term);
        index += 1;
      }
      self.state = if guard.0.is_empty() {
        State::AtLeft
      } else {
        State::InLeft(index.saturating_sub(1).min(guard.0.len() - 1))
      };
      self.tx.send(guard.0.clone()).expect("send terms");
    }
  }

  async fn process_keypress(&mut self) -> io::Result<()> {
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
                guard.0 = vec![term];
                self.state = State::InLeft(0);
                self.tx.send(guard.0.clone()).expect("send terms");
              },
              | State::InLeft(index) => {
                guard.0.insert(index + 1, term);
                self.state = State::InLeft(index + 1);
                self.tx.send(guard.0.clone()).expect("send terms");
              },
              | _ => {},
            }
          },
        | (c, State::Editing(s, state)) => {
          let mut s = s;
          s.push(c);
          self.state = State::Editing(s, state);
        },
        | ('r', State::InLeft(index)) => self.remove(index).await,
        | ('q', State::InLeft(index)) => self.quote(index).await,
        | ('s', State::InLeft(index)) if index > 0 => self.swap(index).await,
        | ('d', State::InLeft(index)) => self.dup(index).await,
        | ('c', State::InLeft(index)) if index > 0 => self.concat(index).await,
        | ('u', State::InLeft(index)) => self.unquote(index).await,
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
          self.state = if guard.0.is_empty() {
            State::AtLeft
          } else {
            State::InLeft(index.saturating_sub(1).min(guard.0.len() - 1))
          };
        },
        | _ => {},
      },
      | (Down, _) => match self.state {
        | State::InLeft(index) => {
          let guard = self.view.read().await;
          self.state = if guard.0.is_empty() {
            State::AtLeft
          } else {
            State::InLeft((index + 1).min(guard.0.len() - 1))
          };
        },
        | _ => {},
      },
      | _ => {},
    }
    Ok(())
  }
}
