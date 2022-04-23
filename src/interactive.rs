use std::io;
use std::io::stdout;
use std::sync::Arc;

use crossterm::execute;
use im::{vector, Vector};
use mlatu_lib::{parse, pretty, rewrite, Engine, Rule, Term};
use tokio::sync::RwLock;

use crate::view::{State, View};

pub struct Interactive {
  rules:Vector<Rule>,
  engine:Engine,
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
  pub fn new(engine:Engine, rules:Vector<Rule>) -> io::Result<Self> {
    crossterm::terminal::enable_raw_mode()?;
    let should_quit = false;
    let rule = Arc::new(RwLock::new(Rule { redex:Vector::new(), reduction:Vector::new() }));
    let state = State::AtLeft;
    let default_status = "mlatu interface".to_string();
    let view = View::new(Arc::clone(&rule),
                         ("| Input |".to_string(), "| Output |".to_string()),
                         default_status)?;
    Ok(Self { rules, engine, view, should_quit, state })
  }

  /// # Panics
  ///
  /// Panics if there was an error displaying to the screen, an error processing
  /// keypresses, or an error handling the Prolog interface.
  ///
  /// # Errors
  ///
  /// Returns `Err` if there was an IO error reading keypresses
  pub async fn run(&mut self) -> io::Result<()> {
    loop {
      if let Err(error) = self.view
                              .refresh_screen(|term| pretty::term(&self.engine, term.clone()),
                                              &self.state,
                                              self.should_quit)
                              .await
      {
        die(&error.to_string());
      }
      if self.should_quit {
        let _result = crossterm::terminal::disable_raw_mode();
        break Ok(())
      }
      self.process_keypress().await?;
    }
  }

  async fn remove(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    guard.redex.remove(index);
    self.state = if guard.redex.is_empty() {
      State::AtLeft
    } else {
      State::InLeft(index.min(guard.redex.len() - 1))
    };
    guard.reduction = rewrite(&self.engine, &self.rules, guard.redex.clone());
  }

  async fn quote(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    guard.redex[index] =
      Term::make_quote(&self.engine, vector![guard.redex[index].clone()]).clone();
    guard.reduction = rewrite(&self.engine, &self.rules, guard.redex.clone());
  }

  async fn swap(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    if index > 0 {
      guard.redex.swap(index, index - 1);
      guard.reduction = rewrite(&self.engine, &self.rules, guard.redex.clone());
    }
  }

  async fn dup(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    let term = guard.redex[index].clone();
    guard.redex.insert(index, term);
    guard.reduction = rewrite(&self.engine, &self.rules, guard.redex.clone());
  }

  async fn concat(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    if let Term::Quote(mut terms) = guard.redex[index].clone() {
      if let Some(Term::Quote(other_terms)) = guard.redex.get(index + 1) {
        terms.extend(other_terms.clone());
        guard.redex.remove(index);
        guard.redex[index] = Term::make_quote(&self.engine, terms).clone();
        guard.reduction = rewrite(&self.engine, &self.rules, guard.redex.clone());
      }
    }
  }

  async fn unquote(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    if let Term::Quote(terms) = guard.redex[index].clone() {
      guard.redex.remove(index);
      let mut i = index;
      for term in terms {
        guard.redex.insert(i, term);
        i += 1;
      }
      self.state = if guard.redex.is_empty() {
        State::AtLeft
      } else {
        State::InLeft(index.min(guard.redex.len() - 1))
      };
      guard.reduction = rewrite(&self.engine, &self.rules, guard.redex.clone());
    }
  }

  async fn process_keypress(&mut self) -> io::Result<()> {
    use crossterm::event::KeyCode::{Backspace, Char, Delete, Down, Esc, Up};

    let event = self.view.read_key().await?;
    match (event.code, event.modifiers) {
      | (Esc, _) => self.should_quit = true,
      | (Char(c), _) => match (c, self.state.clone()) {
        | (' ', State::Editing(s, state)) =>
          if let Ok(term) = parse::term(&self.engine, &s) {
            let mut guard = self.view.write().await;
            match *state {
              | State::AtLeft => {
                guard.redex = vector![term.clone()];
                self.state = State::InLeft(0);
                guard.reduction = rewrite(&self.engine, &self.rules, guard.redex.clone());
              },
              | State::InLeft(index) => {
                guard.redex.insert(index, term.clone());
                self.state = State::InLeft(index);
                guard.reduction = rewrite(&self.engine, &self.rules, guard.redex.clone());
              },
              | _ => {},
            }
          },
        | (c, State::Editing(s, state)) => {
          let mut s = s;
          s.push(c);
          self.state = State::Editing(s, state);
        },
        | ('-', State::InLeft(index)) => self.remove(index).await,
        | ('>', State::InLeft(index)) => self.quote(index).await,
        | ('~', State::InLeft(index)) => self.swap(index).await,
        | ('+', State::InLeft(index)) => self.dup(index).await,
        | (',', State::InLeft(index)) => self.concat(index).await,
        | ('<', State::InLeft(index)) => self.unquote(index).await,
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
          self.state = if guard.redex.is_empty() {
            State::AtLeft
          } else {
            State::InLeft((index + 1).min(guard.redex.len() - 1))
          };
        },
        | _ => {},
      },
      | (Down, _) => match self.state {
        | State::InLeft(index) => {
          let guard = self.view.read().await;
          self.state = if guard.redex.is_empty() {
            State::AtLeft
          } else {
            State::InLeft((index.saturating_sub(1)).min(guard.redex.len() - 1))
          };
        },
        | _ => {},
      },
      | _ => {},
    }
    Ok(())
  }
}
