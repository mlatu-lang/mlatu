use std::sync::Arc;

use tokio::fs::File;
use tokio::io;
use tokio::io::{AsyncSeekExt, AsyncWriteExt};
use tokio::sync::RwLock;

use crate::ast::{Rule, Term};
use crate::parser::parse_term;
use crate::pretty_rule;
use crate::view::{State, View};

pub struct Editor {
  file:File,
  view:View,
  rules:Vec<Arc<RwLock<Rule>>>,
  rule_idx:usize,
  should_quit:bool,
  state:State,
}

fn die(e:&std::io::Error) {
  let _result = crossterm::terminal::Clear(crossterm::terminal::ClearType::All);
  let _result = crossterm::terminal::disable_raw_mode();
  panic!("{}", e)
}

impl Editor {
  /// # Errors
  ///
  /// Will return `Err` if there was an error constructing a terminal
  pub fn new(file:File, original_rules:Vec<Rule>) -> io::Result<Self> {
    crossterm::terminal::enable_raw_mode()?;
    let should_quit = false;
    let rule_idx = 0;
    let (rules, state) = if original_rules.is_empty() {
      (vec![Arc::new(RwLock::new((vec![], vec![])))], State::AtLeft)
    } else {
      let mut rules = Vec::new();
      for rule in original_rules {
        rules.push(Arc::new(RwLock::new(rule)));
      }
      (rules, State::InLeft(0))
    };
    let default_status = format!("mlatu editor (rule {}/{})", rule_idx + 1, rules.len());
    let view = View::new(Arc::clone(&rules[rule_idx]),
                         ("| Pattern |".to_string(), "| Replacement |".to_string()),
                         default_status)?;
    Ok(Self { file, view, rules, rule_idx, should_quit, state })
  }

  pub async fn run(&mut self) {
    loop {
      if let Err(error) = self.view.refresh_screen(&self.state, self.should_quit).await {
        die(&error);
      }
      if self.should_quit {
        let _result = crossterm::terminal::disable_raw_mode();
        break
      }
      if let Err(error) = self.process_keypress().await {
        die(&error);
      }
    }
  }

  async fn save(&mut self) -> io::Result<()> {
    self.file.seek(std::io::SeekFrom::Start(0)).await?;
    for rule in &self.rules {
      let guard = rule.read().await;
      let mut rule = String::new();
      pretty_rule(&*guard.0, &*guard.1, &mut rule);
      rule.push('\n');
      self.file.write_all(rule.as_bytes()).await?;
    }
    Ok(())
  }

  async fn set_left_view(&mut self, index:usize) -> io::Result<()> {
    let rule = &self.rules[self.rule_idx];
    let default_status = format!("mlatu editor (rule {}/{})", self.rule_idx + 1, self.rules.len());
    let guard = rule.read().await;
    self.state =
      if guard.0.is_empty() { State::AtLeft } else { State::InLeft(index.min(guard.0.len() - 1)) };
    self.view = View::new(Arc::clone(rule),
                          ("| Pattern |".to_string(), "| Replacement |".to_string()),
                          default_status)?;
    Ok(())
  }

  async fn set_right_view(&mut self, index:usize) -> io::Result<()> {
    let rule = &self.rules[self.rule_idx];
    let default_status = format!("mlatu editor (rule {}/{})", self.rule_idx + 1, self.rules.len());
    let guard = rule.read().await;
    self.state = if guard.1.is_empty() {
      State::AtRight
    } else {
      State::InRight(index.min(guard.1.len() - 1))
    };
    self.view = View::new(Arc::clone(rule),
                          ("| Pattern |".to_string(), "| Replacement |".to_string()),
                          default_status)?;
    Ok(())
  }

  async fn process_left(&mut self) -> io::Result<()> {
    match self.state {
      | State::AtRight => {
        let guard = self.view.read().await;
        self.state = if guard.0.is_empty() { State::AtLeft } else { State::InLeft(0) };
      },
      | State::InRight(index) => {
        let guard = self.view.read().await;
        self.state = if guard.0.is_empty() {
          State::AtLeft
        } else {
          State::InLeft(index.min(guard.0.len() - 1))
        }
      },
      | State::AtLeft if self.rule_idx > 0 => {
        self.rule_idx -= 1;
        self.set_right_view(0).await?;
      },
      | State::InLeft(index) if self.rule_idx > 0 => {
        self.rule_idx -= 1;
        self.set_right_view(index).await?;
      },
      | _ => {},
    };
    Ok(())
  }

  async fn process_right(&mut self) -> io::Result<()> {
    match self.state {
      | State::AtLeft => {
        let guard = self.view.read().await;
        self.state = if guard.1.is_empty() { State::AtRight } else { State::InRight(0) };
      },
      | State::InLeft(index) => {
        let guard = self.view.read().await;
        self.state = if guard.1.is_empty() {
          State::AtRight
        } else {
          State::InRight(index.min(guard.1.len() - 1))
        }
      },
      | State::AtRight if self.rule_idx < (self.rules.len() - 1) => {
        self.rule_idx += 1;
        self.set_left_view(0).await?;
      },
      | State::InRight(index) if self.rule_idx < (self.rules.len() - 1) => {
        self.rule_idx += 1;
        self.set_left_view(index).await?;
      },
      | _ => {},
    };
    Ok(())
  }

  async fn input_word(&mut self, s:String, state:Box<State>) {
    if let Ok(term) = parse_term(&s) {
      let mut guard = self.view.write().await;

      match *state {
        | State::AtLeft => {
          guard.0 = vec![term];
          self.state = State::InLeft(0);
        },
        | State::AtRight => {
          guard.1 = vec![term];
          self.state = State::InRight(0);
        },
        | State::InLeft(index) => {
          guard.0.insert(index + 1, term);
          self.state = State::InLeft(index + 1);
        },
        | State::InRight(index) => {
          guard.1.insert(index + 1, term);
          self.state = State::InRight(index + 1);
        },
        | _ => {},
      }
    }
  }

  async fn concat_left(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    if let Term::Quote(mut terms) = guard.0[index - 1].clone() {
      if let Term::Quote(other_terms) = guard.0[index].clone() {
        terms.extend(other_terms);
        guard.0.remove(index);
        guard.0[index] = Term::new_quote(terms);
      }
    }
  }

  async fn concat_right(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    if let Term::Quote(mut terms) = guard.1[index - 1].clone() {
      if let Term::Quote(other_terms) = guard.1[index].clone() {
        terms.extend(other_terms);
        guard.1.remove(index);
        guard.1[index] = Term::new_quote(terms);
      }
    }
  }

  async fn unquote_left(&mut self, index:usize) {
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
    }
  }

  async fn unquote_right(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    if let Term::Quote(terms) = guard.1[index].clone() {
      let mut index = index;
      guard.1.remove(index);
      for term in terms {
        guard.1.insert(index, term);
        index += 1;
      }
      self.state = if guard.1.is_empty() {
        State::AtRight
      } else {
        State::InRight(index.saturating_sub(1).min(guard.1.len() - 1))
      };
    }
  }

  async fn remove_left(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    guard.0.remove(index);
    self.state =
      if guard.0.is_empty() { State::AtLeft } else { State::InLeft(index.min(guard.0.len() - 1)) };
  }

  async fn remove_right(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    guard.1.remove(index);
    self.state = if guard.1.is_empty() {
      State::AtRight
    } else {
      State::InRight(index.min(guard.1.len() - 1))
    };
  }

  async fn process_keypress(&mut self) -> io::Result<()> {
    use crossterm::event::KeyCode::{Backspace, Char, Delete, Down, Esc, Left, Right, Up};
    use crossterm::event::KeyModifiers;

    let event = self.view.read_key().await?;
    match (event.code, event.modifiers) {
      | (Esc, _) => self.should_quit = true,
      | (Char('w'), KeyModifiers::CONTROL) => self.save().await?,
      | (Char('r'), KeyModifiers::CONTROL) => {
        if self.rules.len() == 1 {
          self.rules[0] = Arc::new(RwLock::new((Vec::new(), Vec::new())));
        } else {
          self.rules.remove(self.rule_idx);
          self.rule_idx = self.rule_idx.min(self.rules.len() - 1);
        }
        self.set_left_view(0).await?;
      },
      | (Char(' '), KeyModifiers::CONTROL) => {
        self.rules.insert(self.rule_idx, Arc::new(RwLock::new((vec![], vec![]))));
        self.set_left_view(0).await?;
      },
      | (Char(c), _) => match (c, self.state.clone()) {
        | (' ', State::Editing(s, state)) => self.input_word(s, state).await,
        | (c, State::Editing(s, state)) => {
          let mut s = s;
          s.push(c);
          self.state = State::Editing(s, state);
        },
        | ('q', State::InLeft(index)) => {
          let mut guard = self.view.write().await;
          guard.0[index] = Term::new_quote(vec![guard.0[index].clone()]);
        },
        | ('q', State::InRight(index)) => {
          let mut guard = self.view.write().await;
          guard.1[index] = Term::new_quote(vec![guard.1[index].clone()]);
        },
        | ('r', State::InLeft(index)) => self.remove_left(index).await,
        | ('r', State::InRight(index)) => self.remove_right(index).await,
        | ('d', State::InLeft(index)) => {
          let mut guard = self.view.write().await;
          let term = guard.0[index].clone();
          guard.0.insert(index, term);
          self.state = State::InLeft(index.min(guard.0.len() - 1));
        },
        | ('d', State::InRight(index)) => {
          let mut guard = self.view.write().await;
          let term = guard.1[index].clone();
          guard.1.insert(index, term);
          self.state = State::InRight(index.min(guard.1.len() - 1));
        },
        | ('s', State::InLeft(index)) if index > 0 => {
          let mut guard = self.view.write().await;
          guard.0.swap(index, index - 1);
        },
        | ('s', State::InRight(index)) if index > 0 => {
          let mut guard = self.view.write().await;
          guard.1.swap(index, index - 1);
        },
        | ('c', State::InLeft(index)) if index > 0 => self.concat_left(index).await,
        | ('c', State::InRight(index)) if index > 0 => self.concat_right(index).await,
        | ('u', State::InLeft(index)) => self.unquote_left(index).await,
        | ('u', State::InRight(index)) => self.unquote_right(index).await,
        | (' ', _) => self.state = State::Editing(String::new(), Box::new(self.state.clone())),

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
      | (Left, _) => self.process_left().await?,
      | (Right, _) => self.process_right().await?,
      | (Up, _) => match self.state {
        | State::InLeft(index) => self.state = State::InLeft(index.saturating_sub(1)),
        | State::InRight(index) => self.state = State::InRight(index.saturating_sub(1)),
        | _ => {},
      },
      | (Down, _) => match self.state {
        | State::InLeft(index) => {
          let guard = self.view.read().await;
          self.state = State::InLeft((index + 1).min(guard.0.len() - 1));
        },
        | State::InRight(index) => {
          let guard = self.view.read().await;
          self.state = State::InRight((index + 1).min(guard.1.len() - 1));
        },
        | _ => {},
      },
      | _ => {},
    }
    Ok(())
  }
}
