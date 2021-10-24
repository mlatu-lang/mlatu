use std::io::{stdout, Write};
use std::path::PathBuf;
use std::sync::Arc;

use crossterm::queue;
use tokio::sync::RwLock;

use crate::ast::{binary, Rule, Term};
use crate::parser::parse_term;
use crate::view::{State, View};

pub struct Editor {
  path:PathBuf,
  view:View,
  rules:Vec<Arc<RwLock<Rule>>>,
  rule_idx:usize,
  should_quit:bool,
  state:State,
}

fn die(e:&str) {
  std::mem::drop(queue!(stdout(), crossterm::terminal::Clear(crossterm::terminal::ClearType::All)));
  std::mem::drop(crossterm::terminal::disable_raw_mode());
  std::mem::drop(stdout().flush());
  panic!("{}", e)
}

impl Editor {
  /// # Errors
  ///
  /// Will return `Err` if there was an error constructing a terminal
  pub fn new(path:PathBuf, original_rules:&[Rule]) -> Result<Self, String> {
    crossterm::terminal::enable_raw_mode().map_err(|e| e.to_string())?;
    let should_quit = false;
    let rule_idx = 0;
    let (rules, state) = if original_rules.is_empty() {
      (vec![Arc::new(RwLock::new(Rule { pat:vec![], rep:vec![] }))], State::AtLeft)
    } else {
      let mut rules = Vec::new();
      for rule in original_rules {
        rules.push(Arc::new(RwLock::new(rule.clone())));
      }
      (rules, if original_rules[0].pat.is_empty() { State::AtLeft } else { State::InLeft(0) })
    };
    let default_status =
      format!("{} (rule {}/{})", path.to_string_lossy(), rule_idx + 1, rules.len());
    let view = View::new(Arc::clone(&rules[rule_idx]),
                         ("| Pattern |".to_string(), "| Replacement |".to_string()),
                         default_status).map_err(|e| e.to_string())?;
    Ok(Self { path, view, rules, rule_idx, should_quit, state })
  }

  pub async fn run(&mut self) {
    loop {
      if let Err(error) = self.view.refresh_screen(&self.state, self.should_quit).await {
        die(&error.to_string());
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

  async fn save(&mut self) -> Result<(), String> {
    let mut rs = Vec::new();
    for rule in &self.rules {
      let guard = rule.read().await;
      rs.push(guard.clone());
    }
    std::fs::write(self.path.clone(), &binary::serialize_rules(rs)).map_err(|e| e.to_string())
  }

  async fn set_left_view(&mut self, index:usize) -> Result<(), String> {
    let rule = &self.rules[self.rule_idx];
    let default_status =
      format!("{} (rule {}/{})", self.path.to_string_lossy(), self.rule_idx + 1, self.rules.len());
    let guard = rule.read().await;
    self.state = if guard.pat.is_empty() {
      State::AtLeft
    } else {
      State::InLeft(index.min(guard.pat.len() - 1))
    };
    self.view = View::new(Arc::clone(rule),
                          ("| Pattern |".to_string(), "| Replacement |".to_string()),
                          default_status).map_err(|e| e.to_string())?;
    Ok(())
  }

  async fn set_right_view(&mut self, index:usize) -> Result<(), String> {
    let rule = &self.rules[self.rule_idx];
    let default_status =
      format!("{} (rule {}/{})", self.path.to_string_lossy(), self.rule_idx + 1, self.rules.len());
    let guard = rule.read().await;
    self.state = if guard.rep.is_empty() {
      State::AtRight
    } else {
      State::InRight(index.min(guard.rep.len() - 1))
    };
    self.view = View::new(Arc::clone(rule),
                          ("| Pattern |".to_string(), "| Replacement |".to_string()),
                          default_status).map_err(|e| e.to_string())?;
    Ok(())
  }

  async fn process_left(&mut self) -> Result<(), String> {
    match self.state {
      | State::AtRight => {
        let guard = self.view.read().await;
        self.state = if guard.pat.is_empty() { State::AtLeft } else { State::InLeft(0) };
      },
      | State::InRight(index) => {
        let guard = self.view.read().await;
        self.state = if guard.pat.is_empty() {
          State::AtLeft
        } else {
          State::InLeft(index.min(guard.pat.len() - 1))
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

  async fn process_right(&mut self) -> Result<(), String> {
    match self.state {
      | State::AtLeft => {
        let guard = self.view.read().await;
        self.state = if guard.rep.is_empty() { State::AtRight } else { State::InRight(0) };
      },
      | State::InLeft(index) => {
        let guard = self.view.read().await;
        self.state = if guard.rep.is_empty() {
          State::AtRight
        } else {
          State::InRight(index.min(guard.rep.len() - 1))
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
          guard.pat = vec![term];
          self.state = State::InLeft(0);
        },
        | State::AtRight => {
          guard.rep = vec![term];
          self.state = State::InRight(0);
        },
        | State::InLeft(index) => {
          guard.pat.insert(index, term);
          self.state = State::InLeft(index);
        },
        | State::InRight(index) => {
          guard.rep.insert(index, term);
          self.state = State::InRight(index);
        },
        | _ => {},
      }
    }
  }

  async fn concat_left(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    if let Term::Quote(mut terms) = guard.pat[index].clone() {
      if let Some(Term::Quote(other_terms)) = guard.pat.get(index + 1).cloned() {
        terms.extend(other_terms);
        guard.pat.remove(index);
        guard.pat[index] = Term::new_quote(terms);
      }
    }
  }

  async fn concat_right(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    if let Term::Quote(mut terms) = guard.rep[index].clone() {
      if let Some(Term::Quote(other_terms)) = guard.rep.get(index + 1).cloned() {
        terms.extend(other_terms);
        guard.rep.remove(index);
        guard.rep[index] = Term::new_quote(terms);
      }
    }
  }

  async fn unquote_left(&mut self, index:usize) {
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
    }
  }

  async fn unquote_right(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    if let Term::Quote(terms) = guard.rep[index].clone() {
      guard.rep.remove(index);
      let mut i = index;
      for term in terms {
        guard.rep.insert(i, term);
        i += 1;
      }
      self.state = if guard.rep.is_empty() {
        State::AtRight
      } else {
        State::InRight(index.min(guard.rep.len() - 1))
      };
    }
  }

  async fn remove_left(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    guard.pat.remove(index);
    self.state = if guard.pat.is_empty() {
      State::AtLeft
    } else {
      State::InLeft(index.min(guard.pat.len() - 1))
    };
  }

  async fn remove_right(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    guard.rep.remove(index);
    self.state = if guard.rep.is_empty() {
      State::AtRight
    } else {
      State::InRight(index.min(guard.rep.len() - 1))
    };
  }

  async fn process_keypress(&mut self) -> Result<(), String> {
    use crossterm::event::KeyCode::{Backspace, Char, Delete, Down, Esc, Left, Right, Up};
    use crossterm::event::KeyModifiers;

    let event = self.view.read_key().await.map_err(|e| e.to_string())?;
    match (event.code, event.modifiers) {
      | (Esc, _) => self.should_quit = true,
      | (Char('w'), KeyModifiers::CONTROL) => self.save().await?,
      | (Char('r'), KeyModifiers::CONTROL) => {
        if self.rules.len() == 1 {
          self.rules[0] = Arc::new(RwLock::new(Rule { pat:vec![], rep:vec![] }));
        } else {
          self.rules.remove(self.rule_idx);
          self.rule_idx = self.rule_idx.min(self.rules.len() - 1);
        }
        self.set_left_view(0).await?;
      },
      | (Char(' '), KeyModifiers::CONTROL) => {
        self.rules.insert(self.rule_idx, Arc::new(RwLock::new(Rule { pat:vec![], rep:vec![] })));
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
          guard.pat[index] = Term::new_quote(vec![guard.pat[index].clone()]);
        },
        | ('q', State::InRight(index)) => {
          let mut guard = self.view.write().await;
          guard.rep[index] = Term::new_quote(vec![guard.rep[index].clone()]);
        },
        | ('r', State::InLeft(index)) => self.remove_left(index).await,
        | ('r', State::InRight(index)) => self.remove_right(index).await,
        | ('d', State::InLeft(index)) => {
          let mut guard = self.view.write().await;
          let term = guard.pat[index].clone();
          guard.pat.insert(index, term);
          self.state = State::InLeft(index.min(guard.pat.len() - 1));
        },
        | ('d', State::InRight(index)) => {
          let mut guard = self.view.write().await;
          let term = guard.rep[index].clone();
          guard.rep.insert(index, term);
          self.state = State::InRight(index.min(guard.rep.len() - 1));
        },
        | ('s', State::InLeft(index)) => {
          let mut guard = self.view.write().await;
          if guard.pat.len() > index + 1 {
            guard.pat.swap(index, index + 1);
          }
        },
        | ('s', State::InRight(index)) => {
          let mut guard = self.view.write().await;
          if guard.pat.len() > index + 1 {
            guard.rep.swap(index, index + 1);
          }
        },
        | ('c', State::InLeft(index)) => self.concat_left(index).await,
        | ('c', State::InRight(index)) => self.concat_right(index).await,
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
          self.state = State::InLeft((index + 1).min(guard.pat.len() - 1));
        },
        | State::InRight(index) => {
          let guard = self.view.read().await;
          self.state = State::InRight((index + 1).min(guard.rep.len() - 1));
        },
        | _ => {},
      },
      | _ => {},
    }
    Ok(())
  }
}
