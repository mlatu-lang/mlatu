use std::io::{stdout, Write};
use std::path::PathBuf;
use std::sync::Arc;

use crossterm::event::KeyCode::{Backspace, Char, Delete, Down, Esc, Left, Right, Up};
use crossterm::event::KeyModifiers;
use crossterm::queue;
use im::{vector, Vector};
use mlatu_lib::{parse, pretty, Engine, Rule, Term};
use tokio::sync::RwLock;

use crate::view::{State, View};

pub struct Editor {
  engine:Engine,
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
  pub fn new(engine:Engine, path:PathBuf, original_rules:Vector<Rule>) -> Result<Self, String> {
    crossterm::terminal::enable_raw_mode().map_err(|e| e.to_string())?;
    let should_quit = false;
    let rule_idx = 0;
    let (rules, state) = if original_rules.is_empty() {
      (vec![Arc::new(RwLock::new(Rule { redex:Vector::new(), reduction:Vector::new() }))],
       State::AtLeft)
    } else {
      let mut rules = Vec::new();
      let state = if original_rules[0].redex.is_empty() { State::AtLeft } else { State::InLeft(0) };
      for rule in original_rules {
        rules.push(Arc::new(RwLock::new(rule.clone())));
      }
      (rules, state)
    };
    let default_status =
      format!("{} (rule {}/{})", path.to_string_lossy(), rule_idx + 1, rules.len());
    let view = View::new(Arc::clone(&rules[rule_idx]),
                         ("| Pattern |".to_string(), "| Replacement |".to_string()),
                         default_status).map_err(|e| e.to_string())?;
    Ok(Self { engine, path, view, rules, rule_idx, should_quit, state })
  }

  pub async fn run(&mut self) {
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
        break
      }
      if let Err(error) = self.process_keypress().await {
        die(&error);
      }
    }
  }

  async fn save(&mut self) -> Result<(), String> {
    let mut rs = Vector::new();
    for rule in &self.rules {
      let guard = rule.read().await;
      rs.push_back(guard.clone());
    }
    std::fs::write(self.path.clone(), &pretty::rules(&self.engine, rs)).map_err(|e| e.to_string())
  }

  async fn set_left_view(&mut self, index:usize) -> Result<(), String> {
    let rule = &self.rules[self.rule_idx];
    let default_status =
      format!("{} (rule {}/{})", self.path.to_string_lossy(), self.rule_idx + 1, self.rules.len());
    let guard = rule.read().await;
    self.state = if guard.redex.is_empty() {
      State::AtLeft
    } else {
      State::InLeft(index.min(guard.redex.len() - 1))
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
    self.state = if guard.reduction.is_empty() {
      State::AtRight
    } else {
      State::InRight(index.min(guard.reduction.len() - 1))
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
        self.state = if guard.redex.is_empty() { State::AtLeft } else { State::InLeft(0) };
      },
      | State::InRight(index) => {
        let guard = self.view.read().await;
        self.state = if guard.redex.is_empty() {
          State::AtLeft
        } else {
          State::InLeft(index.min(guard.redex.len() - 1))
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
        self.state = if guard.reduction.is_empty() { State::AtRight } else { State::InRight(0) };
      },
      | State::InLeft(index) => {
        let guard = self.view.read().await;
        self.state = if guard.reduction.is_empty() {
          State::AtRight
        } else {
          State::InRight(index.min(guard.reduction.len() - 1))
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
    if let Ok(term) = parse::term(&self.engine, &s) {
      let mut guard = self.view.write().await;

      match *state {
        | State::AtLeft => {
          guard.redex = vector![term.clone()];
          self.state = State::InLeft(0);
        },
        | State::AtRight => {
          guard.reduction = vector![term.clone()];
          self.state = State::InRight(0);
        },
        | State::InLeft(index) => {
          guard.redex.insert(index, term.clone());
          self.state = State::InLeft(index);
        },
        | State::InRight(index) => {
          guard.reduction.insert(index, term.clone());
          self.state = State::InRight(index);
        },
        | _ => {},
      }
    }
  }

  async fn concat_left(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    if let Term::Quote(mut terms) = guard.redex[index].clone() {
      if let Some(Term::Quote(other_terms)) = guard.redex.get(index + 1).cloned() {
        terms.extend(other_terms);
        guard.redex.remove(index);
        guard.redex[index] = Term::make_quote(&self.engine, terms).clone();
      }
    }
  }

  async fn concat_right(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    if let Term::Quote(mut terms) = guard.reduction[index].clone() {
      if let Some(Term::Quote(other_terms)) = guard.reduction.get(index + 1).cloned() {
        terms.extend(other_terms);
        guard.reduction.remove(index);
        guard.reduction[index] = Term::make_quote(&self.engine, terms).clone();
      }
    }
  }

  async fn unquote_left(&mut self, index:usize) {
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
    }
  }

  async fn unquote_right(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    if let Term::Quote(terms) = guard.reduction[index].clone() {
      guard.reduction.remove(index);
      let mut i = index;
      for term in terms {
        guard.reduction.insert(i, term);
        i += 1;
      }
      self.state = if guard.reduction.is_empty() {
        State::AtRight
      } else {
        State::InRight(index.min(guard.reduction.len() - 1))
      };
    }
  }

  async fn remove_left(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    guard.redex.remove(index);
    self.state = if guard.redex.is_empty() {
      State::AtLeft
    } else {
      State::InLeft(index.min(guard.redex.len() - 1))
    };
  }

  async fn remove_right(&mut self, index:usize) {
    let mut guard = self.view.write().await;
    guard.reduction.remove(index);
    self.state = if guard.reduction.is_empty() {
      State::AtRight
    } else {
      State::InRight(index.min(guard.reduction.len() - 1))
    };
  }

  async fn process_keypress(&mut self) -> Result<(), String> {
    let event = self.view.read_key().await.map_err(|e| e.to_string())?;
    match (event.code, event.modifiers) {
      | (Esc, _) => self.should_quit = true,
      | (Char('w'), KeyModifiers::CONTROL) => self.save().await?,
      | (Char('r'), KeyModifiers::CONTROL) => {
        if self.rules.len() == 1 {
          self.rules[0] =
            Arc::new(RwLock::new(Rule { redex:Vector::new(), reduction:Vector::new() }));
        } else {
          self.rules.remove(self.rule_idx);
          self.rule_idx = self.rule_idx.min(self.rules.len() - 1);
        }
        self.set_left_view(0).await?;
      },
      | (Char(' '), KeyModifiers::CONTROL) => {
        self.rules
            .insert(self.rule_idx,
                    Arc::new(RwLock::new(Rule { redex:Vector::new(), reduction:Vector::new() })));
        self.set_left_view(0).await?;
      },
      | (Char(c), _) => match (c, self.state.clone()) {
        | (' ', State::Editing(s, state)) => self.input_word(s, state).await,
        | (c, State::Editing(s, state)) => {
          let mut s = s;
          s.push(c);
          self.state = State::Editing(s, state);
        },
        | ('>', State::InLeft(index)) => {
          let mut guard = self.view.write().await;
          guard.redex[index] =
            Term::make_quote(&self.engine, vector![guard.redex[index].clone()]).clone();
        },
        | ('>', State::InRight(index)) => {
          let mut guard = self.view.write().await;
          guard.reduction[index] =
            Term::make_quote(&self.engine, vector![guard.reduction[index].clone()]).clone();
        },
        | ('-', State::InLeft(index)) => self.remove_left(index).await,
        | ('-', State::InRight(index)) => self.remove_right(index).await,
        | ('+', State::InLeft(index)) => {
          let mut guard = self.view.write().await;
          let term = guard.redex[index].clone();
          guard.redex.insert(index, term);
          self.state = State::InLeft(index.min(guard.redex.len() - 1));
        },
        | ('+', State::InRight(index)) => {
          let mut guard = self.view.write().await;
          let term = guard.reduction[index].clone();
          guard.reduction.insert(index, term);
          self.state = State::InRight(index.min(guard.reduction.len() - 1));
        },
        | ('~', State::InLeft(index)) =>
          if index > 0 {
            let mut guard = self.view.write().await;
            guard.redex.swap(index, index - 1);
          },
        | ('~', State::InRight(index)) =>
          if index > 0 {
            let mut guard = self.view.write().await;
            guard.reduction.swap(index, index - 1);
          },
        | (',', State::InLeft(index)) => self.concat_left(index).await,
        | (',', State::InRight(index)) => self.concat_right(index).await,
        | ('<', State::InLeft(index)) => self.unquote_left(index).await,
        | ('<', State::InRight(index)) => self.unquote_right(index).await,
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
      | (Down, _) => match self.state {
        | State::InLeft(index) => self.state = State::InLeft(index.saturating_sub(1)),
        | State::InRight(index) => self.state = State::InRight(index.saturating_sub(1)),
        | _ => {},
      },
      | (Up, _) => match self.state {
        | State::InLeft(index) => {
          let guard = self.view.read().await;
          self.state = State::InLeft((index + 1).min(guard.redex.len() - 1));
        },
        | State::InRight(index) => {
          let guard = self.view.read().await;
          self.state = State::InRight((index + 1).min(guard.reduction.len() - 1));
        },
        | _ => {},
      },
      | _ => {},
    }
    Ok(())
  }
}
