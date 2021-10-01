use async_std::fs::File;
use async_std::io;
use async_std::prelude::*;
use termion::event::Key;

use crate::ast::{Rule, Term};
use crate::parser::parse_terms;
use crate::terminal::Terminal;

const DEFAULT_STATUS:&str = "Welcome to the mlatu editor";

#[derive(Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum Location {
  Pattern,
  Replacement,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum State {
  InLoc(Location, usize),
  AtLoc(Location),
  Editing(String, Box<Self>),
}

pub struct Editor {
  file:File,
  terminal:Terminal,
  rules:Vec<Rule>,
  rule_idx:usize,
  should_quit:bool,
  state:State,
}

fn die(e:&std::io::Error) {
  Terminal::clear_screen();
  panic!("{}", e)
}

impl Editor {
  /// # Errors
  ///
  /// Will return `Err` if there was an error constructing a terminal
  pub fn new(file:File, rules:Vec<Rule>) -> io::Result<Self> {
    let terminal = Terminal::new()?;
    let should_quit = false;
    let rule_idx = 0;
    let (rules, state) = if rules.is_empty() {
      (vec![Rule::new(vec![], vec![])], State::AtLoc(Location::Pattern))
    } else {
      (rules, State::InLoc(Location::Pattern, 0))
    };
    Ok(Self { file, terminal, rules, rule_idx, should_quit, state })
  }

  pub async fn run(&mut self) {
    if let Err(error) = self.refresh_screen().await {
      die(&error);
    }
    loop {
      if let Err(error) = self.process_keypress().await {
        die(&error);
      }
      if let Err(error) = self.refresh_screen().await {
        die(&error);
      }
      if self.should_quit {
        break
      }
    }
  }

  const fn pattern_half_width(&self, sep_len:u16) -> u16 {
    let width = self.terminal.width();
    // Width of pattern box
    // half of the width, minus first separator
    // = l/2 - s
    // where l is the width and s is the separator length
    width / 2 - sep_len
  }

  const fn replacement_half_width(&self, sep_len:u16) -> u16 {
    let width = self.terminal.width();
    // Width of replacement box
    // entire width, minus first box, minux all 3 separators
    // = l - (l/2 - s) - 3s
    // = l - l/s + s - 3s
    // = l - l/2 - 2s
    // where l is the width and s is the separator length
    width - width / 2 - 2 * sep_len
  }

  fn make_pattern_half(&self, row:u16, s:&mut String) {
    Terminal::clear_current_line();
    let term = self.rules[self.rule_idx].pattern
                                        .get(usize::from(row) - 2)
                                        .map_or_else(String::new, ToString::to_string);
    let width = self.pattern_half_width(1);
    s.push_str(&format!("{0: ^1$}", term, width.into()));
  }

  fn make_replacement_half(&self, row:u16, s:&mut String) {
    let width = self.replacement_half_width(1);
    let term = self.rules[self.rule_idx].replacement
                                        .get(usize::from(row) - 2)
                                        .map_or_else(String::new, ToString::to_string);
    s.push_str(&format!("{0: ^1$}", term, width.into()));
  }

  fn go_to_position(&self) {
    let width = self.terminal.width();
    match &self.state {
      | State::InLoc(Location::Pattern, index) => {
        let term = self.rules[self.rule_idx].pattern.get(*index).expect("bounds check failed");
        let p_t = term.to_string();
        Terminal::cursor_position(width / 4
                                  - u16::try_from(p_t.len()).expect("pattern term text is \
                                                                     greater than 2^16 \
                                                                     characters")
                                    / 2,
                                  u16::try_from(*index).expect("pattern terms longer than 2^16 \
                                                                terms")
                                  + 2);
      },
      | State::AtLoc(Location::Pattern) => {
        Terminal::cursor_position(width / 4 - 1, 2);
      },
      | State::InLoc(Location::Replacement, index) => {
        let term = self.rules[self.rule_idx].replacement.get(*index).expect("bounds check failed");
        let r_t = term.to_string();
        Terminal::cursor_position(3 * width / 4
                                  - (u16::try_from(r_t.len()).expect("replacement term text is \
                                                                      greater than 2^16 \
                                                                      characters"))
                                    / 2
                                  - 1,
                                  u16::try_from(*index).expect("replacement terms longer than \
                                                                2^16 terms")
                                  + 2);
      },
      | State::AtLoc(Location::Replacement) => {
        Terminal::cursor_position(3 * width / 4 - 1, 2);
      },
      | State::Editing(s, _) =>
        Terminal::cursor_position(width / 2
                                  + (u16::try_from(s.len()).expect("input field text is greater \
                                                                    than 2^16 characters")
                                     + 1)
                                    / 2,
                                  0),
    }
  }

  fn display_status(&self) {
    let status = match &self.state {
      | State::Editing(msg, _) => msg.clone(),
      | _ => format!("{} (rule {}/{})", DEFAULT_STATUS, self.rule_idx, self.rules.len()),
    };
    let width = usize::from(self.terminal.width());
    println!("{0: ^1$}\r", status, width);
    println!("-{0:-^1$}-{2:-^3$}-\r",
             "| Pattern |",
             self.pattern_half_width(1).into(),
             "| Replacement |",
             self.replacement_half_width(1).into());
  }

  fn display(&self) {
    self.display_status();
    let height = self.terminal.height();
    for row in 2..height - 1 {
      let mut s = "|".to_owned();
      self.make_pattern_half(row, &mut s);
      s.push('|');
      self.make_replacement_half(row, &mut s);
      s.push('|');
      println!("{}\r", s);
    }
    let mut s = "|".to_owned();
    self.make_pattern_half(height, &mut s);
    s.push('|');
    self.make_replacement_half(height, &mut s);
    s.push('|');
    print!("{}", s);
  }

  async fn refresh_screen(&self) -> io::Result<()> {
    Terminal::cursor_hide();
    Terminal::cursor_position(0, 0);
    if self.should_quit {
      Terminal::clear_screen();
      println!("Goodbye.\r");
    } else {
      self.display();
      self.go_to_position();
    }
    Terminal::cursor_show();
    Terminal::flush().await
  }

  fn get_loc(&self, loc:Location) -> &Vec<Term> {
    match loc {
      | Location::Pattern => &self.rules[self.rule_idx].pattern,
      | Location::Replacement => &self.rules[self.rule_idx].replacement,
    }
  }

  fn get_loc_mut(&mut self, loc:Location) -> &mut Vec<Term> {
    match loc {
      | Location::Pattern => &mut self.rules[self.rule_idx].pattern,
      | Location::Replacement => &mut self.rules[self.rule_idx].replacement,
    }
  }

  fn set_position(&mut self, loc:Location, index:usize) {
    self.state = if self.get_loc(loc).is_empty() {
      State::AtLoc(loc)
    } else {
      State::InLoc(loc, index.min(self.get_loc(loc).len() - 1))
    }
  }

  async fn save(&mut self) -> io::Result<()> {
    self.file.seek(std::io::SeekFrom::Start(0)).await?;
    for rule in &self.rules {
      let mut rule = rule.to_string();
      rule.push('\n');
      self.file.write_all(rule.as_bytes()).await?;
    }
    Ok(())
  }

  fn submit_input(&mut self, s:&str, state:&State) {
    if let Ok(terms) = parse_terms(s) {
      match state {
        | State::AtLoc(loc) => {
          *self.get_loc_mut(*loc) = terms;
          self.set_position(*loc, 0);
        },
        | State::InLoc(loc, index) => {
          let mut index = *index;
          let mut_terms = self.get_loc_mut(*loc);
          for term in terms {
            mut_terms.insert(index, term);
            index += 1;
          }
          self.set_position(*loc, index - 1);
        },
        | State::Editing(..) => {},
      }
    }
  }

  async fn process_keypress(&mut self) -> io::Result<()> {
    let pressed_key = Terminal::read_key()?;
    match (pressed_key, self.state.clone()) {
      | (Key::Esc, _) => self.should_quit = true,
      | (Key::Ctrl('s'), _) => self.save().await?,
      | (Key::Ctrl('d'), _) => {
        if self.rules.len() == 1 {
          self.rules[0].pattern = Vec::new();
          self.rules[0].replacement = Vec::new();
        } else {
          self.rules.remove(self.rule_idx);
          self.rule_idx = self.rule_idx.min(self.rules.len() - 1);
        }
        self.state = State::AtLoc(Location::Pattern);
      },
      | (Key::Ctrl(' '), _) => {
        self.rules.insert(self.rule_idx, Rule::new(vec![], vec![]));
        self.state = State::AtLoc(Location::Pattern);
      },
      | (Key::Backspace | Key::Delete, State::Editing(s, state)) => {
        let mut s = s;
        s.pop();
        self.state = State::Editing(s, state);
      },
      | (Key::Char('\n'), State::Editing(s, state)) => self.submit_input(&s, &*state),
      | (Key::Char(c), State::Editing(s, state)) => {
        let mut s = s;
        s.push(c);
        self.state = State::Editing(s, state);
      },
      | (Key::Left, State::AtLoc(Location::Replacement)) => self.set_position(Location::Pattern, 0),
      | (Key::Left, State::InLoc(Location::Replacement, index)) =>
        self.set_position(Location::Pattern, index),
      | (Key::Left, State::AtLoc(Location::Pattern)) if self.rule_idx > 0 => {
        self.rule_idx -= 1;
        self.set_position(Location::Replacement, 0);
      },
      | (Key::Left, State::InLoc(Location::Pattern, index)) if self.rule_idx > 0 => {
        self.rule_idx -= 1;
        self.set_position(Location::Replacement, index);
      },
      | (Key::Right, State::AtLoc(Location::Pattern)) =>
        self.set_position(Location::Replacement, 0),
      | (Key::Right, State::InLoc(Location::Pattern, index)) =>
        self.set_position(Location::Replacement, index),
      | (Key::Right, State::AtLoc(Location::Replacement))
        if self.rule_idx < (self.rules.len() - 1) =>
      {
        self.rule_idx += 1;
        self.set_position(Location::Pattern, 0);
      }
      | (Key::Right, State::InLoc(Location::Replacement, index))
        if self.rule_idx < (self.rules.len() - 1) =>
      {
        self.rule_idx += 1;
        self.set_position(Location::Pattern, index);
      }
      | (Key::Up, State::InLoc(loc, index)) => self.set_position(loc, index.saturating_sub(1)),
      | (Key::Down, State::InLoc(loc, index)) => self.set_position(loc, index + 1),
      | (Key::Delete | Key::Backspace, State::InLoc(loc, index)) => {
        (*self.get_loc_mut(loc)).remove(index);
        self.set_position(loc, index);
      },
      | (Key::Char(' '), state) => self.state = State::Editing(String::new(), Box::new(state)),
      | (Key::Char('q'), State::InLoc(loc, index)) => {
        let term = self.get_loc(loc)[index].clone();
        (*self.get_loc_mut(loc))[index] = Term::new_quote(vec![term]);
      },
      | (Key::Char('i'), State::InLoc(loc, index)) => {
        if let Term::Quote(terms) = self.get_loc(loc)[index].clone() {
          let mut index = index;
          let mut_terms = self.get_loc_mut(loc);
          mut_terms.remove(index);
          for term in terms {
            mut_terms.insert(index, term);
            index += 1;
          }
          self.set_position(loc, index.saturating_sub(1));
        }
      },
      | _ => {},
    }
    Ok(())
  }
}
