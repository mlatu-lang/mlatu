use std::io;
use std::io::{stdout, Write};
use std::sync::Arc;

use crossterm::event::{Event, EventStream, KeyEvent};
use crossterm::terminal::ClearType;
use crossterm::{cursor, queue, terminal, Command};
use tokio::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use tokio_stream::StreamExt;

use crate::ast::Rule;

#[derive(Clone)]
pub enum State {
  InLeft(usize),
  InRight(usize),
  AtLeft,
  AtRight,
  Editing(String, Box<Self>),
}

pub struct View {
  width:u16,
  height:u16,
  sides:Arc<RwLock<Rule>>,
  labels:(String, String),
  default_status:String,
}

impl View {
  pub fn new(sides:Arc<RwLock<Rule>>, labels:(String, String), default_status:String)
             -> io::Result<Self> {
    let (width, height) = terminal::size()?;
    Ok(Self { width, height, sides, labels, default_status })
  }

  fn queue(command:impl Command) -> io::Result<()> { queue!(stdout(), command) }

  pub fn clear_screen() -> io::Result<()> { Self::queue(terminal::Clear(ClearType::All)) }

  pub fn flush() -> io::Result<()> { stdout().flush() }

  pub async fn read_key(&mut self) -> io::Result<KeyEvent> {
    let mut stream = EventStream::new();
    loop {
      match stream.next().await {
        | Some(Ok(Event::Key(key))) => return Ok(key),
        | Some(Ok(Event::Resize(columns, rows))) => {
          self.width = columns;
          self.height = rows;
        },
        | Some(Ok(Event::Mouse(_))) => {},
        | Some(Err(e)) => return Err(e),
        | None =>
          return Err(io::Error::new(io::ErrorKind::Other, "unexpected end of terminal input")),
      }
    }
  }

  const fn left_half_width(&self, sep_len:u16) -> u16 {
    // Width of left box
    // half of the width, minus first separator
    // = l/2 - s
    // where l is the width and s is the separator length
    self.width / 2 - sep_len
  }

  const fn right_half_width(&self, sep_len:u16) -> u16 {
    // Width of right box
    // entire width, minus first box, minux all 3 separators
    // = l - (l/2 - s) - 3s
    // = l - l/s + s - 3s
    // = l - l/2 - 2s
    // where l is the width and s is the separator length
    self.width - self.width / 2 - 2 * sep_len
  }

  async fn make_left_half(&self, row:u16, s:&mut String) {
    let guard = self.sides.read().await;
    let term = guard.0.get(usize::from(row) - 2).map_or_else(String::new, ToString::to_string);
    let width = self.left_half_width(1);
    s.push_str(&format!("{0: ^1$}", term, width.into()));
  }

  async fn make_right_half(&self, row:u16, s:&mut String) {
    let width = self.right_half_width(1);
    let guard = self.sides.read().await;
    let term = guard.1.get(usize::from(row) - 2).map_or_else(String::new, ToString::to_string);
    s.push_str(&format!("{0: ^1$}", term, width.into()));
  }

  async fn get_target(&self, state:&State) -> (u16, u16) {
    let guard = self.sides.read().await;
    match state {
      | State::InLeft(index) => {
        let term = guard.0.get(*index).expect("bounds check failed");
        let p_t = term.to_string();
        (self.width / 4
         - u16::try_from(p_t.len()).expect("pattern term text is greater than 2^16 characters") / 2,
         u16::try_from(*index).expect("pattern terms longer than 2^16 terms") + 2)
      },
      | State::AtLeft => (self.width / 4 - 1, 2),
      | State::InRight(index) => {
        let term = guard.1.get(*index).expect("bounds check failed");
        let r_t = term.to_string();
        (3 * self.width / 4
         - (u16::try_from(r_t.len()).expect("replacement term text is greater than 2^16 \
                                             characters"))
           / 2,
         u16::try_from(*index).expect("replacement terms longer than 2^16 terms") + 2)
      },
      | State::AtRight => (3 * self.width / 4 - 1, 2),
      | State::Editing(ref s, _) =>
        (self.width / 2
         + (u16::try_from(s.len()).expect("input field text is greater than 2^16 characters") + 1)
           / 2,
         0),
    }
  }

  fn display_status(&mut self, state:&State) {
    let status = match &state {
      | State::Editing(msg, _) => msg.clone(),
      | _ => self.default_status.clone(),
    };
    let width = usize::from(self.width);
    print!("{0: ^1$}\r\n", status, width);
    print!("-{0:-^1$}-{2:-^3$}-\r\n",
           self.labels.0,
           self.left_half_width(1).into(),
           self.labels.1,
           self.right_half_width(1).into());
  }

  async fn display(&mut self, state:State) -> io::Result<()> {
    self.display_status(&state);
    for row in 2..self.height - 1 {
      let mut s = "|".to_owned();
      Self::queue(terminal::Clear(ClearType::CurrentLine))?;
      self.make_left_half(row, &mut s).await;
      s.push('|');
      self.make_right_half(row, &mut s).await;
      s.push('|');
      println!("{}\r", s);
    }
    let mut s = "|".to_owned();
    self.make_left_half(self.height, &mut s).await;
    s.push('|');
    self.make_right_half(self.height, &mut s).await;
    s.push('|');
    print!("{}", s);
    Ok(())
  }

  pub async fn refresh_screen(&mut self, state:&State, should_quit:bool) -> io::Result<()> {
    Self::queue(cursor::Hide)?;
    Self::queue(cursor::MoveTo(0, 0))?;
    if should_quit {
      Self::clear_screen()?;
      println!("Goodbye.\r");
    } else {
      self.display(state.clone()).await?;
      let (x, y) = self.get_target(state).await;
      Self::queue(cursor::MoveTo(x, y))?;
    }
    Self::queue(cursor::Show)?;
    Self::flush()
  }

  pub async fn read(&'_ self) -> RwLockReadGuard<'_, Rule> { self.sides.read().await }

  pub async fn write(&'_ self) -> RwLockWriteGuard<'_, Rule> { self.sides.write().await }
}
