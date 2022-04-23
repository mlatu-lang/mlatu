use std::io;
use std::io::{stdout, Write};
use std::sync::Arc;

use crossterm::event::{Event, EventStream, KeyEvent};
use crossterm::terminal::ClearType;
use crossterm::{cursor, queue, terminal, Command};
use mlatu_lib::{Rule, Term};
use tokio::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use tokio_stream::StreamExt;

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

  async fn make_left_half<F:Fn(&Term) -> String+Copy>(&self, f:F, row:u16, s:&mut String) {
    let guard = self.sides.read().await;
    let term =
      guard.redex.get(usize::from(self.height - row).saturating_sub(1)).map_or_else(String::new, f);
    let width = self.left_half_width(1);
    s.push_str(&format!("{0: ^1$}", term, width.into()));
  }

  async fn make_right_half<F:Fn(&Term) -> String+Copy>(&self, f:F, row:u16, s:&mut String) {
    let width = self.right_half_width(1);
    let guard = self.sides.read().await;
    let term = guard.reduction
                    .get(usize::from(self.height - row).saturating_sub(1))
                    .map_or_else(String::new, f);
    s.push_str(&format!("{0: ^1$}", term, width.into()));
  }

  async fn get_target<F:Fn(&Term) -> String+Copy>(&self, f:F, state:&State) -> (u16, u16) {
    let guard = self.sides.read().await;
    match state {
      | State::InLeft(index) => {
        let term = guard.redex.get(*index).expect("bounds check failed");
        let p_t = f(term);
        (self.width / 4
         - (u16::try_from(p_t.len()).expect("redex term text is greater than 2^16 characters") - 1) / 2,
         (self.height - u16::try_from(*index).expect("redex terms longer than 2^16 terms")).saturating_sub(1))
      },
      | State::AtLeft => (self.width / 4 - 1, self.height),
      | State::InRight(index) => {
        let term = guard.reduction.get(*index).expect("bounds check failed");
        let r_t = f(term);
        (3 * self.width / 4
         - (u16::try_from(r_t.len()).expect("reduction term text is greater than 2^16 \
                                             characters") - 1)
           / 2 - 1,
         (self.height - u16::try_from(*index).expect("reduction terms longer than 2^16 terms")).saturating_sub(1))
      },
      | State::AtRight => (3 * self.width / 4 - 1, self.height),
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

  async fn display<F:Fn(&Term) -> String+Copy>(&mut self, f:F, state:State) -> io::Result<()> {
    self.display_status(&state);
    for row in 2..self.height - 1 {
      let mut s = "|".to_owned();
      Self::queue(terminal::Clear(ClearType::CurrentLine))?;
      self.make_left_half(f, row, &mut s).await;
      s.push('|');
      self.make_right_half(f, row, &mut s).await;
      s.push('|');
      println!("{}\r", s);
    }
    let mut s = "|".to_owned();
    self.make_left_half(f, self.height, &mut s).await;
    s.push('|');
    self.make_right_half(f, self.height, &mut s).await;
    s.push('|');
    print!("{}", s);
    Ok(())
  }

  pub async fn refresh_screen<F:Fn(&Term) -> String+Copy>(&mut self, f:F, state:&State,
                                                          should_quit:bool)
                                                          -> io::Result<()> {
    Self::queue(cursor::Hide)?;
    Self::queue(cursor::MoveTo(0, 0))?;
    if should_quit {
      Self::clear_screen()?;
      println!("Goodbye.\r");
    } else {
      self.display(f, state.clone()).await?;
      let (x, y) = self.get_target(f, state).await;
      Self::queue(cursor::MoveTo(x, y))?;
    }
    Self::queue(cursor::Show)?;
    Self::flush()
  }

  pub async fn read(&'_ self) -> RwLockReadGuard<'_, Rule> { self.sides.read().await }

  pub async fn write(&'_ self) -> RwLockWriteGuard<'_, Rule> { self.sides.write().await }
}
