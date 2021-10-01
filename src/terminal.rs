use async_std::io;
use async_std::io::{stdout, WriteExt};
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::{IntoRawMode, RawTerminal};

pub struct Terminal {
  width:u16,
  height:u16,
  _stdout:RawTerminal<std::io::Stdout>,
}

impl Terminal {
  pub fn new() -> io::Result<Self> {
    let size = termion::terminal_size()?;
    Ok(Self { width:size.0, height:size.1, _stdout:std::io::stdout().into_raw_mode()? })
  }

  pub const fn width(&self) -> u16 { self.width }

  pub const fn height(&self) -> u16 { self.height }

  pub fn clear_screen() { print!("{}", termion::clear::All) }

  pub fn cursor_position(x:u16, y:u16) {
    let x = x.saturating_add(1);
    let y = y.saturating_add(1);
    print!("{}", termion::cursor::Goto(x, y));
  }

  pub async fn flush() -> io::Result<()> { stdout().flush().await }

  pub fn read_key() -> io::Result<Key> {
    loop {
      if let Some(key) = std::io::stdin().lock().keys().next() {
        return key
      }
    }
  }

  pub fn cursor_hide() { print!("{}", termion::cursor::Hide) }

  pub fn cursor_show() { print!("{}", termion::cursor::Show) }

  pub fn clear_current_line() { print!("{}", termion::clear::CurrentLine) }
}
