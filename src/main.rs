#![feature(derive_default_enum)]
#![deny(clippy::correctness)]
#![warn(clippy::suspicious,
        clippy::style,
        clippy::complexity,
        clippy::perf,
        clippy::pedantic,
        clippy::nursery)]

use iced::keyboard::KeyCode;
use iced::{button, executor, keyboard, text_input, tooltip, Application, Button, Clipboard, Column, Command, Element, Length, Radio, Row, Settings, Space, Subscription, Text, TextInput, Tooltip};
use iced_native::{subscription, Event};
use im::{vector, Vector};
use mlatu::{parse, pretty, rewrite, Arena, Rodeo, Term};

macro_rules! button {
  ($condition:expr, $state:expr, $symbol:expr, $name:expr, $message:ident) => {
    Tooltip::new(if ($condition).is_some() {
                   Element::from(Button::new(&mut $state, Text::new($symbol).size(50)).on_press(Message::$message)
                                                                        .width(Length::Units(35)))
                 } else {
                   Element::from(Text::new($symbol).size(50).width(Length::Units(35)))
                 },
                 $name,
                 tooltip::Position::Bottom).into()
  };
}

#[derive(Debug, Clone)]
enum Message {
  TextInputChanged(String),
  TextInputSubmitted,
  RadioSelected(usize),
  PlusPressed,
  MinusPressed,
  WrapPressed,
  UnwrapPressed,
  SwapPressed,
  CombinePressed,
  UpPressed,
  DownPressed,
}

#[derive(Default)]
struct App {
  input:Vector<Term>,
  output:Vector<Term>,
  rodeo:Rodeo,
  arena:Arena,

  text_value:String,
  text_input:text_input::State,
  plus_button:button::State,
  minus_button:button::State,
  wrap_button:button::State,
  unwrap_button:button::State,
  swap_button:button::State,
  combine_button:button::State,
  selected:Option<usize>,
}

impl App {
  fn update_output(&mut self) {
    self.output = rewrite(&self.arena, &self.rodeo, &Vector::new(), self.input.clone());
  }
}

fn unwrap_valid(selected:Option<usize>, input:&Vector<Term>) -> Option<(usize, Vector<Term>)> {
  if let Some(index) = selected {
    if let Term::Quote(terms) = &input[index] {
      return Some((index, terms.clone()))
    }
  }
  None
}

fn swap_valid(selected:Option<usize>, input:&Vector<Term>) -> Option<(usize, usize)> {
  if let Some(index) = selected {
    if index + 1 < input.len() {
      return Some((index, index + 1))
    }
  }
  None
}

fn combine_valid(selected:Option<usize>, input:&Vector<Term>)
                 -> Option<(usize, Vector<Term>, Vector<Term>)> {
  if let Some(index) = selected {
    if let Term::Quote(a) = &input[index] {
      if let Some(Term::Quote(b)) = input.get(index + 1) {
        return Some((index, a.clone(), b.clone()))
      }
    }
  }
  None
}

impl Application for App {
  type Executor = executor::Default;
  type Flags = ();
  type Message = Message;

  fn new(_:()) -> (Self, Command<Message>) {
    (Self::default(), Command::none())
  }

  fn subscription(&self) -> Subscription<Message> {
    subscription::events_with(|event, _status| match event {
      | Event::Keyboard(keyboard::Event::KeyPressed { key_code: KeyCode::Up, modifiers: _, }) =>
        Some(Message::UpPressed),
      | Event::Keyboard(keyboard::Event::KeyPressed { key_code: KeyCode::Down, modifiers: _, }) =>
        Some(Message::DownPressed),
      // | Event::Keyboard(keyboard::Event::CharacterReceived('+')) => Some(Message::PlusPressed),
      // | Event::Keyboard(keyboard::Event::CharacterReceived('-')) => Some(Message::MinusPressed),
      // | Event::Keyboard(keyboard::Event::CharacterReceived('>')) => Some(Message::WrapPressed),
      // | Event::Keyboard(keyboard::Event::CharacterReceived('<')) => Some(Message::UnwrapPressed),
      // | Event::Keyboard(keyboard::Event::CharacterReceived('~')) => Some(Message::SwapPressed),
      // | Event::Keyboard(keyboard::Event::CharacterReceived(',')) =>
      // Some(Message::CombinePressed),
      | _ => None,
    })
  }

  fn title(&self) -> String { "mlatu app".to_string() }

  fn view(&mut self) -> Element<Message> {
    Column::with_children(vec![
            TextInput::new(&mut self.text_input, "Insert word...", &self.text_value, Message::TextInputChanged).on_submit(Message::TextInputSubmitted).into(),
                Row::with_children(vec![
                    button!(self.selected, self.plus_button, "+", "copy", PlusPressed),
                    button!(self.selected, self.minus_button, "-", "remove", MinusPressed),
                    button!(self.selected, self.wrap_button, ">", "wrap", WrapPressed),
                    button!(unwrap_valid(self.selected, &self.input), self.unwrap_button, "<", "unwrap", UnwrapPressed),
                    button!(swap_valid(self.selected, &self.input), self.swap_button, "~", "swap", SwapPressed),
                    button!(combine_valid(self.selected, &self.input), self.combine_button, ",", "combine", CombinePressed)
                ])
            .into(),
            Space::with_height(Length::Units(30)).into(),
            Row::new()
                .spacing(30)
                .push(
                    Column::new()
                        .push(Text::new("Input"))
                        .push(Column::with_children(
                            self.input
                                .iter_mut()
                                .enumerate()
                                .map(|(i, term)| {
                                    Radio::new(
                                        i,
                                        pretty::term(&self.rodeo, term.clone()),
                                        self.selected,
                                        Message::RadioSelected,
                                    )
                                    .text_size(40)
                                    .into()
                                })
                                .collect(),
                        )),
                )
                .push(
                    Column::new()
                        .push(Text::new("Output"))
                        .push(Column::with_children(
                            self.output
                                .iter()
                                .map(|term| {
                                    Text::new(pretty::term(&self.rodeo, term.clone()))
                                        .size(40)
                                        .into()
                                })
                                .collect(),
                        )),
                )
                .into(),
        ])
        .into()
  }

  fn update(&mut self, message:Message, _clipboard:&mut Clipboard) -> Command<Message> {
    match message {
      | Message::TextInputChanged(s) => {
        self.text_value = s;
      },
      | Message::TextInputSubmitted => {
        if let Ok(term) = parse::term(&self.arena, &self.rodeo, &self.text_value) {
          self.input.insert(self.selected.unwrap_or(0), term.clone());
          self.update_output();
          self.text_value = String::new();
        }
      },
      | Message::RadioSelected(index) => {
        self.selected = Some(index);
      },
      | Message::PlusPressed =>
        if let Some(index) = self.selected {
          self.input.insert(index, self.input[index].clone());
          self.update_output();
        },
      | Message::MinusPressed =>
        if let Some(index) = self.selected {
          self.input.remove(index);
          self.update_output();
          if index >= self.input.len() {
            self.selected = None;
          }
        },
      | Message::WrapPressed =>
        if let Some(index) = self.selected {
          let term = Term::make_quote(&self.arena, vector![self.input[index].clone()]);
          self.input[index] = term.clone();
          self.update_output();
        },
      | Message::UnwrapPressed =>
        if let Some((index, terms)) = unwrap_valid(self.selected, &self.input) {
          self.input.remove(index);
          for term in terms {
            self.input.insert(index, term.clone());
          }
          self.update_output();
          if index >= self.input.len() {
            self.selected = None;
          }
        },
      | Message::SwapPressed =>
        if let Some((index1, index2)) = swap_valid(self.selected, &self.input) {
          self.input.swap(index1, index2);
          self.update_output();
        },
      | Message::CombinePressed =>
        if let Some((index, mut terms, second_terms)) = combine_valid(self.selected, &self.input) {
          self.input.remove(index + 1);
          terms.extend(second_terms);
          self.input[index] = Term::make_quote(&self.arena, terms.clone()).clone();
          self.update_output();
          if index >= self.input.len() {
            self.selected = None;
          }
        },
      | Message::UpPressed => {
        self.selected = Some(if let Some(index) = self.selected {
                               index.saturating_sub(1)
                             } else {
                               self.input.len().saturating_sub(1)
                             });
      },
      | Message::DownPressed => {
        self.selected = Some(if let Some(index) = self.selected {
                               self.input.len().saturating_sub(1).min(index + 1)
                             } else {
                               0
                             });
      },
    };
    Command::none()
  }
}

fn main() -> iced::Result { App::run(Settings::default()) }
