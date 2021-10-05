use async_std::channel::{unbounded, Receiver, Sender};
use async_std::sync::{Arc, Mutex};
use async_std::{io};

use crate::ast::{Rule, Term};
use crate::parser::parse_terms;
use crate::prolog::codegen::{generate, generate_query};
use crate::prolog::util::AssertLocation;
use crate::prolog::{pred, ActivatedEngine, Context, ContextExt, Module};
use crate::view::{State, View};

pub struct PrologHandler<'a> {
  ctx:Context<'a, ActivatedEngine<'a>>,
  sender:Sender<String>,
  receiver:Receiver<Vec<Term>>,
}

impl<'a> PrologHandler<'a> {
  pub async fn handle(&mut self) {
    if let Ok(terms) = self.receiver.recv().await {
      if let Ok((list, other)) = generate_query(&self.ctx, &terms) {
        if let Ok(()) = self.ctx.call_once(pred![mlatu: rewrite / 2], [&list, &other]) {
          if let Some(canon) = self.ctx.canonical(&other) {
            self.sender.send(canon).await.unwrap()
          }
        }
      }
    }
  }
}

pub struct Interactive {
  view:View,
  should_quit:bool,
  state:State,
  sender:Sender<Vec<Term>>,
  receiver:Receiver<String>,
}

fn die(e:&std::io::Error) -> ! {
  let _result = crossterm::terminal::Clear(crossterm::terminal::ClearType::All);
  let _result = crossterm::terminal::disable_raw_mode();
  panic!("{}", e)
}

impl Interactive {
  /// # Errors
  ///
  /// Will return `Err` if there was an error constructing a terminal
  pub fn new<'a>(rules:Vec<Rule>, ctx:Context<'a, ActivatedEngine<'a>>)
                 -> io::Result<(Self, PrologHandler<'a>)> {
    let module = Module::new("mlatu");
    let clauses = generate(&ctx, &rules).unwrap();
    for clause in clauses {
      ctx.assert(&clause.clause, Some(&module), AssertLocation::Last).unwrap();
    }
    let (string_sender, string_receiver) = unbounded();
    let (terms_sender, terms_receiver) = unbounded();
    crossterm::terminal::enable_raw_mode()?;
    let should_quit = false;
    let rule = Arc::new(Mutex::new((vec![Term::Word(Arc::new("quote".to_string()))], vec![])));
    let state = State::AtLeft;
    let default_status = "mlatu interface".to_string();
    let view = View::new(Arc::clone(&rule),
                         ("| Input |".to_string(), "| Output |".to_string()),
                         default_status)?;
    Ok((Self { view, should_quit, state, sender:terms_sender, receiver:string_receiver },
        PrologHandler { ctx, sender:string_sender, receiver:terms_receiver }))
  }

  pub async fn run(&mut self, mut handler: PrologHandler<'_>) {
    loop {
      if let Err(error) = self.view.refresh_screen(self.state.clone(), self.should_quit).await {
        die(&error);
      }
      if self.should_quit {
        let _result = crossterm::terminal::disable_raw_mode();
        break
      }
      if let Err(error) = self.process_keypress().await {
        die(&error);
      }
      handler.handle().await;
      if let Ok(string) = self.receiver.recv().await {
        let mut guard = self.view.lock().await;
        guard.1 = vec![Term::Word(Arc::new(string))];
      }
    }
  }

  async fn submit_input(&mut self, s:&str, state:&State) {
    if let Ok(terms) = parse_terms(s) {
      let mut guard = self.view.lock().await;

      match state {
        | State::AtLeft => {
          guard.0 = terms;
          self.state = if guard.0.is_empty() { State::AtLeft } else { State::InLeft(0) };
          self.sender.send(guard.0.clone()).await.unwrap();
        },
        | State::InLeft(index) => {
          let mut index = *index;
          for term in terms {
            guard.0.insert(index, term);
            index += 1;
          }
          self.state = if guard.0.is_empty() {
            State::AtLeft
          } else {
            State::InLeft((index - 1).min(guard.0.len() - 1))
          };
          self.sender.send(guard.0.clone()).await.unwrap();
        },
        | State::AtRight | State::InRight(_) | State::Editing(..) => {},
      }
    }
  }

  async fn process_backspace(&mut self) {
    match self.state.clone() {
      | State::Editing(s, state) => {
        let mut s = s;
        s.pop();
        self.state = State::Editing(s, state);
      },
      | State::InLeft(index) => {
        let mut guard = self.view.lock().await;
        guard.0.remove(index);
        self.state = if guard.0.is_empty() {
          State::AtLeft
        } else {
          State::InLeft(index.min(guard.0.len() - 1))
        };
        self.sender.send(guard.0.clone()).await.unwrap();
      },
      | _ => {},
    }
  }

  async fn process_keypress(&mut self) -> io::Result<()> {
    use crossterm::event::KeyCode::{Backspace, Char, Delete, Down, Enter, Esc, Up};

    let event = self.view.read_key()?;
    match (event.code, event.modifiers) {
      | (Esc, _) => self.should_quit = true,
      | (Char(c), _) => match (c, self.state.clone()) {
        | (c, State::Editing(s, state)) => {
          let mut s = s;
          s.push(c);
          self.state = State::Editing(s, state);
        },
        | ('q', State::InLeft(index)) => {
          let mut guard = self.view.lock().await;
          guard.0[index] = Term::new_quote(vec![guard.0[index].clone()]);
          self.sender.send(guard.0.clone()).await.unwrap();
        },
        | ('i', State::InLeft(index)) => {
          let mut guard = self.view.lock().await;
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
            self.sender.send(guard.0.clone()).await.unwrap();
          }
        },
        | (' ', _) => self.state = State::Editing(String::new(), Box::new(self.state.clone())),

        | _ => {},
      },
      | (Backspace | Delete, _) => self.process_backspace().await,
      | (Enter, _) => match self.state.clone() {
        | State::Editing(s, state) => self.submit_input(&s, &*state).await,
        | _ => {},
      },
      | (Up, _) => match self.state {
        | State::InLeft(index) => {
          let guard = self.view.lock().await;
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
          let guard = self.view.lock().await;
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
