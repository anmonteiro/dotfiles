use std::cmp::max;
use std::io::{self, IsTerminal, Read};
use std::path::PathBuf;

use anyhow::{Context, Result, bail};
use clap::{Parser, ValueEnum};
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use crossterm::execute;
use crossterm::terminal::{
    EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode,
};
use ratatui::backend::CrosstermBackend;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Paragraph, Wrap};
use ratatui::{Frame, Terminal};

use fff_tui::{
    FileMatch, FileSearchEngine, FileSearchView, GitKind, HistoryDirection, HistoryMatch,
    HistorySearchEngine, HistorySearchView, PickerMode, clamp_selected, ensure_selection_visible,
    fuzzy_match_indices, load_history_commands, move_selection_down, move_selection_up,
    selected_label, truncate, truncate_path,
};

#[derive(Debug, Clone, Copy, ValueEnum)]
enum ModeArg {
    Files,
    History,
}

#[derive(Debug, Parser)]
struct Cli {
    #[arg(value_enum)]
    mode: ModeArg,
    #[arg(long)]
    base_path: Option<PathBuf>,
    #[arg(long, env = "FFF_HISTORY_QUERY")]
    query: Option<String>,
    #[arg(long, env = "FFF_HISTORY_DIRECTION", default_value = "backward")]
    history_direction: String,
}

enum PickerEngine {
    Files(FileSearchEngine),
    History(HistorySearchEngine),
}

struct App {
    mode: PickerMode,
    engine: PickerEngine,
    query: String,
    selected: usize,
    scroll: usize,
    files: Option<FileSearchView>,
    history: Option<HistorySearchView>,
}

impl App {
    fn new_files(base_path: PathBuf) -> Result<Self> {
        let engine = FileSearchEngine::new(base_path)?;
        let mut app = Self {
            mode: PickerMode::Files,
            engine: PickerEngine::Files(engine),
            query: String::new(),
            selected: 0,
            scroll: 0,
            files: None,
            history: None,
        };
        app.refresh()?;
        Ok(app)
    }

    fn new_history(query: String, direction: HistoryDirection) -> Result<Self> {
        let mut stdin_data = Vec::new();
        if !io::stdin().is_terminal() {
            io::stdin().read_to_end(&mut stdin_data)?;
        }
        let commands = load_history_commands(&stdin_data, direction);
        let engine = HistorySearchEngine::new(commands)?;
        let mut app = Self {
            mode: PickerMode::History,
            engine: PickerEngine::History(engine),
            query,
            selected: 0,
            scroll: 0,
            files: None,
            history: None,
        };
        app.refresh()?;
        Ok(app)
    }

    fn result_len(&self) -> usize {
        match self.mode {
            PickerMode::Files => self.files.as_ref().map_or(0, |view| view.matches.len()),
            PickerMode::History => self.history.as_ref().map_or(0, |view| view.matches.len()),
        }
    }

    fn refresh(&mut self) -> Result<()> {
        match &self.engine {
            PickerEngine::Files(engine) => {
                self.files = Some(engine.search(&self.query)?);
            }
            PickerEngine::History(engine) => {
                self.history = Some(engine.search(&self.query)?);
            }
        }
        self.selected = clamp_selected(self.selected, self.result_len());
        Ok(())
    }

    fn selected_output(&self) -> Option<String> {
        match self.mode {
            PickerMode::Files => self
                .files
                .as_ref()
                .and_then(|view| view.matches.get(self.selected))
                .map(|item| item.relative_path.clone()),
            PickerMode::History => self
                .history
                .as_ref()
                .and_then(|view| view.matches.get(self.selected))
                .map(|item| item.command.clone()),
        }
    }
}

fn history_direction(value: &str) -> HistoryDirection {
    if value.eq_ignore_ascii_case("forward") {
        HistoryDirection::Forward
    } else {
        HistoryDirection::Backward
    }
}

fn file_git_span(kind: GitKind, selected: bool) -> Span<'static> {
    let color = match kind {
        GitKind::Modified => Color::Yellow,
        GitKind::Added => Color::Cyan,
        GitKind::Deleted => Color::Red,
        GitKind::Renamed => Color::Magenta,
        GitKind::Clean => Color::DarkGray,
    };
    let ch = match kind {
        GitKind::Modified => "M",
        GitKind::Added => "A",
        GitKind::Deleted => "D",
        GitKind::Renamed => "R",
        GitKind::Clean => "·",
    };

    Span::styled(
        ch,
        Style::default().fg(color).bg(if selected {
            Color::DarkGray
        } else {
            Color::Reset
        }),
    )
}

fn badge_spans(item: &FileMatch, selected: bool) -> Vec<Span<'static>> {
    let Some(badge) = &item.badge else {
        return Vec::new();
    };

    let color = if selected {
        Color::LightBlue
    } else if badge.icon == "🔥" {
        Color::Red
    } else {
        Color::Yellow
    };

    vec![
        Span::raw(" "),
        Span::styled(
            format!("{}{}", badge.icon, badge.score),
            Style::default().fg(color).bg(if selected {
                Color::DarkGray
            } else {
                Color::Reset
            }),
        ),
    ]
}

fn file_line(item: &FileMatch, query: &str, selected: bool, width: usize) -> Line<'static> {
    let slash = item
        .relative_path
        .rfind('/')
        .map(|idx| idx + 1)
        .unwrap_or(0);
    let dir = &item.relative_path[..slash];
    let base = &item.relative_path[slash..];
    let badge_width = item
        .badge
        .as_ref()
        .map(|badge| badge.icon.chars().count() + badge.score.to_string().len() + 1)
        .unwrap_or(0);
    let available = width.saturating_sub(4 + badge_width);
    let (dir, base) = truncate_path(dir, base, available);
    let display = format!("{dir}{base}");
    let matched = fuzzy_match_indices(&display, query);

    let mut spans = vec![
        Span::styled(
            if selected { "> " } else { "  " },
            Style::default()
                .fg(if selected {
                    Color::LightBlue
                } else {
                    Color::DarkGray
                })
                .bg(if selected {
                    Color::DarkGray
                } else {
                    Color::Reset
                }),
        ),
        file_git_span(item.git, selected),
        Span::raw(" "),
    ];

    let dir_len = dir.chars().count();
    for (idx, ch) in display.chars().enumerate() {
        let in_match = matched.contains(&idx);
        let is_dir = idx < dir_len;
        let mut style = Style::default().bg(if selected {
            Color::DarkGray
        } else {
            Color::Reset
        });
        style = if in_match {
            style.fg(Color::Cyan).add_modifier(Modifier::BOLD)
        } else if is_dir {
            style.fg(Color::DarkGray)
        } else {
            style.add_modifier(Modifier::BOLD)
        };
        spans.push(Span::styled(ch.to_string(), style));
    }

    spans.extend(badge_spans(item, selected));
    Line::from(spans)
}

fn history_line(item: &HistoryMatch, selected: bool, width: usize) -> Line<'static> {
    let display = truncate(&item.display, width.saturating_sub(2));
    let mut spans = vec![Span::styled(
        if selected { "> " } else { "  " },
        Style::default()
            .fg(if selected {
                Color::LightBlue
            } else {
                Color::DarkGray
            })
            .bg(if selected {
                Color::DarkGray
            } else {
                Color::Reset
            }),
    )];

    for (idx, ch) in display.chars().enumerate() {
        let in_match = item
            .match_ranges
            .iter()
            .any(|(start, end)| idx >= *start && idx < *end);
        let mut style = Style::default().bg(if selected {
            Color::DarkGray
        } else {
            Color::Reset
        });
        style = if in_match {
            style.fg(Color::Cyan).add_modifier(Modifier::BOLD)
        } else {
            style
        };
        spans.push(Span::styled(ch.to_string(), style));
    }

    Line::from(spans)
}

fn render_files(frame: &mut Frame<'_>, area: Rect, app: &mut App) {
    let view = app.files.as_ref().expect("file view");
    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(Style::default().fg(Color::LightGreen))
        .title(" FFFiles ");
    let inner = block.inner(area);
    frame.render_widget(block, area);

    let rows = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(1),
            Constraint::Length(1),
            Constraint::Min(1),
        ])
        .split(inner);

    frame.render_widget(
        Paragraph::new(Line::from(vec![
            Span::styled("🪿 ", Style::default().fg(Color::Cyan)),
            Span::raw(app.query.as_str()),
        ])),
        rows[0],
    );

    let left = format!(
        "{} ({} loaded)",
        selected_label(app.selected, view.total_matched),
        view.loaded
    );
    let header = Line::from(vec![
        Span::styled(left, Style::default().add_modifier(Modifier::BOLD)),
        Span::raw(" "),
        Span::styled(
            view.root_display.as_str(),
            Style::default().fg(Color::DarkGray),
        ),
    ]);
    frame.render_widget(Paragraph::new(header), rows[1]);

    let visible_count = max(1, rows[2].height as usize);
    app.scroll = ensure_selection_visible(app.selected, app.scroll, visible_count);
    let visible = view
        .matches
        .iter()
        .skip(app.scroll)
        .take(visible_count)
        .enumerate()
        .map(|(offset, item)| {
            file_line(
                item,
                &app.query,
                app.scroll + offset == app.selected,
                rows[2].width as usize,
            )
        })
        .collect::<Vec<_>>();
    frame.render_widget(Paragraph::new(visible).wrap(Wrap { trim: false }), rows[2]);
}

fn render_history(frame: &mut Frame<'_>, area: Rect, app: &mut App) {
    let view = app.history.as_ref().expect("history view");
    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(Style::default().fg(Color::LightGreen))
        .title(" FFFHistory ");
    let inner = block.inner(area);
    frame.render_widget(block, area);

    let rows = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(1),
            Constraint::Min(1),
            Constraint::Length(1),
        ])
        .split(inner);

    let right = format!("{} shown", view.matches.len());
    let header = Line::from(vec![
        Span::styled(
            selected_label(app.selected, view.total_matched),
            Style::default().add_modifier(Modifier::BOLD),
        ),
        Span::raw(" "),
        Span::styled(right, Style::default().fg(Color::DarkGray)),
    ]);
    frame.render_widget(Paragraph::new(header), rows[0]);

    let visible_count = max(1, rows[1].height as usize);
    app.scroll = ensure_selection_visible(app.selected, app.scroll, visible_count);
    let visible = view
        .matches
        .iter()
        .skip(app.scroll)
        .take(visible_count)
        .enumerate()
        .map(|(offset, item)| {
            history_line(
                item,
                app.scroll + offset == app.selected,
                rows[1].width as usize,
            )
        })
        .collect::<Vec<_>>();
    frame.render_widget(Paragraph::new(visible).wrap(Wrap { trim: false }), rows[1]);

    frame.render_widget(
        Paragraph::new(Line::from(vec![
            Span::styled("🪿 ", Style::default().fg(Color::Cyan)),
            Span::raw(app.query.as_str()),
        ])),
        rows[2],
    );
}

fn render(frame: &mut Frame<'_>, app: &mut App) {
    match app.mode {
        PickerMode::Files => render_files(frame, frame.area(), app),
        PickerMode::History => render_history(frame, frame.area(), app),
    }
}

fn handle_key(app: &mut App, key: KeyEvent) -> Result<Option<String>> {
    if key.kind != KeyEventKind::Press {
        return Ok(None);
    }

    match key.code {
        KeyCode::Enter => return Ok(app.selected_output()),
        KeyCode::Esc => bail!("cancelled"),
        KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => bail!("cancelled"),
        KeyCode::Up => {
            let wrap = matches!(app.mode, PickerMode::Files);
            app.selected = move_selection_up(app.selected, app.result_len(), wrap);
        }
        KeyCode::Char('p') if key.modifiers.contains(KeyModifiers::CONTROL) => {
            let wrap = matches!(app.mode, PickerMode::Files);
            app.selected = move_selection_up(app.selected, app.result_len(), wrap);
        }
        KeyCode::Down => {
            let wrap = matches!(app.mode, PickerMode::Files);
            app.selected = move_selection_down(app.selected, app.result_len(), wrap);
        }
        KeyCode::Char('n') if key.modifiers.contains(KeyModifiers::CONTROL) => {
            let wrap = matches!(app.mode, PickerMode::Files);
            app.selected = move_selection_down(app.selected, app.result_len(), wrap);
        }
        KeyCode::Backspace => {
            if !app.query.is_empty() {
                app.query.pop();
                app.selected = 0;
                app.scroll = 0;
                app.refresh()?;
            }
        }
        KeyCode::Char(ch) if !key.modifiers.contains(KeyModifiers::CONTROL) => {
            app.query.push(ch);
            app.selected = 0;
            app.scroll = 0;
            app.refresh()?;
        }
        _ => {}
    }

    Ok(None)
}

fn run(app: &mut App) -> Result<Option<String>> {
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    let result = loop {
        terminal.draw(|frame| render(frame, app))?;

        if !event::poll(std::time::Duration::from_millis(100))? {
            continue;
        }

        match event::read()? {
            Event::Key(key) => {
                if let Some(output) = handle_key(app, key)? {
                    break Some(output);
                }
            }
            Event::Resize(_, _) => {}
            _ => {}
        }
    };

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;
    Ok(result)
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let mut app = match cli.mode {
        ModeArg::Files => {
            let base_path = cli
                .base_path
                .unwrap_or(std::env::current_dir().context("failed to resolve current directory")?);
            App::new_files(base_path)?
        }
        ModeArg::History => App::new_history(
            cli.query.unwrap_or_default(),
            history_direction(&cli.history_direction),
        )?,
    };

    match run(&mut app) {
        Ok(Some(output)) => {
            print!("{output}");
            Ok(())
        }
        Ok(None) => Ok(()),
        Err(error) if error.to_string() == "cancelled" => Ok(()),
        Err(error) => Err(error),
    }
}
