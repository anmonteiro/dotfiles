# Rex

## Setup

From the root of this checkout, with Rex running:

```sh
ln -sfn "$PWD/rex" "$HOME/.config/rex"
rex config check
rex config reload
rex do client.theme.import path="$PWD/rex/themes/Taste.ghostty"
```

In Rex settings:

- **Themes > Dark Mode:** select **Taste** and turn **Use extended theme palette** off.
- **Terminal > Colors:** turn **Force theme palette for text**, **Improve text contrast**,
  and **Use Display P3 color space** off.
- **Terminal > Typography:** select **Fira Code**, **12 pt**.
- **Terminal > Cursor:** turn blinking off.
- **Interface:** select **Classic**, turn **Tint with theme** on, and turn
  inactive-pane fading off under **Active Pane**.

## Reload

After changing the config, run `rex config check`, then `rex config reload`
(or press **Ctrl+A**, then **r**).
After changing the theme, rerun the theme import command above.

## Copy mode and navigation

Press **Ctrl+A**, then **[** to enter copy mode. Use vi motions to move,
**v** to select, **Ctrl+V** for a rectangular selection, and **y** to copy.
**Enter** copies and exits; **q** or **Esc** exits.
Press **Ctrl+A**, then **]** to paste the clipboard.

- **Alt+H/J/K/L:** switch panes in copy mode.
- **Ctrl+A**, then **l**: toggle between the last two tabs in the current session.

Last-tab switching uses Rex's built-in `focus_history.reverse` API, available in
Rex Server `0.1.0-preview.23`. An older running server must be restarted into a
version providing this API; reloading the config alone does not upgrade it.

## Find a Codex session

Press **Ctrl+A**, then **f**, paste the session UUID, and press **Enter**.
Or run:

```sh
rex do find_codex_session id=YOUR_SESSION_UUID
```
