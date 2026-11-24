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

## Find a Codex session

Press **Ctrl+A**, then **f**, paste the session UUID, and press **Enter**.
Or run:

```sh
rex do find_codex_session id=YOUR_SESSION_UUID
```
