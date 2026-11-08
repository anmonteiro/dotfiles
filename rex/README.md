# Rex

`init.lua` provides the tmux-style keybindings and Vim-aware pane navigation.

## Taste theme

[Taste.ghostty](themes/Taste.ghostty) matches the terminal colors in
[the Kitty config](../nix/homies/kitty/kitty.conf), copied from
[vim-taste's Kitty palette](https://github.com/jordwalke/vim-taste/blob/3d2990876be923856b539d509a8d83d2ca447760/terminals/kitty.conf).
The upstream iTerm export has different colors; the Kitty palette matches this
setup. Neovim continues to use its own Taste syntax highlights.

The background is `#303030`, foreground `#d4d4d4`, and selection background
`#464646`. Cursor colors match Kitty's effective defaults: `#cccccc` over
`#111111` text.

| ANSI color | Standard (0-7) | Bright (8-15) |
| --- | --- | --- |
| Black | `#202020` | `#565656` |
| Red | `#f58c84` | `#e25256` |
| Green | `#a5d75a` | `#d6fb8b` |
| Yellow | `#fadf78` | `#fcf19c` |
| Blue | `#78bcfb` | `#9ad7fc` |
| Magenta | `#c9a2f4` | `#e1c8f8` |
| Cyan | `#5e9df2` | `#4e8de2` |
| White | `#e4e4e4` | `#ffffff` |

Extended colors remain the standard xterm palette. In particular, tmux's
`colour75` stays `#5fafff` and `colour214` stays `#ffaf00`; its status background
uses Taste's bright green (`colour10`, `#d6fb8b`).

## Install or update

From the root of the dotfiles checkout, with Rex running:

```sh
rex do client.theme.import path="$PWD/rex/themes/Taste.ghostty"
```

If this directory is already symlinked to `~/.config/rex`, this also works:

```sh
rex do client.theme.import path="$HOME/.config/rex/themes/Taste.ghostty"
```

Alternatively, open **Settings > Themes > Import Theme** and select the file.
Importing selects the theme. Rex stores an imported copy, so import again after
editing the file; reloading `init.lua` does not update the theme.

If several app clients are connected, target the intended one with
`rex -C CLIENT_ID do ...`; `rex client ls` lists them.

## Match Kitty's rendering

These are Rex application settings, separate from the imported colors:

- **Themes > Dark Mode:** select **Taste**, with **Use extended theme palette**
  off. The file also disables palette generation to preserve xterm's colors.
- **Terminal > Colors:** turn off **Force theme palette for text**,
  **Improve text contrast**, and **Use Display P3 color space**. This preserves
  the original sRGB colors, including colors supplied by programs such as Neovim.
- **Terminal > Typography:** **Fira Code**, **12 pt**, matching the Kitty config.
- **Terminal > Cursor:** turn blinking off to match Kitty.
- For the same opaque appearance, use **Classic** interface style with
  **Tint with theme** on, and turn off inactive-pane fading under
  **Interface > Active Pane**.

See [Rex's settings reference](https://www.superlogical.com/rex/docs/reference/mac-settings)
for the current controls. Font rasterization and native window decorations can
still differ between applications.
