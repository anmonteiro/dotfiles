" True color terminal support
set termguicolors

"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
  if (has("nvim"))
  "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  endif
  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
  if (has("termguicolors"))
    set termguicolors
  endif
endif

" Disable GUI options support – improves startup time
set guioptions=M
let did_install_default_menus = 1
let did_install_syntax_menu = 1
" Disable some default plugins.
let g:loaded_gzip              = 1
let g:loaded_tar               = 1
let g:loaded_tarPlugin         = 1
let g:loaded_zip               = 1
let g:loaded_zipPlugin         = 1
let g:loaded_rrhelper          = 1
let g:loaded_2html_plugin      = 1
let g:loaded_vimball           = 1
let g:loaded_vimballPlugin     = 1
let g:loaded_getscript         = 1
let g:loaded_getscriptPlugin   = 1
let g:loaded_netrw             = 1
let g:loaded_netrwPlugin       = 1
let g:loaded_netrwSettings     = 1
let g:loaded_netrwFileHandlers = 1
let g:loaded_logiPat           = 1

" Color Scheme
"let g:gruvbox_number_column = 'bg1'
" let g:gruvbox_italic = 1
"let g:gruvbox_italicize_strings = 1
" let g:gruvbox_improved_strings = 1
"let g:gruvbox_improved_warnings = 1

" let g:gruvbox_contrast_dark = 'hard'
set background=dark
let g:taste_allow_italics = 1
colorscheme taste
" Line numbers
set number
" Show line / column on the status bar
set ruler
" Set the sign column
set signcolumn=yes
" 80 column rule
set colorcolumn=80
" hi ColorColumn ctermbg=darkblue guibg=darkblue
" Highlight the line the cursor is currently on

set cursorline

""" Airline related settings
let g:airline_highlighting_cache = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_nr_show = 1

let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = ' '

let g:airline_theme='taste'

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.crypt = '🔒'
let g:airline_symbols.linenr = '☰'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.maxlinenr = '㏑'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.spell = 'Ꞩ'
let g:airline_symbols.notexists = 'Ɇ'
let g:airline_symbols.whitespace = 'Ξ'

" powerline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = '☰'
let g:airline_symbols.maxlinenr = ''


" Airline already shows the current mode
set noshowmode

set list
set listchars=tab:▸\ ,eol:¬
highlight NonText ctermfg=7 guifg=gray guibg=NONE ctermbg=NONE
"Invisible character colors
"4a4a59
"highlight NonText guifg=#eeeeee
"highlight SpecialKey guifg=#4a4a59

if !has("gui_vimr")
  hi Normal ctermbg=NONE guibg=NONE
endif

autocmd VimEnter * highlight Comment cterm=italic gui=italic

""" Vim-Signify
let g:signify_vcs_list = [ 'git' ]
" let g:signify_realtime = 1
let g:signify_sign_add               = '⨁'
let g:signify_sign_change            = '✎'
highlight SignColumn ctermbg=NONE guibg=NONE

""" Indent Guides
let g:indentguides_spacechar = '│'
