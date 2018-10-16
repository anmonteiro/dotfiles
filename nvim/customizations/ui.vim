" True color terminal support
set termguicolors

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
let g:gruvbox_number_column = 'bg1'
colorscheme gruvbox
set background=dark
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

" Airline related settings
let g:airline_highlighting_cache = 1
let g:airline_theme='tomorrow'
" Airline already shows the current mode
set noshowmode

set listchars=tab:▸\ ,eol:¬
set list
"Invisible character colors
"4a4a59
"highlight NonText guifg=#eeeeee
"highlight SpecialKey guifg=#4a4a59
