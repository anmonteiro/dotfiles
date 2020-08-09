set hidden

" From `:help Y`:
"   If you like "Y" to work from the cursor to the end of line (which is more
"   logical, but not Vi-compatible) use ":map Y y$".
map Y y$

" Delete trailing whitespace on save
function! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    " Delete the last entry from the search history, which is the substitution
    " command
    call histdel("search", -1)
    let @/ = histget("search", -1)
    call cursor(l, c)
endfun

autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()

" Share the system clipboard with yank / paste
set clipboard^=unnamed,unnamedplus

" Don't create swap files
set noswapfile

" Set Vim's update time to 1 second – the default is 4 seconds which is too much
set updatetime=200

" show existing tab with 4 spaces width
set tabstop=2
" when indenting with '>', use 4 spaces width
set shiftwidth=2
" On pressing tab, insert 4 spaces
set expandtab
set autoindent

" Ignore case if search contains only lowercase characters
set ignorecase smartcase

" Behavior when completing a command
set completeopt=menuone,preview,noinsert,noselect

" Check if the buffer is open in another tab / window before switching to it
" set switchbuf=usetab,useopen

" NERD Commenter config
let g:NERDSpaceDelims = 1

" Vimagit config
let g:magit_discard_untracked_do_delete = 1

" Set conceal level to 0 on markdown and json files
autocmd FileType json,markdown
    \ autocmd BufWinEnter <buffer> setlocal conceallevel=0

""" leader config

noremap <SPACE> <Nop>
sunmap <SPACE>

map <Space> <leader>

let g:maplocalleader = ","

""" Filetype associations

autocmd BufEnter *.nix :setlocal filetype=nix
autocmd BufRead,BufNewFile Vagrantfile :setlocal filetype=ruby
autocmd BufRead,BufNewFile jbuild :setlocal filetype=dune


""" Syntastic

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_aggregate_errors = 1
" let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_error_symbol = "✗"
let g:syntastic_warning_symbol = "⚠"
highlight link SyntasticWarningSign Typedef

nnoremap <silent> <leader>e :Errors<CR>

""" Rainbow Parens
let g:rainbow_active = 1

""" Automatic formatting
" Enable trimmming of trailing whitespace
"let g:neoformat_basic_format_trim = 1

augroup fmt
  autocmd!
  autocmd BufWritePre * Neoformat
  "autocmd BufWritePre * undojoin | Neoformat
augroup END

" Neoformat JS
let g:neoformat_javascript_prettier = {
  \ 'exe': './node_modules/.bin/prettier',
  \ 'args': ['--single-quote', '--stdin', '--stdin-filepath', '"%:p"'],
  \ 'stdin': 1,
  \ }

let g:neoformat_css_prettier = {
  \ 'exe': './node_modules/.bin/prettier',
  \ 'args': ['--stdin', '--stdin-filepath', '"%:p"', '--parser', 'css'],
  \ 'stdin': 1,
  \ }

let g:neoformat_sql_pg_format = {
  \ 'exe': 'pg_format',
  \ 'args': ['-B', '-s', '2', '-w', '80', '-'],
  \ 'stdin': 1,
  \ }

let g:neoformat_scss_prettier = g:neoformat_css_prettier

" Snoopy Mode
" http://vim.wikia.com/wiki/Invert_the_number_row_keys_for_faster_typing
" map each number to its shift-key character
" inoremap 1 !
" inoremap 2 @
" inoremap 3 #
" inoremap 4 $
" inoremap 5 %
" inoremap 6 ^
" inoremap 7 &
" inoremap 8 *
" inoremap 9 (
" inoremap 0 )
" "inoremap - _
" " and then the opposite
" inoremap ! 1
" inoremap @ 2
" inoremap # 3
" inoremap $ 4
" inoremap % 5
" inoremap ^ 6
" inoremap & 7
" inoremap * 8
" inoremap ( 9
" inoremap ) 0
"inoremap _ -


" autocmd FileType ocaml,reason let g:pairtools_reason_pairclamp = 1
" autocmd FileType ocaml,reason let g:pairtools_reason_tagwrench = 0
" autocmd FileType ocaml,reason let g:pairtools_reason_jigsaw    = 1
" autocmd FileType ocaml,reason let g:pairtools_reason_autoclose  = 1
" autocmd FileType ocaml,reason let g:pairtools_reason_forcepairs = 0
" autocmd FileType ocaml,reason let g:pairtools_reason_closepairs = "(:),[:],{:}" . ',":"'
" autocmd FileType ocaml,reason let g:pairtools_reason_smartclose = 1
" autocmd FileType ocaml,reason let g:pairtools_reason_smartcloserules = '\w,(,&,\*'
" autocmd FileType ocaml,reason let g:pairtools_reason_antimagic  = 1
" autocmd FileType ocaml,reason let g:pairtools_reason_antimagicfield  = "Comment,String,Special"
" autocmd FileType ocaml,reason let g:pairtools_reason_pcexpander = 1
" autocmd FileType ocaml,reason let g:pairtools_reason_pceraser   = 1
" autocmd FileType ocaml,reason let g:pairtools_reason_tagwrenchhook = 'tagwrench#BuiltinNoHook'
" autocmd FileType ocaml,reason let g:pairtools_reason_twexpander = 0
" autocmd FileType ocaml,reason let g:pairtools_reason_tweraser   = 0
" autocmd FileType ocaml,reason let g:pairtools_reason_apostrophe = 0

