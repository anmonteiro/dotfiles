"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=~/.cache/dein/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('/Users/anmonteiro/.cache/dein')
  call dein#begin('/Users/anmonteiro/.cache/dein')

  " Let dein manage dein
  " Required:
  call dein#add('/Users/anmonteiro/.cache/dein/repos/github.com/Shougo/dein.vim')

  " Add or remove your plugins here:
  "  call dein#add('Shougo/neosnippet.vim')
  "  call dein#add('Shougo/neosnippet-snippets')
  " call dein#add('morhetz/gruvbox')
  """ Theming
  call dein#add('vim-airline/vim-airline')
  " call dein#add('vim-airline/vim-airline-themes')
  call dein#add('jordwalke/vim-taste')
  call dein#add('Yggdroot/indentLine')

  """ Editing
  call dein#add('tpope/vim-surround')
  call dein#add('jiangmiao/auto-pairs')
  call dein#add('guns/vim-sexp')
  call dein#add('tpope/vim-sexp-mappings-for-regular-people')
  call dein#add('scrooloose/nerdcommenter')
  call dein#add('vim-syntastic/syntastic')
  call dein#add('luochen1990/rainbow', {
    \ 'on_ft': ['lisp', 'dune', 'clojure'],
    \ })
  call dein#add('guns/vim-clojure-static', {
    \ 'on_ft': ['clojure'],
    \ })

  """ Automatic Formatting
  call dein#add('sbdchd/neoformat')

  """ Navigation
  call dein#add('junegunn/fzf', { 'merged': 0 })
  call dein#add('junegunn/fzf.vim', { 'depends': 'fzf' })

  """ Git
  call dein#add('mhinz/vim-signify')
  call dein#add('jreybert/vimagit', { 'rev': 'next' })
  call dein#add('tpope/vim-fugitive')

  """ OCaml / Reason
  call dein#add('rgrinberg/vim-ocaml' , {
    \ 'on_ft': ['ocaml', 'opam', 'dune'],
    \ })
  call dein#add('jordwalke/vim-reasonml' , {
    \ 'on_ft': ['reason', 'ocaml'],
    \ })
  " call dein#add('reasonml-editor/vim-reason-plus')
    " \ 'on_ft': ['reason', 'ocaml'],
  " call dein#add('autozimu/LanguageClient-neovim', {
    " \ 'rev': 'next',
    " \ 'build': 'bash install.sh',
    " \ 'on_ft': ['reason', 'ocaml'],
    " \ })

  """ Clojure
  call dein#add('tpope/vim-fireplace', {
    \ 'on_ft': ['clojure'],
    \ })

  """ Nix
  call dein#add('LnL7/vim-nix', {
    \ 'on_ft': ['nix'],
    \ })

  " call dein#add('MartinLafreniere/vim-PairTools', {
    " \ 'on_ft': ['ocaml', 'reason', 'javascript', 'vim', 'sql'],
    " \})

  " You can specify revision/branch/tag.
  "call dein#add('Shougo/deol.nvim', { 'rev': '01203d4c9' })

  " Required:
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

"End dein Scripts-------------------------

" Function to source all .vim files in directory
" (https://devel.tech/snippets/n/vIMvi29n/include-all-vim-files-in-a-directory)
" {
function! SourceDirectory(file)
  for s:fpath in split(globpath(a:file, '*.vim'), '\n')
    exe 'source' s:fpath
  endfor
endfunction
" }

call SourceDirectory('~/.config/nvim/customizations/')

if has("gui_vimr")
  source ~/.config/nvim/ginit.vim
endif
