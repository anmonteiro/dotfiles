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
  call dein#add('morhetz/gruvbox')
  call dein#add('reasonml-editor/vim-reason-plus')
  call dein#add('autozimu/LanguageClient-neovim', {
    \ 'rev': 'next',
    \ 'build': 'bash install.sh',
    \ 'on_ft': ['reason', 'ocaml'],
    \ })
  call dein#add('airblade/vim-gitgutter')
  call dein#add('tpope/vim-fireplace', {
    \ 'on_ft': ['clojure'],
    \})
  call dein#add('jiangmiao/auto-pairs', {
    \ 'on_ft': ['ocaml', 'reason', 'javascript', 'vim', 'sql', 'lisp', 'clojure', 'nix', 'json'],
    \})
  call dein#add('vim-airline/vim-airline')
  call dein#add('vim-airline/vim-airline-themes')
  call dein#add('jordwalke/vim-taste')
  call dein#add('scrooloose/nerdcommenter')
  call dein#add('jreybert/vimagit', { 'rev': 'next' })
  call dein#add('ctrlpvim/ctrlp.vim')
  call dein#add('tpope/vim-surround', {
    \ 'on_ft': ['ocaml', 'reason', 'javascript', 'vim', 'sql', 'lisp', 'nix'],
    \})
  call dein#add('tpope/vim-fugitive')
  call dein#add('LnL7/vim-nix', {
    \ 'on_ft': ['nix'],
    \})

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
