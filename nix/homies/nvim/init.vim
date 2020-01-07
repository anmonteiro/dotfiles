if &compatible
  set nocompatible               " Be iMproved
endif

call plug#begin($NVIM_CONFIG_PLUGINS_PATH)
  """ Theming
  Plug 'vim-airline/vim-airline'
  " Plug 'vim-airline/vim-airline-themes'
  Plug 'jordwalke/vim-taste'
  Plug 'Yggdroot/indentLine'

  """ Editing
  Plug 'tpope/vim-surround'
  Plug 'jiangmiao/auto-pairs'
  Plug 'guns/vim-sexp'
  Plug 'tpope/vim-sexp-mappings-for-regular-people'
  Plug 'scrooloose/nerdcommenter'
  Plug 'vim-syntastic/syntastic'
  Plug 'luochen1990/rainbow', { 'for': ['lisp', 'dune', 'clojure'] }
  Plug 'guns/vim-clojure-static', { 'for': 'clojure' }
  Plug 'vim-scripts/ShowTrailingWhitespace'

  """ Automatic Formatting
  Plug 'sbdchd/neoformat'

  """ Navigation
  Plug $NVIM_CONFIG_FZF_PATH . '/share/vim-plugins/*'
  Plug 'junegunn/fzf.vim'
  Plug 'christoomey/vim-tmux-navigator'

  """ Git
  Plug 'mhinz/vim-signify'
  Plug 'jreybert/vimagit', { 'branch': 'next' }
  Plug 'tpope/vim-fugitive'

  """ OCaml / Reason
  Plug 'rgrinberg/vim-ocaml', {'for': ['ocaml', 'opam', 'dune'] }
  Plug 'jordwalke/vim-reasonml', {'for': ['reason', 'ocaml'] }

  """ Clojure
  Plug 'tpope/vim-fireplace', {'for': 'clojure' }

  """ Nix
  Plug 'LnL7/vim-nix', { 'for': 'nix' }

  """ MDX
  Plug 'jxnblk/vim-mdx-js', { 'for': 'mdx' }

  " call dein#add('MartinLafreniere/vim-PairTools', {
    " \ 'on_ft': ['ocaml', 'reason', 'javascript', 'vim', 'sql'],
    " \})
call plug#end()

" Required:
filetype plugin indent on
syntax enable

" Function to source all .vim files in directory
" (https://devel.tech/snippets/n/vIMvi29n/include-all-vim-files-in-a-directory)
" {
function! SourceDirectory(file)
  for s:fpath in split(globpath(a:file, '*.vim'), '\n')
    exe 'source' s:fpath
  endfor
endfunction
" }

call SourceDirectory($NVIM_CONFIG_CUSTOMIZATIONS_PATH)

