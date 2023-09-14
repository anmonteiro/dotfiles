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
  Plug 'neovim/nvim-lspconfig'

  """ Automatic Formatting
  Plug 'sbdchd/neoformat'

  """ Navigation
  Plug $NVIM_CONFIG_FZF_PATH . '/share/vim-plugins/*'
  Plug 'junegunn/fzf.vim'
  Plug 'christoomey/vim-tmux-navigator'

  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
  Plug 'nvim-lua/plenary.nvim'
  Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.3' }

  """ Git
  Plug 'mhinz/vim-signify'
  Plug 'tpope/vim-fugitive'

  """ OCaml / Reason
  Plug 'rgrinberg/vim-ocaml', {'for': ['ocaml', 'reason', 'opam', 'dune'] }
  Plug 'tjdevries/ocaml.nvim', {'do': ':lua require(\"ocaml\").update()'}
  Plug 'nvim-treesitter/playground'

  """ Clojure
  Plug 'tpope/vim-fireplace', {'for': 'clojure' }

  """ Nix
  Plug 'LnL7/vim-nix', { 'for': 'nix' }

  """ MDX
  Plug 'jxnblk/vim-mdx-js', { 'for': 'mdx' }

  """ GraphQL
  Plug 'jparise/vim-graphql'

  """ HCL / Terraform
  Plug 'hashivim/vim-terraform', {'for': ['terraform', 'hcl']}

  """ Coq
  Plug 'whonore/Coqtail'
  ", { 'for': 'coq' }

  """ DB
  Plug 'tpope/vim-dadbod'
  Plug 'kristijanhusak/vim-dadbod-ui'
  Plug 'kristijanhusak/vim-dadbod-completion'


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

lua require('lsp')
lua require('editing')
lua require('plugins.telescope')

call SourceDirectory($NVIM_CONFIG_CUSTOMIZATIONS_PATH)
