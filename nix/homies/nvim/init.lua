
local Plug = vim.fn['plug#']

local plugin_path = vim.fn.expand("$HOME") .. "/.config/nvim/plugged"
vim.call('plug#begin', plugin_path)
  --- Theming
  Plug('vim-airline/vim-airline')
  Plug('jordwalke/vim-taste')
  Plug('Yggdroot/indentLine')

  --- Editing
  Plug('tpope/vim-surround')
  Plug('jiangmiao/auto-pairs')
  Plug('guns/vim-sexp')
  Plug('tpope/vim-sexp-mappings-for-regular-people')
  Plug('scrooloose/nerdcommenter')
  Plug('vim-syntastic/syntastic')
  Plug('luochen1990/rainbow', { ['for'] = {'lisp', 'dune', 'clojure'} })
  Plug('guns/vim-clojure-static', { ['for'] = 'clojure' })
  Plug('vim-scripts/ShowTrailingWhitespace')
  Plug('neovim/nvim-lspconfig')

  --- Automatic Formatting
  Plug('sbdchd/neoformat')

  --- Navigation
  Plug('christoomey/vim-tmux-navigator')

  Plug('nvim-treesitter/nvim-treesitter', { ['do'] = ':TSUpdate'})
  Plug('nvim-lua/plenary.nvim')
  Plug('nvim-telescope/telescope.nvim', { tag = '0.1.8' })

  --- Git
  Plug('mhinz/vim-signify')
  Plug('tpope/vim-fugitive')

  --- OCaml / Reason
  Plug('rgrinberg/vim-ocaml', { ['for'] = {'ocaml', 'reason', 'opam', 'dune'} })
  Plug('tjdevries/ocaml.nvim', { ['do'] = ':lua require(\"ocaml\").update()'})
  Plug('nvim-treesitter/playground')

  --- Clojure
  Plug('tpope/vim-fireplace', { ['for'] = 'clojure' })

  --- Nix
  Plug('LnL7/vim-nix', { ['for'] = 'nix' })

  --- MDX
  Plug('jxnblk/vim-mdx-js', { ['for'] = 'mdx' })

  --- GraphQL
  Plug('jparise/vim-graphql')

  --- HCL / Terraform
  Plug('hashivim/vim-terraform', { ['for'] = {'terraform', 'hcl'}})

  --- Coq
  Plug('whonore/Coqtail') --, { 'for': 'coq' }

  --- DB
  Plug('tpope/vim-dadbod')
  Plug('kristijanhusak/vim-dadbod-ui')
  Plug('kristijanhusak/vim-dadbod-completion')
vim.call('plug#end')

require('ui')
-- order-dependent since "editing.lua" sets leader/local leader
require('editing')
require('navigation')
require('lsp')
require('plugins.telescope')
