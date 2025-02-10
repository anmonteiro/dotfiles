return {
  {
    --- Editing
    "tpope/vim-surround",
    "guns/vim-sexp",
    "tpope/vim-sexp-mappings-for-regular-people",
    {
      "scrooloose/nerdcommenter",
      init = function()
        vim.g.NERDSpaceDelims = 1
        vim.g.NERDCustomDelimiters = { reason = { left = "//", leftAlt = "/*", rightAlt = "*/" } }
      end,
    },
    {
      "vim-syntastic/syntastic",
      init = function()
        vim.opt.statusline = vim.opt.statusline + "%#warningmsg#" + "%{SyntasticStatuslineFlag()}" + "%*"
        vim.g.syntastic_always_populate_loc_list = 1
        vim.g.syntastic_aggregate_errors = 1
        vim.g.syntastic_check_on_open = 1
        vim.g.syntastic_check_on_wq = 0
        vim.g.syntastic_error_symbol = "✗"
        vim.g.syntastic_warning_symbol = "⚠"
        vim.cmd("highlight link SyntasticWarningSign Typedef")
      end,
    },
    {
      "luochen1990/rainbow",
      init = function()
        -- Rainbow Parens
        vim.g.rainbow_active = 1
      end,
    },

    { "LnL7/vim-nix", ft = { "nix" } },
    "jxnblk/vim-mdx-js", -- { ['for'] = 'mdx' })
    "jparise/vim-graphql",
    { "hashivim/vim-terraform", ft = { "terraform", "hcl" } },
  },
}

-- 'nvim-treesitter/playground')

--- Clojure
-- 'guns/vim-clojure-static'
-- 'tpope/vim-fireplace', { ['for'] = 'clojure' })
-- vim.g.clojure_align_subforms = 1

-- vim.g.clojure_special_indent_words = "deftype,defrecord,reify,proxy,extend-type,extend-protocol,letfn,cond"

--- Coq
-- 'whonore/Coqtail') --, { 'for': 'coq' }
