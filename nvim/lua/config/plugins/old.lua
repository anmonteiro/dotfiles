return {
  {
    -- Theming
    "vim-airline/vim-airline",
    "jordwalke/vim-taste",
    "Yggdroot/indentLine",

    --- Editing
    "tpope/vim-surround",
    "guns/vim-sexp",
    "tpope/vim-sexp-mappings-for-regular-people",
    {
      "scrooloose/nerdcommenter",
      config = function()
        vim.g.NERDSpaceDelims = 1
        vim.g.NERDCustomDelimiters = { reason = { left = "//", leftAlt = "/*", rightAlt = "*/" } }
      end,
    },
    "vim-syntastic/syntastic",
    "luochen1990/rainbow",
    -- 'guns/vim-clojure-static'
    "vim-scripts/ShowTrailingWhitespace",

    --- Automatic Formatting
    "sbdchd/neoformat",

    --- Git
    "mhinz/vim-signify",
    "tpope/vim-fugitive",

    { "LnL7/vim-nix", ft = { "nix" } },

    --- MDX
    "jxnblk/vim-mdx-js", -- { ['for'] = 'mdx' })

    --- GraphQL
    "jparise/vim-graphql",

    --- HCL / Terraform
    "hashivim/vim-terraform", -- { ['for'] = {'terraform', 'hcl'}})
  },
}

-- 'nvim-treesitter/playground')

--- Clojure
-- 'tpope/vim-fireplace', { ['for'] = 'clojure' })
--- Coq
-- 'whonore/Coqtail') --, { 'for': 'coq' }
