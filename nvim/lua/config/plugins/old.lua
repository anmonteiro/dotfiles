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
    {
      "luochen1990/rainbow",
      config = function()
        -- Rainbow Parens
        vim.g.rainbow_active = 1
      end,
    },
    -- 'guns/vim-clojure-static'
    "vim-scripts/ShowTrailingWhitespace",

    --- Git
    "mhinz/vim-signify",
    "tpope/vim-fugitive",

    { "LnL7/vim-nix", ft = { "nix" } },

    --- MDX
    "jxnblk/vim-mdx-js", -- { ['for'] = 'mdx' })

    --- GraphQL
    "jparise/vim-graphql",

    --- HCL / Terraform
    { "hashivim/vim-terraform", ft = { "terraform", "hcl" } },
  },
}

-- 'nvim-treesitter/playground')
--- Clojure
-- 'tpope/vim-fireplace', { ['for'] = 'clojure' })
--- Coq
-- 'whonore/Coqtail') --, { 'for': 'coq' }
