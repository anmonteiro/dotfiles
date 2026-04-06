return {
  {
    -- Disabled for now. Re-enable selectively if you miss any of these.
    -- "guns/vim-sexp",
    -- "tpope/vim-sexp-mappings-for-regular-people",
    -- { "LnL7/vim-nix", ft = { "nix" } },
    {
      "davidmh/mdx.nvim",
      lazy = false,
      dependencies = { "nvim-treesitter/nvim-treesitter" },
    },
    -- "jparise/vim-graphql",
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
