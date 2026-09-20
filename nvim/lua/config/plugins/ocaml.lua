local ocaml_ft = {
  "dune",
  "ocaml",
  "ocaml.cram",
  "ocaml.interface",
  "ocaml.menhir",
  "ocaml.mlx",
  "ocaml.ocamllex",
  "opam",
  "reason",
}

return {
  {
    "rgrinberg/vim-ocaml",
    ft = ocaml_ft,
    init = function()
      require("config.ocaml").setup()
    end,
  },
  {
    "tjdevries/ocaml.nvim",
    config = function()
      require("ocaml").setup({
        install_rapper = false,
        install_mlx = false, -- Managed by config.treesitter with the other parsers.
        setup_lspconfig = false,
        setup_conform = false,
      })
    end,
  },
}
