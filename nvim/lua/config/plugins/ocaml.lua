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
  },
  {
    dir = vim.fn.stdpath("config") .. "/" .. "ocaml-plugin",
    ft = ocaml_ft,
    config = function()
      vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
        pattern = "jbuild",
        callback = function()
          vim.opt_local.filetype = "dune"
        end,
      })
    end,
  },
  {
    "tjdevries/ocaml.nvim",
    config = function()
      require("ocaml").setup({
        install_rapper = true,
        install_mlx = true,
        setup_lspconfig = false,
        setup_conform = false,
      })
    end,
  },
}
