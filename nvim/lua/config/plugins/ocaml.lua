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
      vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
        pattern = "jbuild",
        callback = function()
          vim.opt_local.filetype = "dune"
        end,
      })
    end,
  },
  {
    dir = vim.fn.stdpath("config") .. "/" .. "ocaml-plugin",
    ft = ocaml_ft,
    init = function()
      -- autocmd FileType reason silent! call merlin#Register()
      vim.api.nvim_create_autocmd("FileType", {
        pattern = ocaml_ft,
        callback = function()
          vim.api.nvim_set_keymap("n", "<localleader>d", ":MerlinDocument<CR>", { silent = true, noremap = true })
        end,
      })

      vim.g.reasonml_project_airline = 1
      vim.g.reasonml_syntastic_airline = 1
      vim.g.reasonml_clean_project_airline = 1
      vim.g.syntastic_reason = 1
      vim.g.syntastic_ocaml_checkers = { "merlin" }
      vim.g.syntastic_reason_checkers = { "merlin" }
      vim.g["airline#extensions#esy#enabled"] = 1
      vim.g["airline#extensions#reason#enabled"] = 1

      -- Use Python 3 for Merlin
      -- https://github.com/ocaml/merlin/issues/1050
      vim.g.merlin_python_version = 3
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
