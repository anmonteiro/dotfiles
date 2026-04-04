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
    config = function()
      vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
        pattern = "jbuild",
        callback = function()
          vim.bo.filetype = "dune"
        end,
      })
    end,
  },
  {
    dir = vim.fn.stdpath("config") .. "/" .. "ocaml-plugin",
    ft = ocaml_ft,
    config = function()
      local group = vim.api.nvim_create_augroup("ocaml_plugin_keymaps", { clear = true })
      vim.api.nvim_create_autocmd("FileType", {
        group = group,
        pattern = ocaml_ft,
        callback = function(args)
          if vim.bo[args.buf].buftype ~= "" then return end
          vim.keymap.set("n", "<localleader>d", "<cmd>MerlinDocument<CR>", {
            buffer = args.buf,
            silent = true,
            noremap = true,
          })
        end,
      })

      -- Use Python 3 for Merlin
      -- https://github.com/ocaml/merlin/issues/1050
      vim.g.merlin_python_version = 3
    end,
  },
  {
    "tjdevries/ocaml.nvim",
    config = function()
      require("ocaml").setup({
        install_rapper = false,
        install_mlx = true,
        setup_lspconfig = false,
        setup_conform = false,
      })
    end,
  },
}
