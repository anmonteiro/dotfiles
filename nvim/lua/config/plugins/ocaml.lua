return {
  {
    'tjdevries/ocaml.nvim',
    config = function()
      require("ocaml").setup({
        install_rapper = true,
        install_mlx = true,
        setup_lspconfig = false,
        setup_conform = false,
      })
    end
  }
}

