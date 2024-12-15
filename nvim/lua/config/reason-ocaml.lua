-- autocmd FileType reason silent! call merlin#Register()

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "ocaml", "reason" },
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

-- Neoformat
vim.g.neoformat_ocaml_ocamlformat = {
  exe = "ocamlformat",
  args = { "--name", '"%:p"', "-" },
  no_append = 1,
  stdin = 1,
}

vim.g.neoformat_enabled_ocaml = { "ocamlformat" }

-- Use Python 3 for Merlin
-- https://github.com/ocaml/merlin/issues/1050
vim.g.merlin_python_version = 3

-- Set filetype for *.mli files
vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  pattern = "*.mli",
  callback = function()
    vim.bo.filetype = "ocaml.interface"
  end,
})
