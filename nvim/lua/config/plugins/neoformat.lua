return {
  {
    "sbdchd/neoformat",
    config = function()
      vim.api.nvim_create_autocmd("BufWritePre", {
        group = fmt_group,
        pattern = "*",
        callback = function()
          local client = vim.lsp.get_active_clients({ bufnr = 0 })[1]

          if client and client.supports_method("textDocument/formatting") then
            vim.lsp.buf.format()
          else
            -- Fall back to Neoformat
            vim.cmd("Neoformat")
          end
        end,
      })
    end,
    init = function()
      vim.g.neoformat_javascript_prettier = {
        exe = "./node_modules/.bin/prettier",
        args = { "--single-quote", "--stdin", "--stdin-filepath", "%:p" },
        stdin = 1,
      }
      vim.g.neoformat_typescript_prettier = vim.g.neoformat_javascript_prettier
      vim.g.neoformat_typescriptreact_prettier = vim.g.neoformat_javascript_prettier
      vim.g.neoformat_css_prettier = vim.g.neoformat_javascript_prettier
      vim.g.neoformat_scss_prettier = vim.g.neoformat_javascript_prettier

      vim.g.neoformat_sql_pg_format = {
        exe = "pg_format",
        args = { "-B", "-s", "2", "-w", "80", "-" },
        stdin = 1,
      }

      vim.g.neoformat_ocaml_ocamlformat = {
        exe = "ocamlformat",
        args = { "--name", '"%:p"', "-" },
        no_append = 1,
        stdin = 1,
      }
      vim.g.neoformat_enabled_ocaml = { "ocamlformat" }
    end,
  },
}
