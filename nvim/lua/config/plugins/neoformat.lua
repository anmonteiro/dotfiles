return {
  {
    "sbdchd/neoformat",
    config = function()
      local fmt_group = vim.api.nvim_create_augroup("neoformat_fallback", { clear = true })

      local function has_lsp_formatter(bufnr)
        for _, client in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
          if client.supports_method("textDocument/formatting") then return true end
        end
        return false
      end

      vim.api.nvim_create_autocmd("BufWritePre", {
        group = fmt_group,
        pattern = "*",
        callback = function(args)
          if not vim.bo[args.buf].modifiable or vim.bo[args.buf].buftype ~= "" then return end

          if has_lsp_formatter(args.buf) then
            vim.lsp.buf.format({ bufnr = args.buf, async = false })
          else
            vim.api.nvim_buf_call(args.buf, function()
              vim.cmd("silent Neoformat")
            end)
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
