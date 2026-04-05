return {
  {
    "stevearc/conform.nvim",
    config = function()
      local util = require("conform.util")

      require("conform").setup({
        formatters_by_ft = {
          javascript = { "prettier", lsp_format = "prefer" },
          typescript = { "prettier", lsp_format = "prefer" },
          typescriptreact = { "prettier", lsp_format = "prefer" },
          json = { "prettier", lsp_format = "prefer" },
          jsonc = { "prettier", lsp_format = "prefer" },
          css = { "prettier", lsp_format = "prefer" },
          scss = { "prettier", lsp_format = "prefer" },
          sql = { "pg_format", lsp_format = "prefer" },
          ocaml = { "ocamlformat", lsp_format = "prefer" },
          nix = { "nixfmt", lsp_format = "never" },
        },
        format_on_save = function(bufnr)
          if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then return nil end
          if not vim.bo[bufnr].modifiable or vim.bo[bufnr].buftype ~= "" then return nil end

          return {
            bufnr = bufnr,
            timeout_ms = 1000,
            quiet = true,
            lsp_format = vim.bo[bufnr].filetype == "nix" and "never" or "prefer",
          }
        end,
        formatters = {
          prettier = {
            inherit = true,
            command = util.from_node_modules("prettier"),
            prepend_args = { "--single-quote" },
          },
          pg_format = {
            inherit = false,
            command = "pg_format",
            args = { "-B", "-s", "2", "-w", "80", "-" },
          },
          ocamlformat = {
            inherit = false,
            command = "ocamlformat",
            args = { "--name", "$FILENAME", "-" },
          },
          nixfmt = {
            inherit = false,
            command = "nixfmt",
          },
        },
      })

      vim.api.nvim_create_user_command("FormatDisable", function(args)
        if args.bang then
          vim.b.disable_autoformat = true
        else
          vim.g.disable_autoformat = true
        end
      end, {
        bang = true,
        desc = "Disable autoformat-on-save (use ! for current buffer only)",
      })

      vim.api.nvim_create_user_command("FormatEnable", function(args)
        if args.bang then
          vim.b.disable_autoformat = false
        else
          vim.g.disable_autoformat = false
        end
      end, {
        bang = true,
        desc = "Enable autoformat-on-save (use ! for current buffer only)",
      })
    end,
  },
}
