local M = {}

local function configure_global_diagnostics()
  local opts = { noremap = true, silent = true }

  vim.keymap.set("n", "<space>e", vim.diagnostic.open_float, opts)
  vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
  vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)
  vim.keymap.set("n", "<space>q", vim.diagnostic.setloclist, opts)
end

local function configure_lsp_keymaps(bufnr)
  local opts = { noremap = true, silent = true, buffer = bufnr }

  vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
  vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
  vim.keymap.set("n", "<localleader>t", vim.lsp.buf.hover, opts)
  vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
  vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, opts)
  vim.keymap.set("n", "<space>wa", vim.lsp.buf.add_workspace_folder, opts)
  vim.keymap.set("n", "<space>wr", vim.lsp.buf.remove_workspace_folder, opts)
  vim.keymap.set("n", "<space>wl", function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, opts)
  vim.keymap.set("n", "<space>D", vim.lsp.buf.type_definition, opts)
  vim.keymap.set("n", "<space>rn", vim.lsp.buf.rename, opts)
  vim.keymap.set("n", "<space>ca", vim.lsp.buf.code_action, opts)
  vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
  vim.keymap.set("n", "<space>f", function()
    vim.lsp.buf.format({ async = true })
  end, opts)
end

local function make_capabilities()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  local ok, blink = pcall(require, "blink.cmp")
  if ok then
    capabilities = blink.get_lsp_capabilities(capabilities)
  end
  return capabilities
end

local function enable_server(name, config)
  vim.lsp.config(name, config)
  vim.lsp.enable(name)
end

M.setup = function()
  local capabilities = make_capabilities()

  configure_global_diagnostics()

  local on_attach = function(_, bufnr)
    configure_lsp_keymaps(bufnr)
  end

  for _, server in ipairs({ "terraform_lsp" }) do
    enable_server(server, {
      on_attach = on_attach,
      capabilities = capabilities,
      flags = {
        debounce_text_changes = 150,
      },
    })
  end

  enable_server("ts_ls", {
    init_options = {
      filetypes = {
        "typescript",
        "typescriptreact",
        "typescript.tsx",
        "javascript",
        "javascriptreact",
        "javascript.jsx",
      },
      preferences = {
        includeInlayParameterNameHints = "none",
        includeInlayParameterNameHintsWhenArgumentMatchesName = false,
        includeInlayFunctionParameterTypeHints = false,
        includeInlayVariableTypeHints = true,
        includeInlayPropertyDeclarationTypeHints = true,
        includeInlayFunctionLikeReturnTypeHints = true,
        includeInlayEnumMemberValueHints = true,
      },
    },
    cmd = { "typescript-language-server", "--stdio" },
    on_attach = on_attach,
    capabilities = capabilities,
  })

  enable_server("ocamllsp", {
    on_attach = on_attach,
    capabilities = capabilities,
  })

  enable_server("rust_analyzer", {
    cmd = { "rust-analyzer" },
    settings = {
      ["rust-analyzer"] = {
        diagnostics = {
          enable = true,
          experimental = {
            enable = true,
          },
        },
      },
    },
    on_attach = on_attach,
    capabilities = capabilities,
  })

  enable_server("ccls", {
    init_options = {
      compilationDatabaseDirectory = "build",
      index = {
        threads = 0,
      },
      clang = {
        excludeArgs = { "-frounding-math" },
      },
    },
    capabilities = capabilities,
  })

  enable_server("ty", {
    on_attach = on_attach,
    capabilities = capabilities,
  })
end

return M
