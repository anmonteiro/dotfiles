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
  local function format_buffer()
    local ok, conform = pcall(require, "conform")
    if ok then
      conform.format({
        bufnr = bufnr,
        async = true,
        lsp_format = vim.bo[bufnr].filetype == "nix" and "never" or "prefer",
      })
    else
      vim.lsp.buf.format({ bufnr = bufnr, async = true })
    end
  end

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
  vim.keymap.set("n", "<space>f", format_buffer, opts)
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

local function make_nixd_settings(nixpkgs_expr)
  return {
    nixd = {
      formatting = {
        command = { "nixfmt" },
      },
      nixpkgs = {
        expr = nixpkgs_expr or "import <nixpkgs> { }",
      },
    },
  }
end

local function nixd_nixpkgs_expr(root)
  if not root or not vim.uv.fs_stat(vim.fs.joinpath(root, "flake.nix")) then
    return "import <nixpkgs> { }"
  end

  local quoted_root = string.format("%q", root)
  return table.concat({
    "let",
    "  flake = builtins.getFlake " .. quoted_root .. ";",
    "  nixpkgs = if flake.inputs ? nixpkgs then flake.inputs.nixpkgs else null;",
    "in",
    "if flake ? legacyPackages then",
    "flake.legacyPackages.${builtins.currentSystem}",
    "else if nixpkgs != null && nixpkgs ? legacyPackages then",
    "nixpkgs.legacyPackages.${builtins.currentSystem}",
    "else",
    "import (if nixpkgs != null then nixpkgs else <nixpkgs>) { }",
  }, " ")
end

local function configure_nixd_workspace(client, bufnr)
  local bufname = vim.api.nvim_buf_get_name(bufnr)
  local root = vim.fs.root(bufname, { "flake.nix", ".git" })
  client.settings = make_nixd_settings(nixd_nixpkgs_expr(root))
  client:notify("workspace/didChangeConfiguration", { settings = client.settings })
end

M.setup = function()
  local capabilities = make_capabilities()

  configure_global_diagnostics()

  local on_attach = function(client, bufnr)
    configure_lsp_keymaps(bufnr)
    if client.name == "nixd_local" then
      configure_nixd_workspace(client, bufnr)
    end
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

  enable_server("nixd_local", {
    cmd = { "nixd" },
    filetypes = { "nix" },
    root_markers = { "flake.nix", ".git" },
    on_attach = on_attach,
    capabilities = capabilities,
    settings = make_nixd_settings(),
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
