local M = {}
local methods = vim.lsp.protocol.Methods

local function snacks_picker(method, fallback)
  return function()
    local ok, snacks = pcall(require, "snacks")
    local picker = ok and snacks.picker or _G.Snacks and _G.Snacks.picker
    local fn = picker and picker[method]

    if fn then
      fn()
      return
    end

    fallback()
  end
end

local function configure_global_diagnostics()
  local opts = { noremap = true, silent = true }

  vim.keymap.set("n", "<space>e", vim.diagnostic.open_float, opts)
  vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
  vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)
  vim.keymap.set("n", "<space>q", vim.diagnostic.setloclist, opts)
end

local function toggle_lsp_feature(module, filter, label)
  local enabled = module.is_enabled(filter)
  module.enable(not enabled, filter)
  vim.notify(string.format("%s %s", label, enabled and "disabled" or "enabled"), vim.log.levels.INFO, {
    title = "LSP",
  })
end

local function configure_lsp_toggle_commands(bufnr)
  if vim.b[bufnr].config_lsp_toggles_ready then
    return
  end

  vim.b[bufnr].config_lsp_toggles_ready = true

  vim.api.nvim_buf_create_user_command(bufnr, "LspInlayHintsToggle", function()
    toggle_lsp_feature(vim.lsp.inlay_hint, { bufnr = bufnr }, "Inlay hints")
  end, { desc = "Toggle LSP inlay hints" })

  vim.api.nvim_buf_create_user_command(bufnr, "LspCodeLensToggle", function()
    toggle_lsp_feature(vim.lsp.codelens, { bufnr = bufnr }, "Code lens")
  end, { desc = "Toggle LSP code lens" })

  vim.api.nvim_buf_create_user_command(bufnr, "LspDocumentColorToggle", function()
    toggle_lsp_feature(vim.lsp.document_color, { bufnr = bufnr }, "Document colors")
  end, { desc = "Toggle LSP document colors" })
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

  vim.keymap.set("n", "gD", snacks_picker("lsp_declarations", vim.lsp.buf.declaration), opts)
  vim.keymap.set("n", "gd", snacks_picker("lsp_definitions", vim.lsp.buf.definition), opts)
  vim.keymap.set("n", "<localleader>t", vim.lsp.buf.hover, opts)
  vim.keymap.set("n", "gi", snacks_picker("lsp_implementations", vim.lsp.buf.implementation), opts)
  vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, opts)
  vim.keymap.set("n", "<space>wa", vim.lsp.buf.add_workspace_folder, opts)
  vim.keymap.set("n", "<space>wr", vim.lsp.buf.remove_workspace_folder, opts)
  vim.keymap.set("n", "<space>wl", function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, opts)
  vim.keymap.set("n", "<space>D", snacks_picker("lsp_type_definitions", vim.lsp.buf.type_definition), opts)
  vim.keymap.set("n", "<space>rn", vim.lsp.buf.rename, opts)
  vim.keymap.set("n", "<space>ca", vim.lsp.buf.code_action, opts)
  vim.keymap.set("n", "gr", snacks_picker("lsp_references", vim.lsp.buf.references), opts)
  vim.keymap.set("n", "<space>f", format_buffer, opts)
  vim.keymap.set(
    "n",
    "<space>ui",
    "<cmd>LspInlayHintsToggle<CR>",
    vim.tbl_extend("force", opts, {
      desc = "Toggle LSP inlay hints",
    })
  )
  vim.keymap.set(
    "n",
    "<space>uC",
    "<cmd>LspCodeLensToggle<CR>",
    vim.tbl_extend("force", opts, {
      desc = "Toggle LSP code lens",
    })
  )
  vim.keymap.set(
    "n",
    "<space>uc",
    "<cmd>LspDocumentColorToggle<CR>",
    vim.tbl_extend("force", opts, {
      desc = "Toggle LSP document colors",
    })
  )
end

local function enable_supported_lsp_features(client, bufnr)
  if client:supports_method(methods.textDocument_inlayHint, bufnr) then
    vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
  end

  if client:supports_method(methods.textDocument_codeLens, bufnr) then
    vim.lsp.codelens.enable(true, { bufnr = bufnr })
  end

  if client:supports_method(methods.textDocument_documentColor, bufnr) then
    vim.lsp.document_color.enable(true, { bufnr = bufnr })
  end

  if client:supports_method(methods.textDocument_linkedEditingRange, bufnr) then
    require("vim.lsp.linked_editing_range").enable(true, { client_id = client.id })
  end
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
    configure_lsp_toggle_commands(bufnr)
    configure_lsp_keymaps(bufnr)
    enable_supported_lsp_features(client, bufnr)
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
        includeInlayParameterNameHints = "literals",
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
    on_attach = on_attach,
    capabilities = capabilities,
  })

  enable_server("ty", {
    on_attach = on_attach,
    capabilities = capabilities,
  })
end

return M
