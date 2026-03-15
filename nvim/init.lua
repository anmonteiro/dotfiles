if not vim.g.python3_host_prog or vim.g.python3_host_prog == "" then
  local python3_candidates = {
    vim.fn.expand("$HOME/.nix-profile/bin/python3"),
    "/nix/var/nix/profiles/default/bin/python3",
    "/run/current-system/sw/bin/python3",
    vim.fn.exepath("python3"),
  }

  for _, python3_host in ipairs(python3_candidates) do
    if python3_host ~= "" and vim.fn.executable(python3_host) == 1 then
      vim.g.python3_host_prog = python3_host
      break
    end
  end
end

require("config.lazy")
