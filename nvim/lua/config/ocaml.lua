local M = {}

function M.setup()
  vim.filetype.add({
    extension = {
      re = "reason",
      rei = "reason",
    },
    filename = {
      dune = "dune",
      jbuild = "dune",
    },
  })
end

return M
