return {
  {
    "tpope/vim-dadbod",
    init = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern = "sql",
        command = [[setlocal omnifunc=vim_dadbod_completion#omni]],
      })
      -- Source is automatically added, you just need to include it in the chain complete list
      vim.g.completion_chain_complete_list = {
        sql = {
          { complete_items = { "vim-dadbod-completion" } },
        },
      }
    end,
  },
  "kristijanhusak/vim-dadbod-ui",
  "kristijanhusak/vim-dadbod-completion",
}
