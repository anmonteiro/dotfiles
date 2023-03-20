local telescope = require("telescope")
local actions = require("telescope.actions")
local nnoremap = require("utils").nnoremap

telescope.setup({
	defaults = {
		mappings = {
			i = {
				["<Esc>"] = actions.close, -- don't go into normal mode, just close
				["<C-j>"] = actions.move_selection_next, -- scroll the list with <c-j>
				["<C-k>"] = actions.move_selection_previous, -- scroll the list with <c-k>
				["<C-s>"] = actions.select_horizontal, -- open selection in new horizantal split
				["<C-v>"] = actions.select_vertical, -- open selection in new vertical split
				["<C-t>"] = actions.select_tab, -- open selection in new tab
				["<C-y>"] = actions.preview_scrolling_up,
				["<C-e>"] = actions.preview_scrolling_down,
			},
		},
		vimgrep_arguments = {
			"rg",
			"--color=never",
			"-H",
			"--no-heading",
			"-n",
			"--column",
			"-S",
			"--hidden",
			"--trim",
		},
		-- vimgrep_arguments = {
		-- 	"rg",
		-- 	"--color=never",
		-- 	"--no-heading",
		-- 	"--with-filename",
		-- 	"--line-number",
		-- 	"--column",
		-- 	"--smart-case",
		-- 	"--trim",
		-- },
		prompt_prefix = "   ",
		selection_caret = "  ",
		entry_prefix = "  ",
		initial_mode = "insert",
		selection_strategy = "reset",
		sorting_strategy = "ascending",
		layout_strategy = "horizontal",
		layout_config = {
			horizontal = {
				prompt_position = "top",
				preview_width = 0.55,
				results_width = 0.8,
			},
			vertical = {
				mirror = false,
			},
			width = 0.87,
			height = 0.80,
			preview_cutoff = 20,
		},
		file_sorter = require("telescope.sorters").get_fuzzy_file,
		file_ignore_patterns = { "node_modules", ".git", ".next" },
		generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
		path_display = { "truncate" },
		winblend = 0,
		border = {},
		-- borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
		color_devicons = true,
		use_less = true,
		set_env = { ["COLORTERM"] = "truecolor" }, -- default = nil,
		file_previewer = require("telescope.previewers").vim_buffer_cat.new,
		grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
		qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
		-- Developer configurations: Not meant for general override
		buffer_previewer_maker = require("telescope.previewers").buffer_previewer_maker,
	},
	-- pickers = {
	-- 	-- live_grep = {
	-- 	-- 	additional_args = function(opts)
	-- 	-- 		return { "--hidden" }
	-- 	-- 	end,
	-- 	-- },
	-- 	find_files = {
	-- 		find_command = { "fd", "-I", "--type", "f", "--strip-cwd-prefix" },
	-- 	},
	-- },
	extensions = {
		fzf = {
			fuzzy = true,
			override_generic_sorter = false,
			override_file_sorter = true,
			case_mode = "smart_case",
		},
	},
	-- file_previewer = require("telescope.previewers").vim_buffer_cat.new,
	-- grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
	-- qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
	-- Developer configurations: Not meant for general override
	-- buffer_previewer_maker = require("telescope.previewers").buffer_previewer_maker,
})

-- mappings
nnoremap("<leader>e", "<cmd>Telescope find_files hidden=true<cr>")
nnoremap("<leader>p", "<cmd>Telescope live_grep hidden=true<cr>")
nnoremap("<leader>b", "<cmd>Telescope buffers<cr>")
nnoremap("<leader>o", "<cmd>Telescope oldfiles<cr>")
nnoremap("<leader>tt", "<cmd>Telescope<cr>")
nnoremap("<leader>tn", "<cmd>Telescope node_modules list<cr>")
nnoremap("<leader>tr", "<cmd>lua require('telescope').extensions.live_grep_raw.live_grep_raw()<cr>")
nnoremap("<leader>te", "<cmd>Telescope find_files hidden=true <cr>")
nnoremap("<leader>tp", "<cmd>Telescope live_grep hidden=true <cr>")
nnoremap("<leader>th", "<cmd>Telescope help_tags<cr>")
