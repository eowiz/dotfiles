vim.g.loaded_matchparen = 1
vim.g.mapleader = ' '

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
   "folke/which-key.nvim",
   { "folke/neoconf.nvim", cmd = "Neoconf" },
   "folke/neodev.nvim",
   -- { "ellisonleao/gruvbox.nvim", priority = 1000 },
   -- { "joshdick/onedark.vim", priority = 1000 },
   -- { "tomasr/molokai", priority = 1000 },
   -- { "catppuccin/nvim",
   --   name = "catppuccin",
   --   priority = 1000,
   --   config = function()
   --     require("catppuccin").setup({
   --       flavour = "frappe"
   --     })
   --   end
   -- },
   { "savq/melange-nvim", priority = 1000 },
   { "xiyaowong/transparent.nvim", run = ":TransparentEnable" },
   { "nvim-lualine/lualine.nvim", dependencies = { "nvim-tree/nvim-web-devicons" } },
   { "akinsho/bufferline.nvim", version = "*", dependencies = "nvim-tree/nvim-web-devicons" },
   {
     "dinhhuy258/git.nvim",
     config = function()
       require("git").setup()
     end
   },
   { "andymass/vim-matchup" },
   {
     "nvim-telescope/telescope-file-browser.nvim",
     dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
     keys = {
       { "<leader>fb", ":Telescope file_browser path=%:p:h select_buffer=true<CR>", desc = "FileBrowser" },
     },
     config = function()
       require("telescope").load_extension("file_browser")
     end,
   },
   { 
     "nvim-telescope/telescope-ghq.nvim",
     dependencies = { "nvim-telescope/telescope.nvim" }, 
     keys = {
       { "<C-g>", ":Telescope ghq list<CR>", desc = "ghq list" },
     },
     config = function()
       require("telescope").load_extension("ghq")
     end,
   },
   { "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" },
   "vim-denops/denops.vim",
   "vim-skk/skkeleton",
   { "nvim-tree/nvim-tree.lua", dependencies = { "nvim-tree/nvim-web-devicons" } },
   { 'numToStr/Comment.nvim', config = function() require('Comment').setup() end },
   { 'windwp/nvim-autopairs', event = "InsertEnter", opts = {} },
   { 'akinsho/toggleterm.nvim', config = function() require("toggleterm").setup() end },
   { "lukas-reineke/indent-blankline.nvim" },
   { 'folke/noice.nvim', 
     event = "VeryLazy",
     dependencies = { "MunifTanjim/nui.nvim", "rcarriga/nvim-notify" },
     config = function()
       require("noice").setup({
       })
       require("notify").setup({
        background_colour = "#000000",
       })
     end,
   },
})
 
vim.scriptencoding = "utf-8"
vim.opt.encoding = "utf-8"
vim.opt.fileencoding = "utf-8"
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.cursorline = true
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.termguicolors = true

vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.wrapscan = true

vim.opt.clipboard:append({ unnamedeplus = true })

-- Theme
-- vim.cmd.color.scheme catppuccin
vim.cmd.colorscheme 'melange'
vim.opt.fillchars = {
  vert = ' ',
}
-- 
-- -- Font
-- -- vim.opt.guifont = { "HackGen Console NF", ":h21" }
-- -- vim.opt.guifontwide = { "HackGen Console NF", ":h21" }
-- 
-- Status Line
require("lualine").setup()

vim.wo.number = true
vim.wo.relativenumber = true

-- Key config
vim.keymap.set("i", "<C-p>", "<Up>")
vim.keymap.set("i", "<C-n>", "<Down>")
vim.keymap.set("i", "<C-f>", "<Right>")
vim.keymap.set("i", "<C-b>", "<Left>")
vim.keymap.set("i", "<C-a>", "<Home>")
vim.keymap.set("i", "<C-e>", "<End>")

vim.keymap.set("i", "<C-h>", "<BS>")
vim.keymap.set("i", "<C-d>", "<Del>")
vim.keymap.set("i", "<C-k>", "<Esc>lDa")
vim.keymap.set("i", "<C-y>", "<Esc>pa")

vim.keymap.set("i", "<C-v>", "<PageDown>")
vim.keymap.set("i", "<A-v>", "<PageUp>")

vim.keymap.set("i", "<C-Space>", "<Esc>lv")

vim.keymap.set("n", "<C-l>", "zz")
vim.keymap.set("i", "<C-l>", "<Esc>zza")

vim.keymap.set("n", "s", "<NOP>")
vim.keymap.set("n", "sj", "<C-w>j")
vim.keymap.set("n", "sk", "<C-w>k")
vim.keymap.set("n", "sl", "<C-w>l")
vim.keymap.set("n", "sh", "<C-w>h")

vim.keymap.set("n", "<C-w>-", "<C-w>s")
vim.keymap.set("n", "<C-w>\\", "<C-w>v")

vim.api.nvim_create_autocmd("BufEnter", {
  pattern = "*",
  command = "set fo-=c fo-=r fo-=o",
})

vim.api.nvim_create_autocmd({ "BufReadPost" }, {
  pattern = { "*" },
  callback = function()
    vim.api.nvim_exec('silent! normal! g`"zv', false)
  end,
})

-- Tab
require("bufferline").setup()
-- タブを作成、削除
vim.keymap.set('n', '<leader>to', ':tabnew<CR>')
vim.keymap.set('n', '<leader>tx', ':tabclose<CR>')
vim.keymap.set('n', '<leader>tn', ':tabn<CR>')
vim.keymap.set('n', '<leader>tp', ':tabp<CR>')

-- SKK
vim.cmd([[
  call skkeleton#config({ 'globalJisyo': '~/.skk-jisyo' })
]])

vim.keymap.set('i', '<C-j>', '<Plug>(skkeleton-enable)')
vim.keymap.set('c', '<C-j>', '<Plug>(skkeleton-enable)')

-- telecope

-- nvim-tree
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- set termguicolors to enable highlight groups
vim.opt.termguicolors = true

-- empty setup using defaults
require("nvim-tree").setup()

vim.api.nvim_set_keymap("n", "<leader>e", ":NvimTreeToggle<CR>", { silent = true })

-- lazygit
local Terminal = require("toggleterm.terminal").Terminal
local lazygit = Terminal:new({
  cmd = "lazygit",
  direction = "float",
  hidden = true,
})
 
function _lazygit_toggle()
  lazygit:toggle()
end

vim.keymap.set("n", "<leader>lg", "<cmd>lua _lazygit_toggle()<CR>", { noremap = true, silent = true })

-- indent-blankline
vim.opt.list = true
vim.opt.listchars = { tab='>-', trail='⋅', nbsp='+' }
-- vim.opt.listchars:append "space:⋅"
-- vim.opt.listchars:append "eol:↴"

require("indent_blankline").setup {
    space_char_blankline = " ",
    show_current_context = true,
    show_current_context_start = true,
}
