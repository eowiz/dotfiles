vim.g.loaded_matchparen = 1
vim.g.mapleader = ' '
vim.opt.autochdir = true

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.opt.termguicolors = true
vim.opt.guifont="HackGen Console NF:h17"

vim.cmd([[
  if has('persistent_undo')
    set undodir=~/.vim/undo
    set undofile
  endif
]])

vim.filetype.add({
  extension =  {
    mdx = "markdown.mdx",
  },
  filename = {},
  pattern = {},
})

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  "folke/which-key.nvim",
  { "folke/neoconf.nvim", cmd = "Neoconf" },
  "folke/neodev.nvim",
  {
    "sainnhe/gruvbox-material",
    priority = 1000,
    config = function()
      vim.opt.background = "dark"
      vim.g.gruvbox_material_background = "medium"
      vim.cmd.colorscheme "gruvbox-material"
    end,
  },
  -- {
  --   "ellisonleao/gruvbox.nvim",
  --   priority = 1000,
  --   config = function()
  --     vim.cmd.colorscheme "gruvbox"
  --     vim.opt.background = "dark"
  --   end,
  -- },
  -- { "joshdick/onedark.vim", priority = 1000 },
  -- { "tomasr/molokai", priority = 1000 },
  -- { "catppuccin/nvim",
  --   name = "catppuccin",
  --   priority = 1000,
  --   config = function()
  --     require("catppuccin").setup({
  --       flavour = "frappe"
  --     })
  --     vim.cmd.colorscheme 'catppuccin'
  --   end
  -- },
  -- {
  --   "savq/melange-nvim", priority = 1000,
  --   config = function()
  --     vim.cmd.colorscheme 'melange'
  --   end,
  -- },
  -- {
  --   "folke/tokyonight.nvim",
  --   lazy = false,
  --   priority = 1000,
  --   config = function()
  --     require("tokyonight").setup({
  --       transparent = true,
  --       styles = {
  --         sidebars = "transparent",
  --         floats = "transparent"
  --       }
  --     })
  --     vim.cmd.colorscheme 'tokyonight'
  --   end,
  -- },
  -- {
  --   "rebelot/kanagawa.nvim",
  --   priority = 1000,
  --   config = function()
  --     require("kanagawa").setup({
  --       compile = true,
  --       theme = "dragon",
  --       background = {
  --         dark = "dragon",
  --       },
  --     })
  --     vim.cmd.colorscheme 'kanagawa'
  --   end,
  -- },
  { "xiyaowong/transparent.nvim", run = ":TransparentEnable" },
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("lualine").setup({
        options = {
          theme = 'gruvbox-material'
        }
      })
    end,
  },
  { "akinsho/bufferline.nvim",
    version = "*",
    dependencies = "nvim-tree/nvim-web-devicons",
    config = function()
      require("bufferline").setup()
    end
  },
  {
    "petertriho/nvim-scrollbar",
    config = function()
      -- local colors = require("gruvbox.colors").setup()
      require("scrollbar").setup({
      --  handle = {
      --    color = colors.bg_highlight,
      --  },
      --  marks = {
      --    Search = { color = colors.orange },
      --    Error = { color = colors.error },
      --    Warn = { color = colors.warning },
      --    Info = { color = colors.info },
      --    Hint = { color = colors.hint },
      --    Misc = { color = colors.purple },
      --  },
      })
    end,
  },
  { "andymass/vim-matchup" },
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "neovim/nvim-lspconfig",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-nvim-lsp-document-symbol",
      "hrsh7th/cmp-buffer", 
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-cmdline",
      "hrsh7th/nvim-cmp",
      "petertriho/cmp-git",
      "nvim-lua/plenary.nvim",
      "onsails/lspkind.nvim",
    },
    config = function()
      local cmp = require("cmp")

      cmp.setup({
        window = {
          completion = cmp.config.window.bordered({
            border = "single"
          }),
          documentation = cmp.config.window.bordered({
            border = "single"
          }),
        },
        snippets = {
          expand = function(args)
            vim.fn["vsnip#anonymous"](args.body)
          end,
        },
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "nvim_lsp_signature_help" },
          { name = "buffer" },
          { name = "path" },
          { name = "git" },
        }),
        mapping = cmp.mapping.preset.insert({
          ["<A-p>"] = cmp.mapping.select_prev_item(),
          ["<S-Tab>"] = cmp.mapping.select_prev_item(),
          ["<A-n>"] = cmp.mapping.select_next_item(),
          ["<Tab>"] = cmp.mapping.select_next_item(),
          ["<A-l>"] = cmp.mapping.complete(),
          ["<A-c>"] = cmp.mapping.abort(),
          ["<CR>"] = cmp.mapping.confirm({ select = true })
        }),
        formatting = {
          format = require("lspkind").cmp_format({
            mode = "symbol"
          })
        },
      })
      cmp.setup.cmdline(":", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = 'path' }
        }, {
          { name = 'cmdline' }
        })
      })
      cmp.setup.cmdline("/", {
        sources = cmp.config.sources({
          { name = "nvim_lsp_document_symbol" }
        }, {
          { name = "buffer" }
        }),
        mapping = cmp.mapping.preset.cmdline(),
      })
    end,
  },
  {
    "hrsh7th/vim-vsnip",
    dependencies = { "hrsh7th/vim-vsnip-integ" }
  },
  {
    "stevearc/dressing.nvim",
    config = function()
      require("dressing").setup({
        input = {
          border = "single"
        },
        builtin = {
          border = "single"
        },
      })
    end
  },
  {
    "nvim-telescope/telescope.nvim",
    lazy = false,
    keys = {
      { "<leader>fb", ":Telescope buffers<CR>", silent = true },
      { "<leader>ff", ":Telescope fd<CR>", silent = true },
    },
    config = function()
      require('telescope').setup({
        defaults = {
          borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
          layout_strategy = "vertical",
          layout_config = {
            width = 0.9,
            preview_cutoff = 0,
          },
        },
      })
      vim.cmd "autocmd User TelescopePreviewerLoaded setlocal number"
    end,
  },
  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
    keys = {
      { "<leader>fs", ":Telescope file_browser path=%:p:h select_buffer=true<CR>", desc = "FileBrowser", silent = true },
    },
    config = function()
      require("telescope").load_extension("file_browser")
    end,
  },
  {
    "nvim-telescope/telescope-ghq.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" }, 
    keys = {
      { "<C-g>", ":Telescope ghq list<CR>", desc = "ghq list", silent = true, noremap = true },
    },
    config = function()
      require("telescope").load_extension("ghq")
    end,
  },
  {
    "nvim-telescope/telescope-live-grep-args.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" },
    keys = {
      { "<leader>fg", ":Telescope live_grep_args<CR>", silent = true }
    },
    config = function()
      require("telescope").load_extension("live_grep_args")
    end,
  },
  {
    "nvim-telescope/telescope-frecency.nvim",
    dependencies = { "nvim-telescope/telescope.nvim", "kkharji/sqlite.lua" },
    keys = {
      { "<leader>fr", ":Telescope frecency<CR>", silent = true },
      { "<leader>fc", ":lua require('telescope').extensions.frecency.frecency({ workspace = 'CWD' })<CR>", silent = true },
    },
    config = function()
      require("telescope").load_extension("frecency")
    end,
  },
  {
    "delphinus/telescope-memo.nvim",
    dependencies = { "nvim-telescope/telescope.nvim", "glidenote/memolist.vim" },
    keys = {
      { "<leader>mn", ":MemoNew<CR>", silent = true },
      { "<leader>ml", ":Telescope memo list<CR>", silent = true },
      { "<leader>mg", ":Telescope memo live_grep<CR>", silent = true },
    },
    config = function()
      require("telescope").load_extension("memo")
    end,
  },
  {
    "gbrlsnchs/telescope-lsp-handlers.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" },
    config = function()
      require("telescope").load_extension("lsp_handlers")
    end,
  },
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" },
    build = "make",
    config = function()
      require("telescope").load_extension("fzf")
    end,
  },
  -- {
  --   "Allianaab2m/telescope-kensaku.nvim",
  --   dependencies = { "nvim-telescope/telescope.nvim", "lambdalisue/kensaku.vim", "vim-denops/denops.vim" },
  --   keys = { { "<leader>fk", ":Telescope kensaku<CR>", silent = true } },
  --   config = function()
  --     require("telescope").load_extension("kensaku")
  --   end,
  -- },
  { "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" },
  {
    "RRethy/nvim-treesitter-endwise",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = {
          "lua",
          "typescript",
          "tsx",
          "java",
          "prisma",
        },
        highlight = {
          enable = true,
        },
        endwise = {
          enable = true
        },
      })
    end,
  },
  { "RRethy/vim-illuminate" },
  {
    "nvim-zh/colorful-winsep.nvim",
    config = function()
      require("colorful-winsep").setup()
    end,
    event = { "WinNew" },
    symbols = { "━", "┃", "┏", "┓", "┗", "┛" },
    no_exec_files = { "packer", "TelescopePrompt", "mason", "CompetiTest", "NvimTree" },
  },
  {
    "levouh/tint.nvim",
    config = function()
      require("tint").setup({
        tint = -45,
        saturation = 0.6,
      })
    end,
  },
  { 
    "windwp/nvim-ts-autotag",
    config = function()
      require('nvim-ts-autotag').setup()
    end,
  },
  "vim-denops/denops.vim",
  "vim-skk/skkeleton",
  {
    "lambdalisue/fern.vim",
    keys = {
      { "<leader>e", ":Fern . -drawer<CR>", silent = true },
      { "<leader>E", ":Fern . -drawer -reveal=%<CR>", silent = true },
    },
    config = function()
      vim.cmd([[
        let g:fern#default_hidden = 1
      ]])
    end,
  },
  {
    "lambdalisue/nerdfont.vim",
  },
  {
    "lambdalisue/glyph-palette.vim",
    config = function()
      vim.cmd([[
        augroup my-glyph-palette
        autocmd! *
        autocmd FileType fern call glyph_palette#apply()
        autocmd FileType nerdtree,startify call glyph_palette#apply()
        augroup END
      ]])
    end,
  },
  {
    "lambdalisue/fern-renderer-nerdfont.vim",
    dependencies = { "lambdalisue/fern.vim" },
    config = function()
      vim.g["fern#renderer"] = "nerdfont"
      vim.g["fern#renderer#nerdfont#indent_markers"] = "1"
    end,
  },
  { "lambdalisue/fern-git-status.vim" },
  {
    "yuki-yano/fern-preview.vim",
    dependencies = { "lambdalisue/fern.vim" },
    config = function()
      vim.cmd([[
        function! s:fern_settings() abort
          nmap <silent> <buffer> p     <Plug>(fern-action-preview:toggle)
          nmap <silent> <buffer> <C-p> <Plug>(fern-action-preview:auto:toggle)
          nmap <silent> <buffer> <C-f> <Plug>(fern-action-preview:scroll:down:half)
          nmap <silent> <buffer> <C-b> <Plug>(fern-action-preview:scroll:up:half)
          nmap <silent> <buffer> q     <Plug>(fern-action-preview:close)
        endfunction

        augroup fern-settings
          autocmd!
          autocmd FileType fern call s:fern_settings()
        augroup END
      ]])
    end,
  },
  -- {
  --   "nvim-tree/nvim-tree.lua",
  --   dependencies = { "nvim-tree/nvim-web-devicons" },
  --   lazy = false,
  --   keys = {
  --     { "<leader>ee", ":NvimTreeToggle<CR>", silent = true },
  --     { "<leader>ef", ":NvimTreeFocus<CR>", silent = true }
  --   },
  --   config = function()
  --     require("nvim-tree").setup({
  --       sync_root_with_cwd = true,
  --       respect_buf_cwd = true,
  --       update_focused_file = {
  --         enable = true,
  --         update_root = true,
  --       },
  --       renderer = {
  --         group_empty = true,
  --         indent_markers = {
  --           enable = true,
  --           icons = {
  --             corner = '└ ',
  --             edge   = '│ ',
  --             item   = '│ ',
  --             none   = '  ',
  --           },
  --         },
  --       },
  --       on_attach = function(bufnr)
  --         local api = require("nvim-tree.api")

  --         local function opts(desc)
  --           return { desc = "nvim-tree: " .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
  --         end

  --         api.config.mappings.default_on_attach(bufnr)

  --         vim.keymap.set("n", "<C-t>", api.tree.change_root_to_parent, opts("Up"))
  --         vim.keymap.set("n", "?", api.tree.toggle_help, opts("Help"))
  --       end,
  --     })
  --     vim.cmd "hi NvimTreeWinSeparator guifg=gray"
  --   end,
  -- },
  { 'numToStr/Comment.nvim', 
    config = function()
      require('Comment').setup()
    end,
  },
  { 'windwp/nvim-autopairs', event = "InsertEnter", opts = {} },
  {
    "akinsho/toggleterm.nvim",
    config = function()
      require("toggleterm").setup()
    end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require("indent_blankline").setup({
        space_char_blankline = " ",
        show_current_context = true,
        show_current_context_start = true,
      })
    end,
  },
  {
    "folke/noice.nvim", 
    event = "VeryLazy",
    dependencies = { "MunifTanjim/nui.nvim", "rcarriga/nvim-notify" },
    config = function()
      require("noice").setup({
        views = {
          cmdline_popup = {
            border = {
              style = "single"
            },
          },
          popupmenu = {
            border = {
              style = "single"
            },
          },
        },
      })
      require("notify").setup({
        background_colour = "#000000",
        on_open = function(win)
          vim.api.nvim_win_set_config(win, { border = "single" })
        end,
      })
    end,
  },
  {
    "stevearc/aerial.nvim",
    keys = { { "<leader>a", ":AerialToggle!<CR>", silent = true } },
    config = function()
      require("aerial").setup({
        on_attach = function(bufnr)
          vim.keymap.set("n", "{", ":AerialPrev<CR>", { buffer = bufnr, silent = true })
          vim.keymap.set("n", "}", ":AerialNext<CR>", { buffer = bufnr, silent = true })
        end
      })
    end,
  },
  {
    "monaqa/dial.nvim",
    keys = {
      { "<C-a>", "<Plug>(dial-increment)", noremap = true },
      { "<C-x>", "<Plug>(dial-decrement)", noremap = true },
      { "g<C-a>", "g<Plug>(dial-increment)", noremap = true },
      { "g<C-x>", "g<Plug>(dial-decrement)", noremap = true },
    },
  },
  {
    "dinhhuy258/git.nvim",
    config = function()
      require("git").setup()
    end
  },
  {
    "lambdalisue/gin.vim",
    dependencies = { "vim-denops/denops.vim" },
  },
  { "sindrets/diffview.nvim"},
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require("gitsigns").setup()
    end,
  },
  {
    "rhysd/git-messenger.vim",
    keys = { { "<leader>gm", ":GitMessenger" } },
    config = function()
      vim.g.git_messenger_floating_win_opts = { border = "single" }
    end,
  },
  {
    "utilyre/barbecue.nvim",
    dependencies = { "SmiteshP/nvim-navic", "nvim-tree/nvim-web-devicons" },
    config = function()
      require("barbecue").setup()
    end,
  },
  {
    "tkmpypy/chowcho.nvim",
    keys = { { "<C-w>w", ":Chowcho<CR>", silent = true } },
    config = function()
      require("chowcho").setup({
        icon_enabled = true,
        border_style = "rounded",
      })
    end,
  },
  {
    "yuki-yano/fuzzy-motion.vim",
    lazy = false,
    dependencies = { "vim-denops/denops.vim" },
    keys = { { "<leader><leader>", ":FuzzyMotion<CR>", silent = true } },
  },
  {
    "kat0h/bufpreview.vim",
    dependencies = { "vim-denops/denops.vim" },
  },
  {
    "VidocqH/lsp-lens.nvim",
    config = function()
      require("lsp-lens").setup({})
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = {
      "williamboman/mason.nvim",
      "neovim/nvim-lspconfig",
      "hrsh7th/cmp-nvim-lsp"
    },
    config = function()
      require("mason").setup({
        ui = {
          border = "single"
        }
      })
      require("mason-lspconfig").setup({
        ensure_installed = {
          "lua_ls",
          "tsserver",
          "denols",
          "jsonls",
          "rust_analyzer",
        },
        automatic_installation = true,
      })

      local handlers = {
        -- ["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, { virtual_text = false }),
        ["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "single" }),
      }

      -- require("lspconfig").lua_ls.setup({})
      local lspconfig = require("lspconfig");
      lspconfig.jdtls.setup({
        cmd = { "jdtls" },
        root_dir = function(fname)
          return lspconfig.util.root_pattern("pom.xml", "gradle.build", ".git")(fname) or vim.fn.getcwd()
        end,
        handlers = handlers,
      })

      -- TypeScript
      lspconfig.denols.setup({
        root_dir = lspconfig.util.root_pattern("deno.json"),
        init_options = {
          lint = true,
          unstable = true,
          suggest = {
            imports = {
              hosts = {
                ["https://deno.land"] = true,
                ["https://cdn.nest.land"] = true,
                ["https://crux.land"] = true,
              },
            },
          },
        },
      })
      lspconfig.tsserver.setup({
        handlers = handlers,
        root_dir = lspconfig.util.root_pattern("package.json"),
      })

      vim.api.nvim_create_autocmd("LspAttach", {
        callback = function(ev)
          local opts = { buffer = ev.ops }
          vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
          vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
          vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
          vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
          vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, opts)
          vim.keymap.set("n", "<leader>D", vim.lsp.buf.type_definition, opts)
          vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)
          vim.keymap.set({ "n", "v" }, "<leader>ca", vim.lsp.buf.code_action, opts)
          vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
          vim.keymap.set("n", "<leader>f", function()
            vim.lsp.buf.format { async = true }
          end, opts)

          vim.api.nvim_create_autocmd("BufWritePre", {
            buffer = buffer,
            callback = function()
              vim.lsp.buf.format { async = false }
            end
          })
        end
      })
    end,
  },
  {
    "jay-babu/mason-null-ls.nvim",
    dependencies = { "williamboman/mason.nvim", "jose-elias-alvarez/null-ls.nvim" },
    config = function()
      require("mason-null-ls").setup({
        automatic_setup = true,
        handlers = {},
      })
    end,
  },
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    keys = {
      { "<leader>xx", ":TroubleToggle<CR>", silent = true },
      { "gR", ":TroubleToggle lsp_references<CR>", silent = true },
    },
  },
  {
    "ahmedkhalf/project.nvim",
    config = function()
      require("project_nvim").setup({})
      require("telescope").load_extension("projects")
    end,
  },
  -- {
  --   "nvimdev/lspsaga.nvim",
  --   dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-tree/nvim-web-devicons" },
  --   config = function()
  --     require("lspsaga").setup({
  --       lightbulb = {
  --         enable = false
  --       }
  --     })
  --   end,
  -- },
  -- {
  --   "pmizio/typescript-tools.nvim",
  --   dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
  --   config = function()
  --     require("typescript-tools").setup({})
  --   end,
  -- },
})
 
vim.scriptencoding    = "utf-8"
vim.opt.encoding      = "utf-8"
vim.opt.fileencoding  = "utf-8"
vim.opt.autoindent    = true
vim.opt.smartindent   = true
vim.opt.cursorline    = true
vim.opt.tabstop       = 2
vim.opt.softtabstop   = 2
vim.opt.shiftwidth    = 2
vim.opt.expandtab     = true
vim.opt.termguicolors = true

vim.opt.ignorecase = true
vim.opt.smartcase  = true
vim.opt.wrapscan   = true

vim.opt.clipboard:append({ unnamedeplus = true })

vim.opt.list = true
vim.opt.listchars = { tab = '>-', trail = '⋅', nbsp = '+' }
-- vim.opt.listchars:append "space:⋅"
-- vim.opt.listchars:append "eol:↴"

-- Theme
-- vim.cmd.color.scheme catppuccin

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

-- タブを作成、削除
vim.keymap.set('n', '<leader>to', ':tabnew<CR>')
vim.keymap.set('n', '<leader>tx', ':tabclose<CR>')
vim.keymap.set('n', '<leader>tn', ':tabn<CR>')
vim.keymap.set('n', '<leader>tp', ':tabp<CR>')

-- SKK
vim.cmd([[
  call skkeleton#config({ 'globalJisyo': '~/SKK-JISYO.L' })
]])

vim.keymap.set('i', '<C-j>', '<Plug>(skkeleton-enable)')
vim.keymap.set('c', '<C-j>', '<Plug>(skkeleton-enable)')

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

