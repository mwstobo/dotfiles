" Hybrid line number mode
set number
set relativenumber

" Per file type indentation
filetype plugin indent on

" Syntax
syntax on

" Soft-tabs, 4 spaces
set expandtab
set shiftwidth=4
set softtabstop=4

" Always show status line
set laststatus=2
set ruler

" Hidden buffers
set hidden

" Search
set incsearch
set hlsearch

" Show trailing whitespace
highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
match ExtraWhitespace /\s\+\%#\@<!$/

" Tab completion mode
" set wildmode=longest,list,full
set wildmenu
set wildmode=longest:full,full

" Spelling
set spelllang=en_ca

" Backspace
set backspace=indent,eol,start

" Buffer switching
nnoremap <leader>b :buffers<CR>:buffer<Space>

" Language specific settings
autocmd Filetype html setlocal shiftwidth=2 softtabstop=2
autocmd Filetype less setlocal shiftwidth=2 softtabstop=2
autocmd Filetype javascript setlocal shiftwidth=2 softtabstop=2
autocmd Filetype json setlocal shiftwidth=2 softtabstop=2
autocmd Filetype python setlocal colorcolumn=80
autocmd Filetype ruby setlocal shiftwidth=2 softtabstop=2
autocmd Filetype scheme setlocal shiftwidth=2 softtabstop=2 colorcolumn=80
autocmd Filetype go setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd Filetype haskell setlocal colorcolumn=80
autocmd Filetype sml setlocal shiftwidth=3 softtabstop=3 colorcolumn=80
autocmd Filetype ocaml setlocal shiftwidth=2 softtabstop=2 colorcolumn=80
autocmd Filetype yaml setlocal shiftwidth=2 softtabstop=2
autocmd Filetype xml setlocal shiftwidth=2 softtabstop=2

autocmd Filetype text map j gj
autocmd Filetype text map k gk
autocmd Filetype text setlocal wrap linebreak noexpandtab
autocmd Filetype markdown map j gj
autocmd Filetype markdown map k gk
autocmd Filetype markdown setlocal wrap linebreak noexpandtab
autocmd Filetype plaintex map j gj
autocmd Filetype plaintex map k gk
autocmd Filetype plaintex setlocal wrap linebreak noexpandtab

let lua_file = stdpath("config") . "/lua/config.lua"
if filereadable(lua_file)
    lua require("config")
endif

" Supposed to be used with vim-plug
let plugin_file = stdpath("config") . "/plugins.vim"
if filereadable(plugin_file)
    execute "source" plugin_file
endif

let local_file = stdpath("config") . "/local.vim"
if filereadable(local_file)
    execute "source" local_file
endif

if filereadable("./.vimrc.dir")
    source ./.vimrc.dir
endif
