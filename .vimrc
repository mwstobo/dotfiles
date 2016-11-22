" No vi compatibility
set nocompatible

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

" Hidden buffers
set hidden

" Search
set incsearch
set hlsearch

" Swapfile management
set swapfile
set dir=~/.vim/swap

" Show trailing whitespace
highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
match ExtraWhitespace /\s\+\%#\@<!$/

" Tab completion mode
set wildmode=longest,list,full

" Language specific settings
autocmd Filetype html setlocal shiftwidth=2 softtabstop=2
autocmd Filetype json setlocal shiftwidth=2 softtabstop=2
autocmd Filetype javascript setlocal shiftwidth=2 softtabstop=2
autocmd Filetype coffee setlocal shiftwidth=2 softtabstop=2
autocmd Filetype ruby setlocal shiftwidth=2 softtabstop=2
autocmd Filetype markdown setlocal colorcolumn=80 spell
autocmd Filetype text setlocal wrap linebreak spell spelllang=en_ca noexpandtab
autocmd Filetype text map j gj
autocmd Filetype text map k gk

" Supposed to be used with vim-plug
if filereadable(expand("~/.vimrc.plugins"))
    source ~/.vimrc.plugins
endif

if filereadable(expand("~/.vimrc.local"))
    source ~/.vimrc.local
endif
