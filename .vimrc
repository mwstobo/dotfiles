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

" Spelling
set spelllang=en_ca

" Language specific settings
autocmd Filetype coffee setlocal shiftwidth=2 softtabstop=2
autocmd Filetype html setlocal shiftwidth=2 softtabstop=2
autocmd Filetype less setlocal shiftwidth=2 softtabstop=2
autocmd Filetype javascript setlocal shiftwidth=2 softtabstop=2
autocmd Filetype json setlocal shiftwidth=2 softtabstop=2
autocmd Filetype python setlocal colorcolumn=80
autocmd Filetype ruby setlocal shiftwidth=2 softtabstop=2
autocmd Filetype scheme setlocal shiftwidth=2 softtabstop=2 colorcolumn=80
autocmd Filetype go setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd Filetype text map j gj
autocmd Filetype text map k gk
autocmd Filetype text setlocal wrap linebreak noexpandtab
autocmd Filetype markdown map j gj
autocmd Filetype markdown map k gk
autocmd Filetype markdown setlocal wrap linebreak noexpandtab
autocmd Filetype plaintex map j gj
autocmd Filetype plaintex map k gk
autocmd Filetype plaintex setlocal wrap linebreak noexpandtab

" Supposed to be used with vim-plug
if filereadable(expand("~/.vimrc.plugins"))
    source ~/.vimrc.plugins
endif

if filereadable(expand("~/.vimrc.local"))
    source ~/.vimrc.local
endif
