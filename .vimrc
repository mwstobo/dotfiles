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

set textwidth=79
set colorcolumn=+1

" Language specific settings
autocmd Filetype html setlocal shiftwidth=2 softtabstop=2
autocmd Filetype json setlocal shiftwidth=2 softtabstop=2
autocmd Filetype javascript setlocal shiftwidth=2 softtabstop=2
autocmd Filetype ruby setlocal shiftwidth=2 softtabstop=2

func! WordProcessorMode()
  setlocal formatoptions=1
  setlocal noexpandtab
  map j gj
  map k gk
  setlocal spell spelllang=en_ca
  set complete+=s
  setlocal wrap
  setlocal linebreak
endfu
com! WP call WordProcessorMode()

" Supposed to be used with vim-plug
if filereadable(expand("~/.vimrc.plugins"))
    source ~/.vimrc.plugins
endif

if filereadable(expand("~/.vimrc.local"))
    source ~/.vimrc.local
endif
