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

if filereadable(".vimrc.local")
    source .vimrc.local
endif
