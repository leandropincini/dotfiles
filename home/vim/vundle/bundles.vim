set nocompatible
filetype on
filetype off

set rtp+=~/.vim/bundle/vundle
call vundle#rc()

" Vundle manages Vundle
Bundle 'gmarik/vundle'

" My modules here
"Bundle 'altercation/vim-colors-solarized'
Bundle 'rizzatti/dash.vim'
Bundle 'dracula/vim'
Bundle 'tpope/vim-fugitive'
Bundle 'jmcantrell/vim-virtualenv'
Bundle 'airblade/vim-gitgutter'
Bundle 'vim-airline/vim-airline'
Bundle 'vim-airline/vim-airline-themes'
Bundle 'vim-scripts/SearchComplete'
Bundle 'ervandew/supertab'
Bundle 'vim-scripts/taglist.vim'
Bundle 'mattn/emmet-vim'
Bundle 'editorconfig/editorconfig-vim'

filetype plugin indent on

" set solarized colorscheme
"let g:solarized_termcolors=256

" settings for airline
let g:airline_powerline_fonts   = 1
let g:airline_theme             = 'dracula'
let g:airline_enable_branch     = 1
let g:airline_enable_syntastic  = 1

