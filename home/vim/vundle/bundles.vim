set nocompatible
filetype on
filetype off

set rtp+=~/.vim/bundle/vundle
call vundle#rc()

" Vundle manages Vundle
Bundle 'gmarik/vundle'

" My modules here
Bundle 'altercation/vim-colors-solarized'

filetype plugin indent on

" set solarized colorscheme
let g:solarized_termcolors=256
colorscheme solarized

