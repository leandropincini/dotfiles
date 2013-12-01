set nocompatible
filetype on
filetype off

set rtp+=~/.vim/bundle/vundle
call vundle#rc()

" Vundle manages Vundle
Bundle 'gmarik/vundle'

" My modules here
Bundle 'altercation/vim-colors-solarized'
Bundle 'tpope/vim-fugitive'
Bundle 'jmcantrell/vim-virtualenv'
Bundle 'airblade/vim-gitgutter'
Bundle 'bling/vim-airline'

filetype plugin indent on

" set solarized colorscheme
let g:solarized_termcolors=256
colorscheme solarized

" settings for airline
let g:airline_theme             = 'powerlineish'
let g:airline_enable_branch     = 1
let g:airline_enable_syntastic  = 1

" vim-powerline symbols
let g:airline_left_sep          = '⮀'
let g:airline_left_alt_sep      = '⮁'
let g:airline_right_sep         = '⮂'
let g:airline_right_alt_sep     = '⮃'
let g:airline_branch_prefix     = '⭠'
let g:airline_readonly_symbol   = '⭤'
let g:airline_linecolumn_prefix = '⭡'

