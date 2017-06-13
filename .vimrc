set nocompatible              " be iMproved, required
filetype off                  " required

" set runtime path to include Vundle
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'flazz/vim-colorschemes'
Plugin 'bling/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'airblade/vim-gitgutter'
Plugin 'tpope/vim-fugitive'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'tpope/vim-markdown'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

set laststatus=2
let g:airline_section_z = '%{strftime("%H:%M")}'
let g:airline_setcion_y = 'BH: %{bufnr("%")}'
let g:airline_theme = 'solarized'
let g:airline_detect_modified=1
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#branch#empty_message = 'not git'
set ttimeoutlen=50
set t_Co=256

syntax enable
set background=dark
let g:solarized_termcolors=256
colorscheme solarized
set number
hi SpellBad ctermfg=255 ctermbg=124
hi SpellCap ctermfg=255 ctermbg=124
set cursorline
hi CursorLine cterm=NONE
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set ai
set si
set nowrap
set noswapfile
nnoremap  ; :
vnoremap <F5> "+y
map      <F6> :put +<cr>
noremap <Leader>s :update<CR>
inoremap <Up> <NOP>
inoremap <Down> <NOP>
inoremap <Left> <NOP>
inoremap <Right> <NOP>

augroup mkd
  autocmd BufRead *.mkd  set ai formatoptions=tcroqn2 comments=n:>
augroup END

autocmd FileType make setlocal noexpandtab
