set nocompatible              " be iMproved, required
filetype off                  " required

" set runtime path to include Vundle and initialize

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'flazz/vim-colorschemes'
Plugin 'bling/vim-airline'
Plugin 'airblade/vim-gitgutter'
Plugin 'tpope/vim-fugitive'
Plugin 'ctrlpvim/ctrlp.vim'
" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
" Plugin 'tpope/vim-fugitive'
" plugin from http://vim-scripts.org/vim/scripts.html
" Plugin 'L9'
" Git plugin not hosted on GitHub
" Plugin 'git://git.wincent.com/command-t.git'
" git repos on your local machine (i.e. when working on your own plugin)
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
" Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
" Avoid a name conflict with L9
" Plugin 'user/L9', {'name': 'newL9'}

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

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
inoremap <F4> <c-o>:!make -j8<cr>
inoremap <F2> <c-o>:w<cr>
inoremap <F3> <c-o><c-w><c-w>
map <F3> <c-w><c-w>
nnoremap  <F7> <c-o>:tabnew<cr>
nnoremap  <F8> <c-o>:tabclose<cr>
nnoremap  <F9> <Esc>gT
nnoremap  <F12> <Esc>gt
nnoremap  ; :
inoremap  <F7> <c-o>:tabnew<cr>
inoremap  <F8> <c-o>:tabclose<cr>
inoremap  <F9> <Esc>gT
inoremap  <F12> <Esc>gt
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
