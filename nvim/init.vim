set clipboard+=unnamedplus
set confirm | set autowriteall | set hidden

call plug#begin()

" Syntax highlighting
Plug 'sheerun/vim-polyglot'

" Modeline theme
Plug 'itchyny/lightline.vim'

" Color scheme
Plug 'catppuccin/nvim', { 'as': 'catppuccin' }

Plug 'tpope/vim-fugitive'

Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

Plug 'preservim/nerdtree'

Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'github/copilot.vim'

Plug 'tpope/vim-fugitive'

call plug#end()

"Colorscheme
colorscheme catppuccin 

" Custom fzf find files in directory of active buffer
function! ABFiles()
  execute 'FZF' expand('%:p:h')
endfunction

function! GitRoot()
  let l:cwd = expand('%:p:h')
  let l:git_dir = system('git -C ' . l:cwd . ' rev-parse --show-toplevel 2>/dev/null')
  return len(l:git_dir) > 0 ? substitute(l:git_dir, '\n$', '', '') : l:cwd
endfunction

" Only search text in git repo or current dir of the currently active buffer
command! -bang -nargs=* MyRg call fzf#vim#grep("rg --column --line-number --no-heading --color=always --smart-case -- ".shellescape(<q-args>).' '.GitRoot(), 1, fzf#vim#with_preview(), <bang>0)

let g:fzf_layout = { 'down': '20%' }

nnoremap <C-j> :NERDTree %:p:h<CR>
nnoremap <C-x>b :Buffers<CR>
nnoremap <C-p> :MyRg<CR>
nnoremap <C-x>f :call ABFiles()<CR>
nnoremap <leader>c :nohlsearch<CR>
nnoremap <leader>f *<CR>
nnoremap <C-c>m :vertical G<CR>

imap qw <ESC>:w<CR><ESC>
imap wq <ESC>:w<CR><ESC>
imap jk <ESC>
imap kj <ESC>

set ignorecase
set smartcase
set incsearch

set nowildmenu
set wildmode=longest:full,full

set number
set nowrap

set guifont=Hack:h11
