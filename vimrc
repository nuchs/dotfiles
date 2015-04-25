" ---------------------------------------------------
" Load Plugins
" ---------------------------------------------------

set nocompatible 
filetype off    
set rtp+=~/.vim/bundle/Vundle.vim
let path='~/.vim/bundle'
call vundle#begin(path)

Plugin 'gmarik/Vundle.vim'

Plugin 'altercation/vim-colors-solarized'
Plugin 'bling/vim-airline'
Plugin 'kien/ctrlp.vim'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'Raimondi/delimitMate'
Plugin 'scrooloose/nerdcommenter'
Plugin 'tpope/vim-dispatch'
Plugin 'tpope/vim-surround'
Plugin 'vim-scripts/bufkill.vim'

call vundle#end()            
filetype plugin indent on   

" ---------------------------------------------------
" General settings
" ---------------------------------------------------

set hidden
set vb
set history=150
set scrolloff=4
set nobackup
set autoindent
set tabstop=2
set shiftwidth=2
set softtabstop=2
set smarttab
set expandtab
set number
set ruler
set foldmethod=indent
set foldlevel=99
set diffopt=vertical
set diffopt+=iwhite
set hlsearch
set incsearch
set smartcase
set encoding=UTF-8
set backspace=indent,eol,start
set background=dark
set colorcolumn=+2,+3,+4
set guioptions=
set formatoptions-=ro
set diffexpr=MyDiff()
colorscheme solarized
syntax enable

" ---------------------------------------------------
" Key bindings
" ---------------------------------------------------

" Vimrc editing
noremap <F6> :split $MYVIMRC<CR>
noremap <F7> :wq<CR>:so $MYVIMRC<CR>

" Window management
nnoremap <C-s>   :vsplit<CR>
nnoremap <C-c>   :split<CR>
nnoremap <C-h>   <C-w><Left>
nnoremap <C-l>   <C-w><Right>
nnoremap <C-k>   <C-w><Up>
nnoremap <C-j>   <C-w><Down>
nnoremap <A-h> <C-w><
nnoremap <A-l> <C-w>>
nnoremap <A-k> <C-w>+
nnoremap <A-j> <C-w>-

" Movement
noremap  k gk
noremap  j gj
inoremap <A-l> <Right>
inoremap <A-h> <Left>
inoremap <A-k> <Up>
inoremap <A-j> <Down>

" Misc
inoremap jk <Esc>

" Force myself to use the right keys
inoremap <Esc> <Nop>
inoremap <Left> <Nop>
inoremap <Right> <Nop>
inoremap <Up> <Nop>
inoremap <Down> <Nop>

" Search and the command line
nnoremap :: q:
nnoremap // q/
nnoremap <Leader>z :reg<CR>

" Buffers
nnoremap <Tab> :bn<CR>
nnoremap <S-Tab> :bp<CR>
nnoremap <Leader>Q :bd<CR>
nnoremap <Leader>q :BD<CR>

" Bad Ex mode. Bad!
nnoremap Q <Nop>

" ---------------------------------------------------
" Autocommands
" ---------------------------------------------------

" Put these in an autocmd group, so that we can delete them easily.
augroup vimrcEx
au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  "Use groovy syntax highlighting for .gradle files
  au BufNewFile,BufRead *.gradle set filetype=groovy
 
  " When editing a file, always jump to the last known cursor position.
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  " Turn on rainbow parentheses
  au VimEnter * RainbowParenthesesToggle
  au Syntax * RainbowParenthesesLoadRound
  au Syntax * RainbowParenthesesLoadSquare
  au Syntax * RainbowParenthesesLoadBraces

augroup END

" ---------------------------------------------------
" Plugin configuration options
" ---------------------------------------------------

" Solarized
nnoremap <Leader>bg :let &background = ( &background == "dark"? "light" : "dark" )<CR>

" Airline
set laststatus=2
let g:airline#extensions#tabline#enabled = 1

" Dispatch
nnoremap <Leader>gc :Dispatch gradle clean<CR>
nnoremap <Leader>gb :Dispatch gradle build<CR>

" ---------------------------------------------------
" Random gumpf
" ---------------------------------------------------

function MyDiff()
  let opt = '-a --binary '
  if &diffopt =~ 'icase' | let opt = opt . '-i ' | endif
  if &diffopt =~ 'iwhite' | let opt = opt . '-b ' | endif
  let arg1 = v:fname_in
  if arg1 =~ ' ' | let arg1 = '"' . arg1 . '"' | endif
  let arg2 = v:fname_new
  if arg2 =~ ' ' | let arg2 = '"' . arg2 . '"' | endif
  let arg3 = v:fname_out
  if arg3 =~ ' ' | let arg3 = '"' . arg3 . '"' | endif
  let eq = ''
  if $VIMRUNTIME =~ ' '
    if &sh =~ '\<cmd'
      let cmd = '""' . $VIMRUNTIME . '\diff"'
      let eq = '"'
    else
      let cmd = substitute($VIMRUNTIME, ' ', '" ', '') . '\diff"'
    endif
  else
    let cmd = $VIMRUNTIME . '\diff'
  endif
  silent execute '!' . cmd . ' ' . opt . arg1 . ' ' . arg2 . ' > ' . arg3 . eq
endfunction

