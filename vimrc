set nocompatible
filetype off

" ---------------------------------------------------
" Vundle bundles
" ---------------------------------------------------

set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'Raimondi/delimitMate'
Plugin 'SirVer/ultisnips'
Plugin 'Valloric/YouCompleteMe'
Plugin 'altercation/vim-colors-solarized'
Plugin 'duff/vim-scratch'
Plugin 'mhinz/vim-startify'
Plugin 'jelera/vim-javascript-syntax'
Plugin 'marijnh/tern_for_vim'
Plugin 'mileszs/ack.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-fugitive'
Plugin 'wincent/command-t'

call vundle#end()

" ---------------------------------------------------
" General settings
" ---------------------------------------------------

filetype plugin indent on
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
nnoremap <A-S-h> <C-w><
nnoremap <A-S-l> <C-w>>
nnoremap <A-S-k> <C-w>+
nnoremap <A-S-j> <C-w>-

" Movement
noremap  k gk
noremap  j gj
noremap  H 0
noremap  L $
noremap  K H
noremap  J L
nnoremap <A-h> <C-o>
nnoremap <A-l> <C-i>
inoremap <C-l> <Right>
inoremap <C-g> <Left>
inoremap <C-k> <Up>
inoremap <C-j> <Down>

" Misc
nnoremap <A-k> K
nnoremap <A-j> J
inoremap jk <Esc>
nnoremap <Leader>bg :let &background = ( &background == "dark"? "light" : "dark" )<CR>

" Force myself to use the right keys
inoremap <Esc> <Nop>
inoremap <Left> <Nop>
inoremap <Right> <Nop>
inoremap <Up> <Nop>
inoremap <Down> <Nop>

" Search and the command line
nnoremap <BSlash> :nohlsearch<cr>
nnoremap :: q:
nnoremap // q/

" ---------------------------------------------------
" Plugin configuration options
" ---------------------------------------------------

" You complete me
let g:ycm_add_preview_to_completeopt=0
let g:ycm_confirm_extra_conf=0
set completeopt-=preview

" Ultisnips
let g:UltiSnipsListSnippets="<Leader>ul"
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-n>"
let g:UltiSnipsEditSplit="vertical"
noremap <Leader>ue :UltiSnipsEdit<CR>

" Syntastic
noremap <F5> :SyntasticCheck<CR>:Errors<CR>
nnoremap [e :lprev<CR>
nnoremap ]e :lnext<CR>

" Ack
nnoremap <Leader>a :Ack!
nnoremap [q :cprev<CR>
nnoremap ]q :cnext<CR>

" Conque
noremap <Leader>s :ConqueTermVSplit bash<CR>
noremap <Leader>c :ConqueTermSplit bash<CR>

 " VCSCommand
noremap <F4> :VCSVimDiff<CR>

" Tern
nnoremap <Leader>jt :TernType<CR>
nnoremap <Leader>jr :TernRefs<CR>
nnoremap <Leader>jn :TernRename<CR>
nnoremap <Leader>jd :TernDef<CR>
nnoremap <Leader>js :TernDefSplit<CR>
nnoremap <Leader>ji :TernDoc<CR>
