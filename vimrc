" ----------------------------------------------------------------------
" Settings
" ----------------------------------------------------------------------
set nocompatible

set autoindent
set backspace=indent,eol,start
set colorcolumn=+1,+2
set encoding=utf-8
set expandtab
set hidden
set history=3000
set hlsearch
set incsearch
set mouse=a
set nobackup
set noerrorbells
set nowritebackup
set number
set ruler
set scrolloff=3
set shiftwidth=2
set showcmd
set showmatch
set signcolumn=yes
set smartcase
set smartindent
set smarttab
set tabstop=2
set textwidth=80
set termwinsize=15x0
set vb

filetype plugin indent on
syntax on
colorscheme onedark
   
let &t_SI .= "\<Esc>[6 q"
let &t_EI .= "\<Esc>[0 q"

" ----------------------------------------------------------------------
" Mappings
" ----------------------------------------------------------------------
let mapleader=" "

nnoremap <Leader><Leader> <C-6><CR>

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap k gk
nnoremap j gj
nnoremap gk k
nnoremap gj j

inoremap jk <ESC>
nnoremap <Leader>q :q<CR>
nnoremap <Leader>Q :bd<CR>
nnoremap <Leader>t :bo :terminal<CR>
nnoremap <Leader>v :e ~/.vimrc<CR>

" ----------------------------------------------------------------------
" Plugins
" ----------------------------------------------------------------------
call plug#begin()

Plug 'AndrewRadev/tagalong.vim'
Plug 'airblade/vim-rooter'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'itchyny/lightline.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'joshdick/onedark.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'mattn/emmet-vim'
Plug 'shime/vim-livedown'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'

call plug#end()

" ----------------------------------------------------------------------
" Plugin Configuration
" ----------------------------------------------------------------------

" Vim Plug
" -----------------------------------
nnoremap <Leader>pi  :PlugInstall<CR>
nnoremap <Leader>pu  :PlugUpdate<CR>
nnoremap <Leader>pc  :PlugClean<CR>
nnoremap <Leader>puu :PlugUpgrade<CR>
nnoremap <Leader>ps  :PlugStatus<CR>

" Vim Commentary
" -----------------------------------
vnoremap <Leader>c :Commentary<CR>
nnoremap <Leader>c :Commentary<CR>

" FZF
" -----------------------------------
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>/ :Lines<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>g :GitFiles<CR>

" Light Line
" -----------------------------------
set noshowmode
set laststatus=2

" Livedown
" -----------------------------------
let g:livedown_open=1
let g:livedown_port=3000
let g:livedown_browser="chrome"

" CoC
" -----------------------------------


