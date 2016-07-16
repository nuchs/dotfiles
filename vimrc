" ---------------------------------------------------
" Load Plugins
" ---------------------------------------------------

set nocompatible 
filetype off    
set rtp+=~/.vim/bundle/Vundle.vim
let path='~/.vim/bundle'
call vundle#begin(path)
Plugin 'gmarik/Vundle.vim'

Plugin 'airblade/vim-gitgutter'
Plugin 'altercation/vim-colors-solarized'
Plugin 'benmills/vimux'
Plugin 'bruno-/vim-man'
Plugin 'cespare/vim-toml'
Plugin 'Chiel92/vim-autoformat'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'jszakmeister/vim-togglecursor'
Plugin 'kien/ctrlp.vim'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'Raimondi/delimitMate'
Plugin 'regedarek/ZoomWin'
Plugin 'rking/ag.vim'
Plugin 'rust-lang/rust.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'SirVer/ultisnips'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tmux-plugins/vim-tmux'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'Valloric/YouCompleteMe'
Plugin 'vim-scripts/argtextobj.vim'
Plugin 'vim-scripts/bufkill.vim'

call vundle#end()            
filetype plugin indent on   

" ---------------------------------------------------
" General settings
" ---------------------------------------------------

let mapleader=" "
set autoindent
set background=dark
set backspace=indent,eol,start
set colorcolumn=+1,+2,+3
set completeopt-=preview
set diffexpr=MyDiff()
set diffopt+=iwhite
set diffopt=vertical
set encoding=UTF-8
set expandtab
set foldlevel=99
set foldmethod=indent
set formatoptions-=ro
set guifont=Inconsolata-dz
set guioptions=
set hidden
set history=150
set hlsearch
set incsearch
set nobackup
set number
set ruler
set scrolloff=3
set shiftwidth=2
set smartcase
set smarttab
set softtabstop=2
set tabstop=2
set textwidth=79
set vb
colorscheme solarized
syntax enable

" ---------------------------------------------------
" Key bindings
" ---------------------------------------------------

" Vimrc editing
noremap <F6> :e  $MYVIMRC<CR>
noremap <F7> :so $MYVIMRC<CR>

" Window management
nnoremap <Leader>s :vsplit<CR>
nnoremap <Leader>c :split<CR>
nnoremap <C-h>     <C-w><Left>
nnoremap <C-l>     <C-w><Right>
nnoremap <C-k>     <C-w><Up>
nnoremap <C-j>     <C-w><Down>
nnoremap <A-Left>  <C-w><
nnoremap <A-Right> <C-w>>
nnoremap <A-Up>    <C-w>+
nnoremap <A-Down>  <C-w>-

" Movement
noremap  k gk
noremap  j gj

set mouse+=a
if &term =~ '^screen'
    " tmux knows the extended mouse mode
    set ttymouse=xterm2
endif

" Misc
inoremap jk <Esc>
inoremap JK <Esc>

" Force myself to use the right keys
inoremap <Esc>   <Nop>
inoremap <Left>  <Nop>
inoremap <Right> <Nop>
inoremap <Up>    <Nop>
inoremap <Down>  <Nop>

" Search and the command line
nnoremap :: q:
nnoremap // q/
nnoremap <Leader>z :reg<CR>
nnoremap / /\v
vnoremap / /\v

" Buffers
nnoremap <Tab>     :bn<CR>
nnoremap <S-Tab>   :bp<CR>
nnoremap <Leader>Q :bd<CR>
nnoremap <Leader>q :BD<CR>
nnoremap <Leader><Space> :b#<CR>

" Bad Ex mode. Bad!
nnoremap Q <Nop>

" ---------------------------------------------------
" Autocommands
" ---------------------------------------------------

" Put these in an autocmd group, so that we can delete them easily.
augroup vimrcEx
au!

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

" Git Gutter
nnoremap <Leader>gp :GitGutterPreviewHunk<CR>

" Vundle
nnoremap <Leader>vi :PluginInstall<CR>
nnoremap <Leader>vu :PluginUpdate<CR>
nnoremap <Leader>vc :PluginClean<CR>

" Ultisnips
nnoremap <Leader>u :UltiSnipsEdit<CR>
let g:UltiSnipsExpandTrigger ="<c-k>"
let g:UltiSnipsJumpForwardTrigger="<c-k>"
let g:UltiSnipsJumpBackwardTrigger="<c-j>"

" Ag
let g:ag_prg="ag --vimgrep --smart-case"
nnoremap <Leader>a :Ag 

" Ctrl-P
let g:ctrlp_working_path_mode = 'rc'
nnoremap <Leader>f :CtrlP<CR>
nnoremap <Leader>m :CtrlPMRU<CR>
nnoremap <Leader>b :CtrlPBuffer<CR>
let g:ctrlp_custom_ignore = {
      \ 'dir':  '\v[\/]\.(git|hg|svn)$',
      \ 'file': '\v\.(exe|so|dll|class)$',
      \ }

" Vimux
let VimuxUseNearest = 0
nnoremap <Leader>rp :VimuxPromptCommand<CR>
nnoremap <Leader>rl :VimuxRunLastCommand<CR>
nnoremap <Leader>rb :VimuxPromptCommand("cargo build")<CR><CR>
nnoremap <Leader>rt :VimuxPromptCommand("cargo test")<CR><CR>

" YouCompleteMe
let g:ycm_rust_src_path = '/usr/src/rust/src'

" Rust auto format
let g:formatdef_rustfmt = '"rustfmt"'
let g:formatters_rust = ['rustfmt']
nnoremap <Leader>t :Autoformat<CR>

" ---------------------------------------------------
" Random gumpf
" ---------------------------------------------------

function! MyDiff()
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

