" ---------------------------------------------------
" Load Plugins
" ---------------------------------------------------

set nocompatible 
filetype off    
set rtp+=~/.vim/bundle/Vundle.vim
let path='~/.vim/bundle'
call vundle#begin(path)

Plugin 'airblade/vim-gitgutter'
Plugin 'airblade/vim-rooter'
Plugin 'altercation/vim-colors-solarized'
Plugin 'benmills/vimux'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'gmarik/Vundle.vim'
Plugin 'jszakmeister/vim-togglecursor'
Plugin 'kien/ctrlp.vim'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'Raimondi/delimitMate'
Plugin 'regedarek/ZoomWin'
Plugin 'rking/ag.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'SirVer/ultisnips'
Plugin 'sjl/gundo.vim'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'vim-scripts/argtextobj.vim'
Plugin 'vim-scripts/AutoComplPop'
Plugin 'vim-scripts/bufkill.vim'
Plugin 'vim-scripts/LustyJuggler'
Plugin 'vim-scripts/YankRing.vim'
Plugin 'wellle/tmux-complete.vim'

call vundle#end()            
filetype plugin indent on   

" ---------------------------------------------------
" General settings
" ---------------------------------------------------

let mapleader=" "
set hidden
set vb
set history=150
set scrolloff=3
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
set guifont=Inconsolata-dz
set formatoptions-=ro
set diffexpr=MyDiff()
colorscheme solarized
syntax enable

" ---------------------------------------------------
" Key bindings
" ---------------------------------------------------

" Vimrc editing
noremap <F6> :split $MYVIMRC<CR>
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
inoremap <A-l> <Right>
inoremap <A-h> <Left>
inoremap <A-k> <Up>
inoremap <A-j> <Down>

set mouse+=a
if &term =~ '^screen'
    " tmux knows the extended mouse mode
    set ttymouse=xterm2
endif

" Misc
inoremap jk <Esc>

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
nnoremap <Leader>q :bd<CR>
nnoremap <Leader>Q :BD<CR>

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
nnoremap <Leader>t :let &background = ( &background == "dark"? "light" : "dark" )<CR>

"Git Gutter
nnoremap <Leader>gp :GitGutterPreviewHunk<CR>

" Vundle
nnoremap <Leader>vi :PluginInstall<CR>
nnoremap <Leader>vu :PluginUpdate<CR>
nnoremap <Leader>vc :PluginClean<CR>

" Ultisnips
nnoremap <Leader>u :UltiSnipsEdit<CR>

" Ag
let g:ag_prg="ag --vimgrep --smart-case"
nnoremap <Leader>a :Ag 

" Yank Ring
let g:yankring_min_element_length = 2
let g:yankring_history_dir = '~/.vim/'
nnoremap <silent> <Leader>y :YRShow<CR>

" Ctrl-P
let g:ctrlp_working_path_mode = 'rc'
nnoremap <Leader>f :CtrlP<CR>
nnoremap <Leader>m :CtrlPMRU<CR>

" Lusty Juggler
let g:LustyJugglerShowKeys = 'a' 
let g:LustyJugglerDefaultMappings = 0
nnoremap <Leader><Space> :LustyJugglePrevious<CR>
nnoremap <Leader>b       :LustyJuggler<CR>

" Vimux
nnoremap <Leader>rp :VimuxPromptCommand<CR>
nnoremap <Leader>rl :VimuxRunLastCommand<CR>
nnoremap <Leader>ri :VimuxInspectRunner<CR>
nnoremap <Leader>rx :VimuxCloseRunner<CR>
nnoremap <Leader>rc :VimuxInterruptRunner<CR>
nnoremap <Leader>rz :VimuxZoomRunner<CR>

" Gundo
nnoremap <F5> :GundoToggle<CR>

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

