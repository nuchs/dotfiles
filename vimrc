" === Settings === {{{1
set nocompatible

set autoindent
set backspace=indent,eol,start
set colorcolumn=+1,+2
set diffopt+=iwhite
set diffopt=vertical
set encoding=utf-8
set expandtab
set foldmethod=marker
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
set term=kitty
set termguicolors
set textwidth=80
set vb

filetype plugin indent on
syntax on
   
" Change cursor shape based on mode
let &t_SI .= "\<Esc>[6 q"
let &t_EI .= "\<Esc>[0 q"

" === Commands & Functions === {{{1
if has("autocmd")
    au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endif

:source $MYETC/vimbreviations

" === Mappings === {{{1
let mapleader=" "

nnoremap <Leader><Space> :b#<CR>
nnoremap <Leader>o :ZoomWin<CR>
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
nnoremap <Leader>Q :wqa<CR>
nnoremap <Leader>t :bo :terminal<CR>
nnoremap <Leader>v :e $MYVIMRC<CR>
nnoremap <Leader>sv :source $MYVIMRC<CR>

" Copy to Wayland clipboard
nnoremap <C-c> :call system("wl-copy", @")<CR>
xnoremap <silent> <C-c> :w !wl-copy<CR><CR>

" Bad Ex mode. Bad!
nnoremap Q <Nop>

" === Plugins === {{{1
call plug#begin()

Plug 'AndrewRadev/tagalong.vim'
Plug 'itchyny/lightline.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'nuchs/vim-hypr-nav'
Plug 'mattn/emmet-vim'
Plug 'mcchrish/nnn.vim'
Plug 'morhetz/gruvbox'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'theRealCarneiro/hyprland-vim-syntax'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'vim-scripts/ZoomWin'

call plug#end()

" === Plugin Configuration === {{{1

" Colorscheme {{{2
" -----------------------------------
colorscheme gruvbox
let g:gruvbox_contrast_dark = 'hard'
set background=dark

" Vim Plug {{{2
" -----------------------------------
nnoremap <Leader>pi  :PlugInstall<CR>
nnoremap <Leader>pu  :PlugUpdate<CR>
nnoremap <Leader>pc  :PlugClean<CR>
nnoremap <Leader>puu :PlugUpgrade<CR>
nnoremap <Leader>ps  :PlugStatus<CR>

" Vim Commentary {{{2
" -----------------------------------
vnoremap <Leader>c :Commentary<CR>
nnoremap <Leader>c :Commentary<CR>

" FZF {{{2
" -----------------------------------
nnoremap <Leader>/ :BLines<CR>
nnoremap <Leader>ff :Lines<CR>
nnoremap <Leader>rg :Rg<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>g :GitFiles?<CR>
nnoremap <Leader>h :History<CR>
nnoremap <Leader>: :History:<CR>
nnoremap <Leader>// :History/<CR>
nnoremap <Leader>k :Helptags<CR>

" Light Line {{{2
" -----------------------------------
set noshowmode
set laststatus=2

" NNN {{{2
" -----------------------------------
let g:nnn#layout = { 'window': { 'width': 0.5, 'height': 0.6, 'xoffset':0.9, 'highlight': 'Comment' } }

" CoC (Fuck knows what all this does) {{{2
" -----------------------------------
nnoremap <Leader>cc :CocConfig<CR>

let g:coc_global_extensions = [
  \'coc-css',
  \'coc-dictionary',
  \'coc-eslint',
  \'coc-go',
  \'coc-html',
  \'coc-json',
  \'coc-prettier',
  \'coc-rust-analyzer',
  \'coc-sh',
  \'coc-syntax',
  \'coc-tsserver',
  \'coc-vimlsp',
  \'coc-word',
  \]

inoremap <silent><expr> <TAB>
      \ coc#pum#visible() ? coc#_select_confirm() :
      \ CheckBackspace() ? "\<TAB>" :
      \ coc#refresh()

function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list
nmap <silent> <F7> <Plug>(coc-diagnostic-prev)
nmap <silent> <F8> <Plug>(coc-diagnostic-next)
nnoremap <Leader>d :CocDiagnostics<CR>

" GoTo code navigation
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Symbol renaming
nmap <leader>rn <Plug>(coc-rename)

" Use K to show documentation in preview window
nnoremap <silent> K :call ShowDocumentation()<CR>

function! ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
endfunction

" Status line
function! CocCurrentFunction()
    return get(b:, 'coc_current_function', '')
endfunction

let g:lightline = {
      \ 'colorscheme': 'gruvbox',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'cocstatus', 'currentfunction', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'cocstatus': 'coc#status',
      \   'currentfunction': 'CocCurrentFunction'
      \ },
      \ }

