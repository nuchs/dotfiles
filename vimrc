" ----------------------------------------------------------------------
" Settings
" ----------------------------------------------------------------------
set nocompatible

set autoindent
set backspace=indent,eol,start
set colorcolumn=+1,+2
set diffopt+=iwhite
set diffopt=vertical
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

nnoremap <Leader><Space> :b#<CR>

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

" Bad Ex mode. Bad!
nnoremap Q <Nop>

" ----------------------------------------------------------------------
" Plugins
" ----------------------------------------------------------------------
call plug#begin()

Plug 'AndrewRadev/tagalong.vim'
Plug 'airblade/vim-rooter'
Plug 'christoomey/vim-tmux-navigator'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'itchyny/lightline.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'joshdick/onedark.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'kien/rainbow_parentheses.vim'
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
nnoremap <Leader>r :Rg<CR>
nnoremap <Leader>/ :BLines<CR>
nnoremap <Leader>ff :Lines<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>g :GitFiles?<CR>
nnoremap <Leader>h :History<CR>
nnoremap <Leader>: :History:<CR>
nnoremap <Leader>// :History/<CR>
nnoremap <Leader>k :Helptags<CR>

" Light Line
" -----------------------------------
set noshowmode
set laststatus=2

" Livedown
" -----------------------------------
let g:livedown_open=1
let g:livedown_port=3000
let g:livedown_browser="chrome"

" CoC (Fuck knows what all this does)
" -----------------------------------
inoremap <silent><expr> <TAB>
      \ coc#pum#visible() ? coc#pum#next(1) :
      \ CheckBackspace() ? "\<Tab>" :
      \ coc#refresh()
inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"

" Make <CR> to accept selected completion item or notify coc.nvim to format
" <C-g>u breaks current undo, please make your own choice
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call ShowDocumentation()<CR>

function! ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
endfunction

" Symbol renaming
nmap <leader>rn <Plug>(coc-rename)

function! CocCurrentFunction()
    return get(b:, 'coc_current_function', '')
endfunction

let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'cocstatus', 'currentfunction', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'cocstatus': 'coc#status',
      \   'currentfunction': 'CocCurrentFunction'
      \ },
      \ }
