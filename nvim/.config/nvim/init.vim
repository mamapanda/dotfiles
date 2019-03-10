" Bootstrap `Vim-Plug'
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
" End Bootstrap


call plug#begin('~/.config/nvim/plugged')
" appearance
Plug 'crusoexia/vim-monokai'
Plug 'vim-airline/vim-airline'
" visual aids
Plug 'luochen1990/rainbow'
Plug 'machakann/vim-highlightedyank'
" navigation
Plug 'ctrlpvim/ctrlp.vim'
Plug 'vim-scripts/matchit.zip'
Plug 'justinmk/vim-sneak'
" editing
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-commentary'
Plug 'tommcdo/vim-exchange'
Plug 'tommcdo/vim-lion'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
" text objects
Plug 'wellle/targets.vim'
Plug 'michaeljsmith/vim-indent-object'
" language
Plug 'sheerun/vim-polyglot'
Plug 'jpalardy/vim-slime'
Plug 'neoclide/coc.nvim', {'tag': '*', 'do': {-> coc#util#install()}}
Plug 'w0rp/ale'
" git
Plug 'tpope/vim-fugitive'
call plug#end()


set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

set hidden
set nohlsearch
set ignorecase
set smartcase
set number
set relativenumber
set clipboard+=unnamedplus

inoremap fd    <Esc>
nnoremap Y     y$
tnoremap <C-w> <C-\><C-n><C-w>

colorscheme monokai

let g:rainbow_active = 1
let g:highlightedyank_highlight_duration = 200

let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

map f <Plug>Sneak_f
map F <Plug>Sneak_F
map t <Plug>Sneak_t
map T <Plug>Sneak_T

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> K  :call CocAction('doHover')<CR>
nmap <silent> \  :CocList diagnostics<CR>

let g:slime_target = 'neovim'
autocmd TermOpen * :echo b:terminal_job_id

let g:ale_linters = {
\   'c':          [],
\   'cpp':        [],
\   'css':        [],
\   'html':       [],
\   'javascript': [],
\   'json':       [],
\   'python':     [],
\   'r':          [],
\   'typescript': []
\}
let g:ale_fixers = {
\   'c':          ['clang-format'],
\   'cpp':        ['clang-format'],
\   'css':        ['prettier'],
\   'go':         ['gofmt'],
\   'html':       ['prettier'],
\   'javascript': ['prettier'],
\   'json':       ['prettier'],
\   'markdown':   ['prettier'],
\   'python':     ['black'],
\   'typescript': ['prettier']
\}
let g:ale_fix_on_save                 = 1
let g:ale_python_black_options        = '--line-length 80'
let g:ale_css_prettier_options        = '--tab-width 4'
let g:ale_javascript_prettier_options = '--tab-width 4'
let g:ale_typescript_prettier_options = '--tab-width 4'
