" Bootstrap `Vim-Plug'
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
" End Bootstrap


call plug#begin('~/.config/nvim/plugged')
" appearance
Plug 'sickill/vim-monokai'
Plug 'itchyny/lightline.vim'
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
Plug 'MarcWeber/vim-addon-mw-utils'  " vim-snipmate dependency
Plug 'tomtom/tlib_vim'               " vim-snipmate dependency
Plug 'garbas/vim-snipmate'
Plug 'honza/vim-snippets'
" git
Plug 'tpope/vim-fugitive'
call plug#end()


colorscheme monokai

set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

set hidden
set ignorecase
set smartcase
set number
set relativenumber
set clipboard+=unnamedplus
set termguicolors

inoremap fd    <Esc>
nnoremap Y     y$
tnoremap <C-w> <C-\><C-n><C-w>

highlight Search ctermfg=white ctermbg=magenta
highlight Search guifg=white guibg=magenta

autocmd CmdlineEnter /,\? :set hlsearch
autocmd CmdlineLeave /,\? :set nohlsearch

let g:lightline = {
\   'colorscheme' : 'molokai'
\}

let g:rainbow_active = 1
let g:highlightedyank_highlight_duration = 200

let g:ctrlp_user_command = 'rg %s --color=never --files --hidden --glob "!.git/"'
let g:ctrlp_use_caching  = 0

let g:sneak#use_ic_scs = 1
map f <Plug>Sneak_f
map F <Plug>Sneak_F
map t <Plug>Sneak_t
map T <Plug>Sneak_T

autocmd FileType r setlocal commentstring=##\ %s

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
\   'typescript': ['prettier'],
\   '*':          ['remove_trailing_lines', 'trim_whitespace']
\}
let g:ale_fix_on_save                 = 1
let g:ale_python_black_options        = '--line-length 80'
let g:ale_javascript_prettier_options = '--tab-width 4'  " css, js, ts
autocmd FileType html let b:ale_javascript_prettier_options = '--tab-width 2'

smapclear  " vim-snipmate uses select-mode
