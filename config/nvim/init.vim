if ! filereadable(expand('~/.config/nvim/autoload/plug.vim'))
	echo "Downloading junegunn/vim-plug to manage plugins..."
	silent !mkdir -p ~/.config/nvim/autoload/
	silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ~/.config/nvim/autoload/plug.vim
endif

" Aparently is better to put this option in the begining of
" the config file.
set nocompatible

call plug#begin('~/.vim/plugged')

Plug 'junegunn/goyo.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'justinmk/vim-syntax-extra'
Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
Plug 'vimwiki/vimwiki'
Plug 'mhinz/vim-startify'
Plug 'luochen1990/rainbow'
Plug 'voldikss/vim-floaterm'

Plug 'jiangmiao/auto-pairs'

Plug 'psliwka/vim-smoothie'
"Plug 'christoomey/vim-tmux-navigator'

Plug 'puremourning/vimspector'

"Plug 'dhruvasagar/vim-table-mode'  "Better table in markdown

Plug 'tpope/vim-markdown'
Plug 'vim-pandoc/vim-pandoc-syntax'

""NerdTree Plugins
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'scrooloose/nerdtree'
Plug 'ryanoasis/vim-devicons'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

" git help
"Plug 'mhinz/vim-signify'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

""
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

Plug 'mbbill/undotree'

Plug 'easymotion/vim-easymotion'

Plug 'sheerun/vim-polyglot'

"Smart commenting
Plug 'scrooloose/nerdcommenter'

"Help to surround
Plug 'machakann/vim-sandwich'
"Highlight your yank aera
Plug 'machakann/vim-highlightedyank'

"Better Python syntax highlighting for Vim
Plug 'vim-python/python-syntax'

"Plug 'tomasiser/vim-code-dark'
Plug 'gruvbox-community/gruvbox'
"Plug 'morhetz/gruvbox'

"Plug 'kshenoy/vim-signature' "vim-signature is a plugin to place,
                             "toggle and display marks. And show it in the left.

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update


call plug#end()

let g:airline_theme='gruvbox'

" symbols section for unicode/airline symbols
" air-line
let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" unicode symbols
"let g:rirline_left_sep = '»'
let g:airline_left_sep = ''
"let g:airline_left_sep = '▶'
let g:airline_left_sep = ''
"let g:airline_right_sep = '«'
let g:airline_right_sep = ''
"let g:airline_right_sep = '◀ '
let g:airline_right_sep = ''
let g:airline_symbols.linenr = '␊ '
let g:airline_symbols.linenr = '␤ '
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.whitespace = 'Ξ'

" airline symbols
"let g:airline_left_sep = ''
let g:airline_left_sep = ''
"let g:airline_left_alt_sep = '❯'
let g:airline_left_alt_sep = ''
"let g:airline_right_sep = ''
let g:airline_right_sep = ''
"let g:airline_right_alt_sep = '❮'
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ' '
let g:airline_symbols.linenr = ''

" tabline show
let g:airline#extensions#tabline#enabled = 1
" tabline complete path of file
let g:airline#extensions#tabline#fnamecollapse = 0

" fullpath status line
let g:airline_section_c = '%<%F%m %#__accent_red#%{airline#util#wrap(airline#parts#readonly(),0)}%#__restore__#'


"" Encoding
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8


"" Fix backspace indent
set backspace=indent,eol,start

"" Tabs. May be overridden by autocmd rules
set tabstop=4
set softtabstop=0
set shiftwidth=4
set expandtab

set number relativenumber

:augroup numbertoggle
:  autocmd!
:  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
:  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
:augroup END

"" Searching
set hlsearch
set incsearch
set ignorecase
set smartcase
set inccommand=nosplit

" Search mappings: These will make it so that going to the next one in a
" search will center on the line it's found in.
nnoremap n nzzzv
nnoremap N Nzzzv

"Prerequisites for VimWiki
"Make sure you have these settings in your vimrc file:
"set nocompatible
filetype plugin on
syntax on

set ruler

"Hide the default mode text (e.g. -- INSERT -- below the statusline)
set noshowmode

imap kj <Esc>

" Rainbow Parenthesis activation
let g:rainbow_active = 1 "set to 0 if you want to enable it later via :RainbowToggle


" Enable autocompletion:
set wildmode=longest,list,full
" Disables automatic commenting on newline:
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o


" Enable autocompletion:
set wildmode=longest,list,full

" Disables automatic commenting on newline:
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Automatically deletes all trailing whitespace on save.
autocmd BufWritePre * %s/\s\+$//e



"set listchars=tab:>-,trail:.,extends:>,precedes:<,space:.,eol:¶
set listchars=tab:>-,trail:.,extends:>,precedes:<,space:•,eol:¶
"set list

" Use the system clipboard
set clipboard+=unnamedplus
"set clipboard=unnamed

" Open the split below and right not the other way around
set splitbelow splitright

set cursorline        "highlight current line
set cursorcolumn "highlight current column

set hidden

set undofile


" Make sure Vim returns to the same line when you reopen a file.
" Thanks, Amit
augroup line_return
    au!
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \     execute 'normal! g`"zvzz' |
        \ endif
augroup END


"hi cCustomFunc  gui=bold guifg=yellowgreen
"hi cCustomClass gui=reverse guifg=#00FF00



"" VimWiki
" Configure settings for VimWiki default path and default syntax
" in this case Munra is going to use Markdown
"let g:vimwiki_list = [{'path': '~/Storage/vimwiki/', 'syntax': 'markdown', 'ext': '.md'}]
let g:vimwiki_list = [{
	\ 'path': '~/Storage/vimwiki/',
	\ 'syntax': 'markdown',
	\ 'ext': '.md'}]

let g:vimwiki_listsyms = '✗ ✓ '

augroup pandoc_syntax
    au! BufNewFile,BufFilePre,BufRead *.md set filetype=markdown.pandoc
augroup END

"" Enable markdown syntax in .md files
"autocmd BufNewFile,BufFilePre,BufRead index.md set filetype=markdown
"autocmd BufRead,BufNew *.md setf markdown
"au BufNewFile,BufRead *.md,*.markdown,*.mdown,*.mkd,*.mkdn,README.md,index.md  setf markdown
""autocmd BufNewFil ,BufRead *.md set syntax=json
"au BufReadPost index.md set syntax=markdown
"let g:vimwiki_ext2syntax = {'.md': 'markdown', '.markdown': 'markdown'}



""" Define map leader and key bindings
let mapleader = "\<Space>"

nnoremap <Leader>f :NERDTreeToggle<Enter>
nnoremap <Leader>l :set list!<CR>
nnoremap <leader>n :set number! relativenumber!<CR>

""  Auto closing ", (, {, [
"" to avoid typing it every time and when we don't want the mapping,
"" we need to escape it using "ctrl" + "v" before typing the mapped char like
"inoremap " ""<left>
"inoremap ' ''<left>
"inoremap ( ()<left>
"inoremap [ []<left>
"inoremap { {}<left>
"inoremap {<CR> {<CR>}<ESC>O
"inoremap {;<CR> {<CR>};<ESC>O

nmap <A-h> <C-w>h
nmap <A-j> <C-w>j
nmap <A-k> <C-w>k
nmap <A-l> <C-w>l


""FuzzyFind
nnoremap <Leader>b :Buffers!<CR>
nnoremap <Leader>H :History!<CR>
"nnoremap <Leader>o :cd ~<CR>:Files!<CR>
nnoremap <Leader>o :cd .<CR>:Files!<CR>
nnoremap <leader><tab> :b#<cr>  "Buffer configs

" Bind "//" to a fzf-powered buffer search
nmap // :BLines!<CR>

" Bind "??" to a fzf-powered project search
"nmap ?? :Rg!<CR>

" Bind "cc" to a fzf-powered command search
"nmap cc :Commands!<CR>



"call fzf#run(fzf#wrap({'dir': '~'}))
" Allow FZF to searh from home directory not from working directory
"nnoremap <leader>o :call fzf#run({'options': '--reverse --prompt "FZF_from_Home: "', 'down': 10, 'dir': '~', 'sink': 'e' })<CR>
" Always enable preview window on the right with 60% width
"let g:fzf_preview_window = 'right:60%'
let g:fzf_layout = {'window': {'width':0.8, 'height':0.8 }}
let $FZF_DEFAULTS_OPTS='--reverse'

nnoremap <leader>h :nohl

" Floaterm
nnoremap <silent> <leader>t :FloatermNew<CR>
nnoremap <silent> <F4>   :FloatermToggle<CR>
tnoremap <silent> <F4>   <C-\><C-n>:FloatermToggle<CR>
nnoremap <silent> <leader>p :FloatermNew ipython<CR>

let g:floaterm_width=0.8
let g:floaterm_height=0.8
let g:floaterm_wintitle=0
let g:floaterm_autoclose=1

" Sneak
"map <leader>s <Plug>Sneak_s
"map <leader>S <Plug>Sneak_S

" Move across wrapped lines like regular lines
"Go to the first non-blank character of a line
nnoremap 0 ^
"Just in case you need to go to the very beginning of a line
nnoremap ¡ 0


nmap <leader>h :nohlsearch<cr>

" For config Undotree edit '.vim/plugged/undotree/plugin/undotree.vim'
nnoremap <leader>u :UndotreeToggle<cr>

" Easy Motion
map <leader><leader>. <Plug>(easymotion-repeat)
map <leader><leader>w <Plug>(easymotion-overwin-w)
map <leader><leader>f <Plug>(easymotion-overwin-f)
map <leader><leader>j <Plug>(easymotion-overwin-line)
map <leader><leader>k <Plug>(easymotion-overwin-line)


"  Spell check Two language in the same time.
"set spelllang=es_es,en_us
map <leader>se :setlocal spell! spelllang=en_us<CR>
map <leader>ss :setlocal spell! spelllang=es_es<CR>


" Move selected line / block of text in visual mode
" shift + k to move up
" shift + j to move down
xnoremap K :move '<-2<CR>gv-gv
xnoremap J :move '>+1<CR>gv-gv


noremap <leader>Pt :set paste<cr>
noremap <leader>Pn :set nopaste<cr>

" Stop using :w to save. Use :update instead.
" :update writes the file to disk only when there are changes.
" So, it could be really helpful especially if the file is huge.
"nnoremap <leader>w :update<cr>
"inoremap <leader>w <Esc>:update<cr>gi
nnoremap <leader>w :write<cr>
"inoremap <leader>w <Esc>:write<cr>gi

"Config vim-tmux-navigator
"let g:tmux_navigator_no_mappings = 1

"nnoremap <silent> <M-h> :TmuxNavigateLeft<cr>
"nnoremap <silent> <M-j> :TmuxNavigateDown<cr>
"nnoremap <silent> <M-k> :TmuxNavigateUp<cr>
"nnoremap <silent> <M-l> :TmuxNavigateRight<cr>
"nnoremap <silent> <M-\> :TmuxNavigatePrevious<cr>

:command WQ wq
:command Wq wq
:command W w
:command Q q


" Vimspector config
let g:vimspector_install_gadgets = [ 'debugpy', 'vscode-cpptools', 'CodeLLDB' ]
let g:vimspector_enable_mappings = 'HUMAN'
nmap <leader>V :call vimspector#Launch()<CR>
nmap <leader>vr :VimspectorReset<CR>
nmap <leader>ve :VimspectorEval
nmap <leader>vw :vimspectorWatch
nmap <leader>vo :VimspectorShowOutput
nmap <leader>vs :VimspectorStop


" vim-python/python-syntax. Enable all syntax highlighting features
let g:python_highlight_all = 1


let NERDTreeQuitOnOpen = 1
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let g:NERDTreeWinSize=50


" set a byte character marker (BOM) utf-8 symbol when retrieving file encoding
" disabled by default with no value
let g:WebDevIconsUnicodeByteOrderMarkerDefaultSymbol = ' '

""this is useful for avoiding unnecessary system()
let g:WebDevIconsOS = 'Darwin'

let g:WebDevIconsUnicodeDecorateFolderNodes = 1
let g:DevIconsEnableFoldersOpenClose = 1

let g:DevIconsDefaultFolderOpenSymbol=' ' " symbol for open folder (f07c)
let g:WebDevIconsUnicodeDecorateFolderNodesDefaultSymbol=' ' " symbol for closed folder (f07b)

let g:DevIconsEnableFolderExtensionPatternMatching = 1

let g:WebDevIconsUnicodeDecorateFolderNodes = 1

""This is for the git NerdTree Plugin
let g:loaded_nerdtree_git_status = 1
let g:NERDTreeShowIgnoredStatus = 1

let g:NERDTreeIndicatorMapCustom = {
    \ "Modified"  : "✹ ",
    \ "Staged"    : "✚ ",
    \ "Untracked" : "✭ ",
    \ "Renamed"   : "➜ ",
    \ "Unmerged"  : "═ ",
    \ "Deleted"   : "✖ ",
    \ "Dirty"     : "✗ ",
    \ "Clean"     : "✔︎ ",
    \ 'Ignored'   : '☒ ',
    \ "Unknown"   : "? "
    \ }

"" hexokinase Configuration
let g:Hexokinase_highlighters = ['backgroundfull']
" Patterns to match for all filetypes
" Can be a comma separated string or a list of strings
" Default value:
let g:Hexokinase_optInPatterns = 'full_hex,triple_hex,rgb,rgba,hsl,hsla,colour_names'
let g:Hexokinase_refreshEvents = ['InsertLeave']
" Reenable hexokinase on enter
autocmd VimEnter * HexokinaseTurnOn


"" Configs misc
"autocmd FileType tex,latex,markdown setlocal spell spelllang=en_au
" Vertically center document when entering insert mode
"autocmd InsertEnter * norm zz



""""Should be in the end if not transparent background do not work
colorscheme gruvbox
"colorscheme codedark
hi Normal guibg=NONE ctermbg=NONE
"let g:gruvbox_contrast_dark = 'soft'
set termguicolors
"set mouse=a
