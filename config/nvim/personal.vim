" Encoding
set encoding=utf-8
set fileencoding=utf-8
set fileencoding=utf-8

" set auto indent
set autoindent

" File indentation
set tabstop=2
set shiftwidth=2
set expandtab     " use space not tab
set autoindent

" Remap leader key
let mapleader=" "

" Enable hidden buffers
set hidden

" searching
set ignorecase                    " ignore case in search
set hlsearch                      " highlight all search matches
set cursorline                    " highlight current line
set smartcase                     " pay attention to case when caps are used

" enable visual bel (disable audio bell)
set vb

" reload file changed outside vim
set autoread

" common editor settings
set showmatch           " highlight matching brackets
set scrolloff=2         " minimum lines above/blow cursor
set nofoldenable        " disable code folding

" hint to keep lines short
if exists('+colorcolumn')
  set colorcolumn=80
endif

" Open new split panes to right and bottom, which feels more natural
" set splitbelow
set splitright
" Auto resize Vim splits to active split
set winwidth=104
set winheight=5
set winminheight=5
set winheight=999

" mustache/handlebars
let g:mustache_abbreviations = 1

" Remove trailing whitespace on save for ruby files.
au BufWritePre *.rb :%s/\s\+$//e

" *******************************************
"                 THEME
" *******************************************

set background=dark
colorscheme base16-railscasts
" further customization
highlight clear SignColumn
highlight VertSplit    ctermbg=236
highlight ColorColumn  ctermbg=237
highlight LineNr       ctermbg=236 ctermfg=240
highlight CursorLineNr ctermbg=236 ctermfg=240
highlight CursorLine   ctermbg=236
highlight StatusLineNC ctermbg=238 ctermfg=0
highlight StatusLine   ctermbg=240 ctermfg=12
highlight IncSearch    ctermbg=3   ctermfg=1
highlight Search       ctermbg=1   ctermfg=3
highlight Visual       ctermbg=3   ctermfg=0
highlight Pmenu        ctermbg=240 ctermfg=12
highlight PmenuSel     ctermbg=3   ctermfg=1
highlight SpellBad     ctermbg=0   ctermfg=1
" highlight trailing spaces in annoying red
highlight ExtraWhitespace ctermbg=1 guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()
" AIRLINE
" Enable the list of buffers
let g:airline#extensions#tabline#enabled = 1
" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline_theme='jellybeans'

" IndentLine
let g:indentLine_enabled = 1
let g:indentLine_concealcursor = 0
let g:indentLine_color_term = 239
" let g:indentLine_char = '│'
let g:indentLine_faster = 1


" *******************************************
" SEARCH, FUZZY FIND AND CONFIGURATION
" *******************************************

" set ack as grep
set grepprg=ack

" using CTRLP config
let g:ctrlp_map = '<leader>f'
let g:ctrlp_custom_ignore = '\v[\/](node_modules|target|dist|bower_components)|(\.(swp|tox|ico|git|hg|svn))$'
let g:ctrlp_use_caching = 1
let g:ctrlp_max_height = 30
let g:ctrlp_working_path_mode = 0
let g:ctrlp_match_window_reversed = 0
let g:ctrlp_open_new_file = 'r'
let g:ctrlp_cache_dir = $HOME . '/.cache/ctrlp'
" USE SILVER SEARCHER FOR CTRLP
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
" rubymine like recent files
noremap <leader>e :CtrlPBuffer<CR>

" in-built fuzzy find
set path+=**                                  " :find file inside recursive directory
set wildmenu                                  " enable bash style tab completion
set wildmode=list:longest,full
set wildignore+=*/tmp/*,*/node_modules/*,*/bower_components/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite

" NERDTree configurations
map <c-\> :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
noremap <leader>0 :NERDTreeFocus<CR>

" *******************************************
"             BINDINGS
" *******************************************

" markdown preview
map <leader>m :!open -a "Macdown" %<cr><cr>

" map git commands
map <leader>b :Gblame<cr>
map <leader>l :!clear && git log -p %<cr>
map <leader>d :!clear && git diff %<cr>

" map Silver Searcher
" map <leader>a :Ag!<space>
" bind \ (backward slash) to grep shortcut
command! -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
nnoremap \ :Ag<SPACE>
" Ag will search from project root
let g:ag_working_path_mode="r"

" clear the command line and search highlighting
noremap <esc> :nohl<return><esc>

" toggle spell check with <F5>
map <F5> :setlocal spell! spelllang=en_us<cr>
imap <F5> <ESC>:setlocal spell! spelllang=en_us<cr><Paste>

" rename current file, via Gary Bernhardt
function! RenameFile()
  let old_name = expand('%')
  let new_name = input('New file name: ', expand('%'))
  if new_name != '' && new_name != old_name
    exec ':saveas ' . new_name
    exec ':silent !rm ' . old_name
    redraw!
  endif
endfunction
map <leader>n :call RenameFile()<cr>

" remap vrc_trigger (vim rest console) to c-k
let g:vrc_trigger = '<C-k>'

"Use enter to create new lines w/o entering insert mode
nnoremap <CR> o<Esc>

" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

" Add vim notes in dropbox/notes directory (vim.notes)
let g:notes_directories = ['~/Dropbox/Notes']

noremap <leader>l :bn<CR>
noremap <leader>h :bp<CR>

" open a new empty buffer
nmap <C-t> :enew<cr>

" Close the current buffer and move to the previous one
" This replicates the idea of closing a tab
nmap <C-x> :bp <BAR> bd #<CR>

"" Open current line on GitHub
noremap ,o :!echo `git url`/blob/`git rev-parse --abbrev-ref HEAD`/%\#L<C-R>=line('.')<CR> \| xargs open<CR><CR>

" Re-indent everything, set cursor back to current line
" noremap <C-i> gg=G''

" BROKEN: right now (should work out of the box)
au  BufNewFile,BufRead *.handlebars,*.hbs set filetype=html.handlebars syntax=mustache | runtime! ftplugin/mustache.vim ftplugin/mustache*.vim ftplugin/mustache/*.vim
