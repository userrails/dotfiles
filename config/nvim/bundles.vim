set nocompatible

call plug#begin('~/.local/share/nvim/plugged')

" Run interactve commands
Plug 'christoomey/vim-run-interactive'

" Fuzzy find
Plug 'ctrlpvim/ctrlp.vim'

" Run test case from within vim
Plug 'janko-m/vim-test'

" Make non existing directory when we create new file
Plug 'pbrisbin/vim-mkdir'

" Comment
Plug 'vim-scripts/tComment'

" useful tpope plugins
Plug 'tpope/vim-surround'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rake'

" Plugins with issue (load time issue and issue when saving file)
Plug 'scrooloose/syntastic'
Plug 'tpope/vim-bundler'

" alternative to grep
Plug 'mileszs/ack.vim'

" sidebar
Plug 'scrooloose/nerdtree'

" auto add closing pairs
Plug 'jiangmiao/auto-pairs'

" language packs (on demand)
Plug 'sheerun/vim-polyglot'

" Rails
Plug 'tpope/vim-rails'

" EMBER JS
Plug 'dsawardekar/portkey'
Plug 'dsawardekar/ember.vim'

" HTML
Plug 'mattn/emmet-vim'
Plug 'vim-scripts/HTML-AutoCloseTag'

" REST CLIENT
Plug 'diepm/vim-rest-console'

" THEME
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'Yggdroot/indentLine'

call plug#end()
