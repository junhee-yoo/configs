set statusline="%f%m%r%h%w [%Y] [0x%02.2B]%< %F%=%4v,%4l %3p%% of %L"
set tabstop=4
set shiftwidth=4
set cindent
set autoindent
set smartindent
syntax on~@~@~@~@~@~@~@~@~@~@
set incsearch
set backspace=eol,start,indent
set history=1000
set hlsearch
hi Search ctermbg=grey
set ignorecase
set showmatch
set tagbsearch
set cst
let NERDTreeIgnore=['.o$']
set textwidth=80
set colorcolumn=+1

set expandtab
set softtabstop=4
"set textwidth=80
"set wrap
"set cinoptions=h1,l1,g1,t0,i4,+4,(0,w1,W4

filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#begin()
Plugin 'gmarik/vundle'
" non github repos
Plugin 'fatih/vim-go'
Plugin 'The-NERD-tree'
Plugin 'Command-T'
call vundle#end()

filetype plugin indent on

if filereadable(".vimrc_local")
    so .vimrc_local
else
    if filereadable("../.vimrc_local")
        so ../.vimrc_local
    endif
endif

nmap <F9> :NERDTreeToggle<CR>

if exists("g:did_load_filetypes")
    filetype off
    filetype plugin indent off
endif
set runtimepath+=$GOROOT/misc/vim
    filetype plugin indent on
    syntax on

"set tags=~/workspace/src/mongo/src/mongo/tags
set tags=~/workspace/src/aerospike-server/tags
set tags=~/workspace/src/as_all_port/tags

set csprg=/usr/bin/cscope
set csto=0
set cst
set nocsverb

if filereadable("./cscope.out")
    cs add cscope.out
else
    cs add ~/workspace/src/mongo/src/mongo/cscope.out
    cs add ~/workspace/src/aerospike-server/cscope.out
    cs add ~/workspace/projects/as_all_port/cscope.out
endif
set csverb

map <C-\> :vsp <CR> :wincmd l<CR> :exec("tag ".expand("<cword>"))<CR>
"map <C-\> : tab split<CR>:exec("tag ".expand("<cword>"))<CR>
"map <A-]> : vsp <CR>:exec("tag ".expand("<cword>"))<CR>
"fun! SPLITAG() range
"    let oldfile=expand("%:p")
"    if &modified
"        split
"    endif
"    exe "tag ". expand("<cword>")
"    let curfile=expand("%:p")
"    if curfile == oldfile
"        let pos=getpos(".")
"        if &modified
"            " if we have split before:
"            quit
"        endif
"        call setpos('.',pos)
"    endif
"endfun
"nmap <C-]> :call SPLITAG()<CR>z.

" Automatically open, but do not go to (if there are errors) the quickfix /
" location list window, or close it when is has become empty.
"
" Note: Must allow nesting of autocmds to enable any customizations for quickfix
" buffers.
" Note: Normally, :cwindow jumps to the quickfix window if the command opens it
" (but not if it's already open). However, as part of the autocmd, this doesn't
" seem to happen.
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow
