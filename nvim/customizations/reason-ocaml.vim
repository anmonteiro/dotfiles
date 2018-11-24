" let g:opamshare = substitute(system('opam config var bin'),'\n$','','''')
" execute "set rtp+=" . g:opamshare
let g:LanguageClient_serverCommands = {
    \ 'reason': ['/Users/anmonteiro/Documents/github/reason-language-server/_esy/default/build/default/bin/Bin.exe'],
    \ 'ocaml': ['/Users/anmonteiro/Documents/github/reason-language-server/_esy/default/build/default/bin/Bin.exe'],
    \ }
" ['ocaml-language-server', '--stdio'],

autocmd BufEnter *dune* :setlocal filetype=lisp
