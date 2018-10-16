" let g:opamshare = substitute(system('opam config var bin'),'\n$','','''')
" execute "set rtp+=" . g:opamshare
let g:LanguageClient_serverCommands = {
    \ 'reason': ['ocaml-language-server', '--stdio'],
    \ 'ocaml': ['ocaml-language-server', '--stdio'],
    \ }

autocmd BufEnter *dune* :setlocal filetype=lisp
