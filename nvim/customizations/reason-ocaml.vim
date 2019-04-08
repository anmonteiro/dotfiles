" let g:opamshare = substitute(system('opam config var bin'),'\n$','','''')
" execute "set rtp+=" . g:opamshare
let g:LanguageClient_serverCommands = {
    \ 'reason': ['/Users/anmonteiro/Documents/github/reason-language-server/_esy/default/build/default/bin/Bin.exe'],
    \ 'ocaml': ['/Users/anmonteiro/Documents/github/reason-language-server/_esy/default/build/default/bin/Bin.exe'],
    \ }
" ['ocaml-language-server', '--stdio'],

autocmd FileType reason silent! call merlin#Register()
autocmd FileType reason nnoremap <silent> gf :ReasonPrettyPrint<CR>
autocmd FileType ocaml,reason nnoremap <silent> <localleader>d :MerlinDocument<CR>

let g:reasonml_project_airline = 1
let g:reasonml_syntastic_airline = 1
let g:reasonml_clean_project_airline = 1
let g:syntastic_reason = 1
let g:syntastic_ocaml_checkers = ['merlin']
let g:syntastic_reason_checkers = ['merlin']
let g:airline#extensions#esy#enabled = 1
let g:airline#extensions#reason#enabled = 1

" Neoformat
let g:neoformat_ocaml_ocamlformat = {
  \ 'exe': 'ocamlformat',
  \ 'args': ['--disable-outside-detected-project', '--name', '"%:p"', '-'],
  \ 'no_append': 1,
  \ 'stdin': 1,
  \ }

let g:neoformat_enabled_ocaml = ['ocamlformat']
