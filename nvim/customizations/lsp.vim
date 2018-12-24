let g:LanguageClient_autoStart = 1
" set omnifunc=LanguageClient#complete
" set completefunc=LanguageClient#complete

" https://github.com/autozimu/LanguageClient-neovim/issues/380
" autocmd FileType ocaml,reason nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
" autocmd FileType ocaml,reason nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
" autocmd FileType ocaml,reason nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
" autocmd FileType ocaml,reason nnoremap <silent> gf :call LanguageClient#textDocument_formatting()<CR>
" autocmd FileType ocaml,reason nnoremap <silent> ge :call LanguageClient#explainErrorAtPoint()<CR>

" Run `refmt` on save
" autocmd FileType reason autocmd BufWritePre <buffer> call LanguageClient_textDocument_formatting()

let g:LanguageClient_loggingFile = '/tmp/LanguageClient.log'
let g:LanguageClient_loggingLevel = 'INFO'
let g:LanguageClient_serverStderr = '/tmp/LanguageServer.log'

