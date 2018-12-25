set wildmode=longest:list,full
set wildignore+=*/_opam/*,*/_build/*,*/.svn/*,*/node_modules/*,*/_esy/*

" CtrlP configuration
let g:ctrlp_map = '<c-c><c-p>'

let g:ctrlp_root_markers = [
      \ 'dune-project',
      \ 'pom.xml',
      \ 'package.json',
      \ 'project.clj',
      \ 'build.boot',
      \ '.npmignore'
      \ ]

let g:ctrlp_show_hidden = 1

nnoremap ; :
nmap <silent> <leader>/ :nohlsearch<CR>


nnoremap <silent> <leader>n :bn<CR>
nnoremap <silent> <leader>p :bp<CR>
