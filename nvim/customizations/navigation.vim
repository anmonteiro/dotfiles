set wildmode=longest:list,full
set wildignore+=*/_opam/*,*/_build/*,*/.svn/*

" CtrlP configuration
let g:ctrlp_map = '<c-c><c-p>'

let g:ctrlp_root_markers = ['dune-project', 'pom.xml', 'package.json', 'project.clj', 'build.boot', '.npmignore']
let g:ctrlp_show_hidden = 1


" let g:ctrlp_custom_ignore = {
  " \ 'dir': '\v[\/]_(opam|build)$',
  " \ }
