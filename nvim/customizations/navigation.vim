set wildmode=longest:list,full
set wildignore+=*/_opam/*,*/_build/*,*/.svn/*,*/node_modules/*,*/_esy/*

nnoremap ; :
nmap <silent> <leader>/ :nohlsearch<CR>

nnoremap <silent> <leader>n :bn<CR>
nnoremap <silent> <leader>p :bp<CR>

""" FZF
let $FZF_DEFAULT_COMMAND = 'ag -l
      \ --nocolor
      \ --hidden
      \ --ignore .git
      \ --ignore .svn
      \ --ignore .hg
      \ --ignore .DS_Store
      \ --ignore node_modules
      \ -g ""'

let g:fzf_layout = { 'down': '~30%' }
let g:fzf_buffers_jump = 1

nnoremap <silent> <C-p> <Esc>:Files<CR>
nnoremap <silent> <C-b> <Esc>:Buffers<CR>
nnoremap <silent> <C-s> <Esc>:Ag<CR>

let g:fzf_colors = {
  \ 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment']
  \ }

