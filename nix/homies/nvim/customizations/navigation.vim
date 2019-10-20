set wildmode=longest:list,full
set wildignore+=*/_opam/*,*/_build/*,*/.svn/*,*/node_modules/*,*/_esy/*

map ; :
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
nnoremap <silent> <C-h> <Esc>:History<CR>
nnoremap <silent> <C-s> <Esc>:Ag<CR>
nnoremap <silent> <C-l> <Esc>:BLines<CR>
nnoremap <silent> <leader><Space> <Esc>:Commands<CR>
" Because `;` is mapped to `:`
nnoremap <silent> <leader>; <Esc>:History:<CR>
" nnoremap <silent> <leader>/ <Esc>:History/<CR>

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

let s:preview_opts = 'right:40%'

command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 fzf#vim#with_preview(s:preview_opts),
  \                 <bang>0)

""" TODO: bang support
command! -nargs=* Ag1 call Agg_with_one_opt(<f-args>)
command! -nargs=* Agg call Agg_with_opts(<f-args>)

function! Agg_with_one_opt( ... )
  let s:args = copy(a:000)
  let s:opt = remove(s:args, 0)
  call fzf#vim#ag(join(s:args, ' '),
  \               s:opt,
  \               fzf#vim#with_preview(s:preview_opts),
  \               0)
endfunction

function! Agg_with_opts( ... )
  let s:args = copy(a:000)
  call fzf#vim#ag('',
  \               join(a:000, ' '),
  \               fzf#vim#with_preview(s:preview_opts),
  \               0)
endfunction

nnoremap <silent> <C-x> <Esc>:Agg<space>