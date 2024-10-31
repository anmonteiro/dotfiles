set wildmode=longest:list,full
set wildignore+=*/_opam/*,*/_build/*,*/.svn/*,*/node_modules/*,*/_esy/*

map ; :
nmap <silent> <leader>/ :nohlsearch<CR>

nnoremap <silent> <leader>n :bn<CR>
nnoremap <silent> <leader>p :bp<CR>

""" Vim / TMUX interaction
let g:tmux_navigator_no_mappings = 1

nnoremap <silent> <M-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <M-j> :TmuxNavigateDown<cr>
nnoremap <silent> <M-k> :TmuxNavigateUp<cr>
nnoremap <silent> <M-l> :TmuxNavigateRight<cr>
nnoremap <silent> <M-\> :TmuxNavigatePrevious<cr>

" new splits open to the right and bottom
set splitbelow
set splitright

highlight def link NormalFloat Normal

" git push --set-upstream origin `current_branch`
function! Gpsup(...)
  let l:symbolicRef = substitute(system('git symbolic-ref --quiet HEAD'),'\n$','','''')
  let l:currentBranch = substitute(l:symbolicRef, '^refs/heads/', '', '''')

  execute 'Git push --set-upstream origin ' . l:currentBranch . ' ' . join(a:000, " ")
endfunction

command! -nargs=* Gpsup call Gpsup(<q-args>)
