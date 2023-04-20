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

" TODO: draw top border to separate FZF buffer. Workaround here:
" https://github.com/neovim/neovim/issues/9718#issuecomment-559573308
function! FloatingFZF()
  let width = float2nr(winwidth(0) - 6)
  let win_height = winheight(0)
  let height = float2nr(win_height * 0.3)
  " 7 is totally random but looks nice
  if height < 7
    if win_height >= 7
      let height = 7
    endif
  endif
  let screenpos = win_screenpos(0)
  let pos_x = screenpos[1]
  let pos_y = screenpos[0]
  let opts = { 'relative': 'editor',
             \ 'row': (pos_y + win_height - height),
             \ 'col': (pos_x + 5),
             \ 'width': width,
             \ 'height': height,
             \ 'style': 'minimal'}

  call nvim_open_win(nvim_create_buf(v:false, v:true), v:true, opts)
endfunction

highlight def link NormalFloat Normal

let g:fzf_layout = { 'window': 'call FloatingFZF()' }
let g:fzf_buffers_jump = 1

" nnoremap <silent> <C-p> <Esc>:Files<CR>
" nnoremap <silent> <C-b> <Esc>:Buffers<CR>
" nnoremap <silent> <C-h> <Esc>:History<CR>
nnoremap <silent> <C-s> <Esc>:Ag<CR>
nnoremap <silent> <C-l> <Esc>:BLines<CR>
nnoremap <silent> <leader><Space> <Esc>:Commands<CR>
" Because `;` is mapped to `:`
" nnoremap <silent> <leader>; <Esc>:History:<CR>
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

" Don't show the status line when inside fzf
autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 ruler
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
  call fzf#vim#ag('',
  \               join(a:000, ' '),
  \               fzf#vim#with_preview(s:preview_opts),
  \               0)
endfunction

nnoremap <silent> <C-x> <Esc>:Agg<space>

" git push --set-upstream origin `current_branch`
function! Gpsup(...)
  let l:symbolicRef = substitute(system('git symbolic-ref --quiet HEAD'),'\n$','','''')
  let l:currentBranch = substitute(l:symbolicRef, '^refs/heads/', '', '''')

  execute 'Git push --set-upstream origin ' . l:currentBranch . ' ' . join(a:000, " ")
endfunction

command! -nargs=* Gpsup call Gpsup(<q-args>)
