let g:airline#themes#taste#palette = {}

function! airline#themes#taste#refresh() abort
  let g:airline#themes#taste#palette = luaeval(
        \ "type(package.loaded['config.theme.taste']) == 'table'"
        \ . " and package.loaded['config.theme.taste'].airline_palette()"
        \ . " or {}"
        \ )
endfunction

call airline#themes#taste#refresh()
