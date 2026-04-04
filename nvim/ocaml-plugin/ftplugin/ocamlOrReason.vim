"
" Language:     Reason
" Description:  Vim ftplugin file for Reason
"

" Skip scratch buffers such as file-picker previews. They may borrow the
" filetype for syntax highlighting, but they are not real project files and
" should not try to register with Merlin.
if &buftype !=# ''
  finish
endif

if exists("b:finished_activating_merlin_for_buffer_successfully") && b:finished_activating_merlin_for_buffer_successfully
  finish
endif

let b:finished_activating_merlin_for_buffer_successfully = 0

if exists("b:doing_ftplugin") && b:doing_ftplugin
  " " Something had set ft=reason during the loading of this ftplugin/reason.vim
  " " file! This happens if we lazily load plugins.
  finish
endif

let b:doing_ftplugin = 1
let b:merlin_env = {}

if !b:finished_activating_merlin_for_buffer_successfully
  if exists("*MerlinSelectBinary")
    delfunction MerlinSelectBinary
  endif

  if executable('ocamlmerlin')
    " throw "hiii"
    let merlinBin = substitute(system('which ocamlmerlin'),'\n$','','''')

    " let merlinBase = fnamemodify(merlinBin, ':p:h:h')
    " let b:ocamlmerlinRtp = merlinBase . "/share/merlin/vim"
    call ReasonMaybeUseThisMerlinVimPluginForAllProjects(merlinBin)
  endif

  " if exists("b:ocamlmerlinRtp")
    " let g:plugs_reasonPluginLoader={}
    " let g:plugs_reasonPluginLoader['merlin'] = {'dir': (b:ocamlmerlinRtp)}
    " call call(function("ReasonPluginLoaderLoad"), keys(g:plugs_reasonPluginLoader))
    " execute "set rtp+=".b:ocamlmerlinRtp
  " endif
endif

" WARNING DO NOT EARLY RETURN
let b:doing_ftplugin = 0
