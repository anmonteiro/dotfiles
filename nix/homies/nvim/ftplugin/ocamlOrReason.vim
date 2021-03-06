"
" Language:     Reason
" Description:  Vim ftplugin file for Reason

if exists("b:finished_activating_merlin_for_buffer_successfully") && b:finished_activating_merlin_for_buffer_successfully
  finish
endif

if exists("b:doing_ftplugin") && b:doing_ftplugin
  " Something had set ft=reason during the loading of this ftplugin/reason.vim
  " file! This happens if we lazily load plugins.
  finish
endif

" Where esy is found to be installed in the local project as a ./esy
" (This is not a typical use case.)
let b:reasonml_local_esy_discovered_path=''
" The discovered version of that binary. Empty object means not valid.
let b:reasonml_local_esy_discovered_version={}
" We'll check at most one time per local project - too expensive otherwise.
let b:reasonml_local_esy_checked=0

 " Sets the g: version of those variables.
call esy#SetGlobalEsy()
" We won't warn for missing global esy/old versions because maybe the local
" project has it.


let b:doing_ftplugin = 1
let b:merlin_env = {}

" Don't need output from this function in case we're not in an Esy project.
silent let b:finished_activating_merlin_for_buffer_successfully = reason#LoadBuffer()

if !b:finished_activating_merlin_for_buffer_successfully
  if exists("*MerlinSelectBinary")
    delfunction MerlinSelectBinary
  endif

  if executable('ocamlmerlin')
    let merlinBin = substitute(system('which ocamlmerlin'),'\n$','','''')
    let merlinBase = fnamemodify(merlinBin, ':p:h:h')
    let b:ocamlmerlinRtp = merlinBase . "/share/merlin/vim"
  endif

  if exists("b:ocamlmerlinRtp")
    let g:plugs_reasonPluginLoader={}
    let g:plugs_reasonPluginLoader['merlin'] = {'dir': (b:ocamlmerlinRtp)}
    call call(function("ReasonPluginLoaderLoad"), keys(g:plugs_reasonPluginLoader))
    execute "set rtp+=".b:ocamlmerlinRtp
  endif
endif

" WARNING DO NOT EARLY RETURN
let b:doing_ftplugin = 0
