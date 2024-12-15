" Performs any sandbox/environment switching/reloading/cache-invalidation.
" Exposes descriptoin of result so that callers can customize the UI to the
" specific flow.
" TODO: Avoid invalidating caches unless the new projectInfo is actually
" actionable, but invalidate caches on correct builds. It could be in an
" intermediate state during development. Keep the last working environment in
" tact.
function! reason#RegisterMerlinOnEnvironmentChangedForReadyProject()
  let forcedMerlin = g:reasonml_force_ocamlmerlin_path
  let merlinBin = substitute(system('which ocamlmerlin'),'\n$','','''')
  let merlinPath = !empty(forcedMerlin) ? forcedMerlin : (v:shell_error == 0 ? merlinBin : -1)
  if merlinPath != -1 && !empty(merlinPath)
    " Load merlin vim plugin if necessary/possible. Calling into this
    " function, actually ends up setting ft=reason so you get caught in a loop
    " which is why we have a b:doing_ftplugin variable). If b:doing_ftplugin
    " is 1, then it means we're in a "reentrant" ftplugin call and we know to
    " bail, letting the original call succeed. Calling into here will also end
    " up calling plugin/reason.vim's `MerlinSelectBinary()` if merlin was
    " found at this project path and the merlin vim plugin was loaded. TODO:
    " We shouldn't ever have a globally registered merlin path. It should
    " always be tracked per project sandbox per file.
    call ReasonMaybeUseThisMerlinVimPluginForAllProjects(merlinPath)
    " g:merlin was provided by merlin loaded plugin.
    if exists('g:merlin')
      " Merlin looks for them under these names.
      let b:merlin_path = merlinPath
      " Set the most recent merlin env/path in case we need some backup later.
      let g:reasonml_most_recent_ocamlmerlin_path = merlinPath
      let g:reasonml_most_recent_merlin_env = b:merlin_env
      " Registers this buffer with merlin, will trigger the MerlinSelectBinary call.
      try
        call merlin#Register()
      catch
        call console#Warn(v:exception)
        call console#Warn("Could not load merlin merlin support. The environment might not be setup correctly - see :messages for exception")
      endtry
    endif
    return 1
  else
    return 0
  endif
endfunction


function! reason#LoadBuffer()
  " For every new buffer we can perform the check again if necessary.
  let res = reason#RegisterMerlinOnEnvironmentChangedForReadyProject()
  if res == -1
    call console#Warn("Could not find merlin support. Is it listed in your devDependencies?")
    return 0
  endif
  return res
endfunction
