" Language:     OCaml
" Description:  Vim ftplugin file for Reason

if exists("b:finished_activating_buffer_successfully")
  finish
endif

if exists("b:doing_ftplugin")
  " something had set ft=reason during the loading of this ftplugin/reason.vim
  " file! this happens if we lazily load plugins.
  finish
endif

let b:doing_ftplugin = 1
let b:did_warn_no_esy_yet = 0
let b:did_warn_cant_status = 0

" call ReasonEnsureShellPlugins()

" Still waiting to load an esy project. It's okay, you can retry again by
" resettig the fieltype=reason
let projectRoot = esy#FetchProjectRoot()
if projectRoot != []
  call esy#TrySetGlobalEsyBinaryOrWarn()
  if empty(g:reasonml_esy_discovered_path)
    let b:doing_ftplugin =0
    finish
  endif
  let info = esy#FetchProjectInfoForProjectRoot(projectRoot)
  " For every new buffer we can perform the check again if necessary.
  if empty(info)
  else
    let status = esy#ProjectStatusOfProjectInfo(info)
    if empty(status) || !status['isProject']
      " Okay, maybe this is a BuckleScript, or OPAM package. We'll work with
      " the globally installed toolchain
      " Detect when an esy field is later added. We'll need to completely kill
      " merlin. We can only have one version of merlin loaded per Vim.
    else
      if !status['isProjectReadyForDev']
        let msg = status['isProjectFetched'] ? 'Project installed but not built. Run esy build.' : 'Project not installed and not built. Run esy from the root directory.'
        call console#Info("Esy: " . msg)
        let b:doing_ftplugin =0
        finish
      endif
    endif
  endif
endif

let s:save_cpo = &cpo
set cpo&vim

let &cpo = s:save_cpo
unlet s:save_cpo

" vim-reason-loader code
" =============
" The following two "if executable" checks are the primary original code for
" this plugin. The majority of the remaining code is simply copied from
" Vim-Plug in order to reuse Vim-Plug's lazy loading code.
if !empty(projectRoot)
  let b:thisProjectsMerlinPath = esy#EsyLocateBinary("ocamlmerlin")
else
  let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
  let g:reasonml_ocamlmerlin_path = g:opamshare . "/merlin/vim"
  let b:thisProjectsMerlinPath = -1
endif

" Calling into this function, actually ends up setting ft=reason so you get
" caught in a loop which is why we have a b:doing_ftplugin variable). If
" b:doing_ftplugin is 1, then it means we're in a "reentrant" ftplugin call
" and we know to bail, letting the original call succeed. Calling into here
" will also end up calling plugin/reason.vim's `MerlinSelectBinary()` if
" merlin was found at this project path and the merlin vim plugin was loaded.
if b:thisProjectsMerlinPath != -1
  call ReasonMaybeUseThisMerlinForAllProjects(b:thisProjectsMerlinPath)
endif

" ReasonMaybeUseThisMerlinForAllProjects should set
" g:reasonml_ocamlmerlin_path if it was able to.
if !empty(g:reasonml_ocamlmerlin_path)
  if exists('g:merlin')
    let res = merlin#Register()
  else
    if exists("*MerlinSelectBinary")
      delfunction MerlinSelectBinary
    endif
    let ocamlmerlinRtp = g:opamshare . "/merlin/vim"
    let g:plugs_reasonPluginLoader={}
    let g:plugs_reasonPluginLoader['merlin'] = {'dir': (ocamlmerlinRtp)}
    call call(function("ReasonPluginLoaderLoad"), keys(g:plugs_reasonPluginLoader))
    execute "set rtp+=".ocamlmerlinRtp
  endif
  let b:finished_activating_buffer_successfully = 1
else
  " Do not set b:finished_activating_buffer_successfully. Could not find merlin.
  let res = console#Error("Could not find merlin support. Is it listed in your devDependencies?")
endif

let b:doing_ftplugin = 0
