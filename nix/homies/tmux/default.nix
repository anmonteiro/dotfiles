{
  lib,
  stdenv,
  tmux,
  fetchFromGitHub,
  fetchpatch,
  ncurses,
  jemalloc,
  python3,
  terminal-notifier,
  writeShellScript,
  replaceVars,
  symlinkJoin,
  makeWrapper,
}:

let
  patchedTmux = tmux.overrideAttrs (old: {
    version = "next-3.9";
    src = fetchFromGitHub {
      owner = "tmux";
      repo = "tmux";
      rev = "a73616d781f2af13085691c741cfb0780d365328";
      hash = "sha256-wYK44WiFo6l1RJxVPDZoR1A5jTVDwxueVaJegYsz1mE=";
    };
    patches = (old.patches or [ ]) ++ [
      (fetchpatch {
        name = "tmux-osc7501.patch";
        # https://github.com/tmux/tmux/pull/5830
        url = "https://github.com/tmux/tmux/compare/a73616d781f2af13085691c741cfb0780d365328...63dcc50c751c6a48c442ee16e99e6b309a02616f.diff";
        hash = "sha256-Zyh/FBaofxTpfmLpCb8YuXYCqLB92Cn1khzool0XOOg=";
      })
      ./osc7501-hook.patch
    ];
    buildInputs = (old.buildInputs or [ ]) ++ lib.optional stdenv.hostPlatform.isDarwin jemalloc;
    configureFlags =
      (old.configureFlags or [ ]) ++ lib.optional stdenv.hostPlatform.isDarwin "--enable-jemalloc";
    postPatch = (old.postPatch or "") + ''
      substituteInPlace regress/input-common.inc \
        regress/osc7501-root-subscriptions.sh \
        --replace-fail 'PATH=/bin:/usr/bin' '# Keep the Nix build PATH.'
    '';
    doCheck = true;
    nativeCheckInputs = (old.nativeCheckInputs or [ ]) ++ [ python3 ];
    checkPhase = ''
      runHook preCheck
      export TEST_TMUX="$PWD/tmux"
      export TERMINFO_DIRS="${ncurses}/share/terminfo"
      (
        cd regress
        sh osc7501-root.sh
        sh osc7501-root-subscriptions.sh
      )
      python3 ${./test-notify.py} \
        --tmux "$TEST_TMUX" \
        --notify ${../../../.codex/notify.py} \
        --config ${./tmux.conf}
      runHook postCheck
    '';
  });
  notify = writeShellScript "codex-tmux-notify" ''
    exec ${python3}/bin/python3 ${../../../.codex/notify.py} \
      --tmux ${patchedTmux}/bin/tmux \
      --notifier ${terminal-notifier}/bin/terminal-notifier -- "$@"
  '';
  config = replaceVars ./tmux.conf { tmux_notify = notify; };
in

symlinkJoin {
  name = "tmux";
  buildInputs = [ makeWrapper ];
  paths = [ patchedTmux ];
  postBuild = ''
    wrapProgram "$out/bin/tmux" --add-flags "-f ${config}"
  '';
}
