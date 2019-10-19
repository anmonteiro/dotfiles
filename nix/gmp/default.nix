{ pkgs }:
# add lib and include paths to the outputs to install for gmp
pkgs.gmp6.overrideAttrs (oldAttrs: {
   meta = oldAttrs.meta // {
     outputsToInstall = [ "out" "dev" "info" ];
   };
})
