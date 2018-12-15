{ pkgs }:
{
  # add lib and include paths to the outputs to install for openssl
  gmp6 = pkgs.gmp6.overrideAttrs (oldAttrs: {
     meta = oldAttrs.meta // {
       outputsToInstall = [ "out" "dev" "info" ];
     };
  });
}
