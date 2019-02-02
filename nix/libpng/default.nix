{ pkgs }:
{
  # add lib and include paths to the outputs to install for openssl
  gmp6 = pkgs.libpng.overrideAttrs (oldAttrs: {
     meta = oldAttrs.meta // {
       outputsToInstall = [ "out" "dev" ];
     };
  });
}
