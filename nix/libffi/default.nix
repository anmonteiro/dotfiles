{ pkgs }:
{
  # add lib and include paths to the outputs to install for openssl
  libffi = pkgs.libffi.overrideAttrs (oldAttrs: {
     meta = oldAttrs.meta // {
       outputsToInstall = [ "out" "dev" "info" ];
     };
  });
}
