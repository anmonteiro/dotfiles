{ pkgs }:
{
  # add lib and include paths to the outputs to install for openssl
  openssl_1_1 = pkgs.openssl_1_1.overrideAttrs (oldAttrs: {
     meta = oldAttrs.meta // {
       outputsToInstall = [ "out" "bin" "dev" ];
     };
  });
}
