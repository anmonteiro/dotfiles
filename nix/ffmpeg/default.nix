{ pkgs }:
{
  # add lib and include paths to the outputs to install for openssl
  ffmpeg_4 = pkgs.ffmpeg_4.overrideAttrs (oldAttrs: {
     meta = oldAttrs.meta // {
       outputsToInstall = [ "out" "dev" "bin" ];
     };
  });
}
