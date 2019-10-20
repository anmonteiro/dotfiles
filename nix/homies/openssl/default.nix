pkgs:
pkgs.openssl_1_1.overrideAttrs (oldAttrs: {
   meta = oldAttrs.meta // {
     outputsToInstall = [ "out" "bin" "dev" ];
   };
})
