# This file has been generated by node2nix 1.7.0. Do not edit!

{nodeEnv, fetchurl, fetchgit, globalBuildInputs ? []}:

let
  sources = {};
in
{
  now = nodeEnv.buildNodePackage {
    name = "now";
    packageName = "now";
    version = "16.6.0";
    src = fetchurl {
      url = "https://registry.npmjs.org/now/-/now-16.6.0.tgz";
      sha512 = "OHWgh7QOgXi7IDI68yJ7SBTcQZr8rFGdM4w2W5CdOyCoISletVhb1+KO4g4iBQGVFVNp1W6hv4qJGAOODmib5Q==";
    };
    buildInputs = globalBuildInputs;
    meta = {
      description = "The command-line interface for Now";
      homepage = https://zeit.co/;
      license = "Apache-2.0";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
}