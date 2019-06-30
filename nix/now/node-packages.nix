# This file has been generated by node2nix 1.7.0. Do not edit!

{nodeEnv, fetchurl, fetchgit, globalBuildInputs ? []}:

let
  sources = {};
in
{
  now = nodeEnv.buildNodePackage {
    name = "now";
    packageName = "now";
    version = "15.6.0";
    src = fetchurl {
      url = "https://registry.npmjs.org/now/-/now-15.6.0.tgz";
      sha512 = "qvZyBYOUs3Cr+J8CZNgGVOgU1SFp/OFnkDESpJf5O4TnaiZzAZfrOGy7aZdkXJj6SALS94gfctPqh9LczJ2ldA==";
    };
    buildInputs = globalBuildInputs;
    meta = {
      description = "The command-line interface for Now";
      homepage = "https://github.com/zeit/now-cli#readme";
      license = "Apache-2.0";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
}