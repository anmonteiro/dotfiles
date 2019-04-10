# This file has been generated by node2nix 1.6.0. Do not edit!

{nodeEnv, fetchurl, fetchgit, globalBuildInputs ? []}:

let
  sources = {};
in
{
  now = nodeEnv.buildNodePackage {
    name = "now";
    packageName = "now";
    version = "15.0.3";
    src = fetchurl {
      url = "https://registry.npmjs.org/now/-/now-15.0.3.tgz";
      sha512 = "fbvgxX934odHavABfXV4dnejY2Q1robSbT/cqO2gckeqRyRXxQRLC1XWaEMXEvbdpIrNXgabDpd6a6rAXw+mfg==";
    };
    buildInputs = globalBuildInputs;
    meta = {
      description = "The command-line interface for Now";
      homepage = "https://github.com/zeit/now-cli#readme";
      license = "Apache-2.0";
    };
    production = true;
    bypassCache = true;
  };
}