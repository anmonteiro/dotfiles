# This file has been generated by node2nix 1.6.0. Do not edit!

{nodeEnv, fetchurl, fetchgit, globalBuildInputs ? []}:

let
  sources = {};
in
{
  bs-platform = nodeEnv.buildNodePackage {
    name = "bs-platform";
    packageName = "bs-platform";
    version = "4.0.18";
    src = fetchurl {
      url = "https://registry.npmjs.org/bs-platform/-/bs-platform-4.0.18.tgz";
      sha512 = "BwzW0iYHvREqUZIgQxJmdJrxexppLvJxYQ4LLexbhCp7uZU5DIZ5ub4ZHpkCkc8fn8bsXWc+Rrejb3csi+BoAQ==";
    };
    buildInputs = globalBuildInputs;
    meta = {
      description = "bucklescript compiler, ocaml standard libary by bucklescript and its required runtime support";
      homepage = "https://github.com/bucklescript/bucklescript#readme";
      license = "SEE LICENSE IN LICENSE";
    };
    production = true;
    bypassCache = true;
  };
}