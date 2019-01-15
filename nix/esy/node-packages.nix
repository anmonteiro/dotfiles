# This file has been generated by node2nix 1.6.0. Do not edit!

{nodeEnv, fetchurl, fetchgit, globalBuildInputs ? []}:

let
  sources = {
    "@esy-ocaml/esy-opam-0.0.15" = {
      name = "_at_esy-ocaml_slash_esy-opam";
      packageName = "@esy-ocaml/esy-opam";
      version = "0.0.15";
      src = fetchurl {
        url = "https://registry.npmjs.org/@esy-ocaml/esy-opam/-/esy-opam-0.0.15.tgz";
        sha512 = "s3gEaDMCSJwxxz+vjVYAVYWOMqJC+lIckT20A9sB2nYLQ8qGkkm8nIHd7LvgCFPUGxmETOy0etGMLA+K6v6WZw==";
      };
    };
    "esy-solve-cudf-0.1.10" = {
      name = "esy-solve-cudf";
      packageName = "esy-solve-cudf";
      version = "0.1.10";
      src = fetchurl {
        url = "https://registry.npmjs.org/esy-solve-cudf/-/esy-solve-cudf-0.1.10.tgz";
        sha512 = "/MrZOBH0wuJndvZN8pl+S3Mg3zJaK70PH9ZZwqDeJHulghEWROEZxpmenNiS9pqAaxyUVhTZJBt2/vL9jKCJbg==";
      };
    };
  };
in
{
  esy = nodeEnv.buildNodePackage {
    name = "esy";
    packageName = "esy";
    version = "0.5.1";
    src = fetchurl {
      url = "https://registry.npmjs.org/esy/-/esy-0.5.1.tgz";
      sha512 = "j/N3rjJPbzIRIRT4F6yzO9FJnM3DK4L/fLkGnG+hYuV5V3bTXW9wbm+cNfgar0Ymn73Hq+tKzr4Gp++a8Fe0CQ==";
    };
    dependencies = [
      sources."@esy-ocaml/esy-opam-0.0.15"
      sources."esy-solve-cudf-0.1.10"
    ];
    buildInputs = globalBuildInputs;
    meta = {
      description = "Package builder for esy";
      license = "MIT";
    };
    production = true;
    bypassCache = true;
  };
}
