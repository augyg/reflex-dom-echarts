{ mkDerivation, aeson, base, bytestring, containers
, data-default, dependent-sum, ghcjs-dom, jsaddle
, lens, lib, reflex, reflex-dom-core, scientific, text, time
, unordered-containers, vector, pkgs
}:
let
  #inherit (nixpkgs) pkgs;
  nix-thunk = pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "nix-thunk";
    rev = "8fe6f2de2579ea3f17df2127f6b9f49db1be189f";
    sha256 = "14l2k6wipam33696v3dr3chysxhqcy0j7hxfr10c0bxd1pxv7s8b";
  };
  n = import nix-thunk {};
  hlib = pkgs.haskell.lib ;
      # echarts-jsdomSrc = pkgs.fetchFromGitHub {
    #   owner = "augyg";
    #   repo = "echarts-jsdom";
    #   rev = "aaffb109ef01a449b36bb6d27be8111bb72ae0dc";
    #   sha256 = "sha256-o0pYQy6J3L91RNArbcfMaFgLXvGZarugJLVPQpteCMQ=";
    # };

  echarts-jsdom = pkgs.haskellPackages.callCabal2nix "echarts-jsdom" (
    pkgs.fetchFromGitHub {
      owner = "augyg";
      repo = "echarts-jsdom";
      rev = "aaffb109ef01a449b36bb6d27be8111bb72ae0dc";
      sha256 = "sha256-o0pYQy6J3L91RNArbcfMaFgLXvGZarugJLVPQpteCMQ=";
    };
    #n.thunkSource ./deps/echarts-jsdom
  ) {};

in
mkDerivation {
  pname = "reflex-dom-echarts";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers data-default dependent-sum
    (pkgs.haskell.lib.doJailbreak echarts-jsdom) ghcjs-dom jsaddle lens reflex reflex-dom-core
    scientific text time unordered-containers vector
  ];
  license = "unknown";
}





