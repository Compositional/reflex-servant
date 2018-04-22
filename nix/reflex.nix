
# cabal2nix https://github.com/reflex-frp/reflex

{ mkDerivation, base, bifunctors, comonad, containers, criterion
, data-default, deepseq, dependent-map, dependent-sum
, exception-transformers, fetchgit, haskell-src-exts
, haskell-src-meta, lens, loch-th, MemoTrie, monad-control, mtl
, prim-uniq, primitive, process, random, ref-tf, reflection
, semigroupoids, semigroups, split, stdenv, stm, syb
, template-haskell, these, time, transformers, transformers-compat
, unbounded-delays
}:
mkDerivation {
  pname = "reflex";
  version = "0.5";
  src = fetchgit {
    url = "https://github.com/reflex-frp/reflex";
    sha256 = "0myyla3frfymig6bwip3n54z85r1kai65b0spw0pldxvrhg4k4xc";
    rev = "9f3356fecb1620f74b45291b38aaaafbe92458fa";
  };
  libraryHaskellDepends = [
    base bifunctors comonad containers data-default dependent-map
    dependent-sum exception-transformers haskell-src-exts
    haskell-src-meta lens MemoTrie monad-control mtl prim-uniq
    primitive random ref-tf reflection semigroupoids semigroups stm syb
    template-haskell these time transformers transformers-compat
    unbounded-delays
  ];
  testHaskellDepends = [
    base bifunctors containers deepseq dependent-map dependent-sum lens
    mtl ref-tf semigroups split these transformers
  ];
  benchmarkHaskellDepends = [
    base containers criterion deepseq dependent-map dependent-sum
    loch-th mtl primitive process ref-tf split stm time transformers
  ];
  homepage = "https://github.com/reflex-frp/reflex";
  description = "Higher-order Functional Reactive Programming";
  license = stdenv.lib.licenses.bsd3;
}
