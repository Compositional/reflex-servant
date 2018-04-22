let internal = rec {
  pkgs = import <nixpkgs> {};
  inherit (pkgs) lib;
  hl = pkgs.haskell.lib;
  
  basePkgs = pkgs.haskell.packages.ghc802;
  # basePkgs = pkgs.haskellPackages;
  
  hs = basePkgs.extend(self: super: {
    reflex-servant = hl.addBuildTool (hl.dontJailbreak (self.callCabal2nix "reflex-servant" (lib.cleanSource ./.) {})) super.markdown-unlit;
  });
  servantOverrides_0_13 = self: super: {
    http-types = self.callHackage "http-types" "0.12" {};
    servant = hl.doJailbreak (self.callPackage ./nix/servant.nix {});
    servant-client-core = hl.doJailbreak (self.callHackage "servant-client-core" "0.13" {});
  };
  servantOverrides_0_12 = self: super: {
    servant = self.callHackage "servant" "0.12.1" {};
    servant-client-core = self.callHackage "servant-client-core" "0.12" {};
  };
  reflexOverrides = self: super: {
    exception-transformers = hl.dontCheck (super.exception-transformers);
    reflex = self.callPackage ./nix/reflex.nix {};
  };
};
in with internal; {
  v1 = ((hs.extend servantOverrides_0_13).extend reflexOverrides).reflex-servant;
  v2 = ((hs.extend servantOverrides_0_12).extend reflexOverrides).reflex-servant;
  inherit internal;
}
