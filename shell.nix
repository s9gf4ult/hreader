# shell.nix
let project = import ./default.nix { nixpkgs = <nixpkgs>; };
in project.shellFor {
  tools = {
    cabal = "latest";
    # stack = "latest";
    # hlint = "latest"; # Selects the latest version in the hackage.nix snapshot
    haskell-language-server = "latest";
  };
  exactDeps = true;
}
