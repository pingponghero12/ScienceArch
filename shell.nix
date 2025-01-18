{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
    buildInputs = [
        (pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [ 
            scotty
            wai-middleware-static
        ]))
    ];
}
