{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
    buildInputs = [
        (pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [ 
            scotty
            wai-middleware-static
            mysql-simple
            clientsession
            wai-session
            bytestring
            vault
            wai-session-clientsession
            data-default
        ]))
        pkgs.haskellPackages.cabal-install
        pkgs.plantuml
        pkgs.mariadb
        pkgs.docker
        pkgs.docker-compose
    ];
}
