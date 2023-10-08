{
  description = "A very basic flake";

  inputs.haskellNix.url = "haskellNix"; # This uses the registry. old way was  "github:input-output-hk/haskell.nix"
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-2305";

  outputs = { self, nixpkgs, haskellNix }:
    let
      system = "x86_64-linux";
      overlays = [ ] ;
      #   haskellNix.overlay
      #   (final: prev: {

      #     # haskell-nix = prev.haskell-nix // {
      #     #   extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings // {
      #     #     # String pkgconfig-depends names are mapped to lists of Nixpkgs
      #     #     # package names
      #     #     "python3-ptghci" = [ "python3-ptghci" ];
      #     #   };
      #     # };

      #       # in (pwp.override {
      #       #   makeWrapperArgs = ["--set PYTHONHOME $out"];
      #       # })
      #       #  .overrideAttrs (finalAttrs: prevAddrs: {
      #       #  pname = "python3";
      #       #})

      #     ptghci-hs = final.haskell-nix.cabalProject' {
      #       src = ./.;
      #       compiler-nix-name = "ghc945";
      #       shell.tools = {
      #         cabal = {};
      #         hlint = {};
      #         ghcid = {};
      #         haskell-language-server = {

      #           version = "latest";
      #           cabalProject = ''
      #               packages: .
      #               package haskell-language-server
      #                flags: -floskell -stylishHaskell -eval
      #             '';
      #           modules = [{
      #             packages.haskell-language-server.flags.floskell = false;
      #             packages.haskell-language-server.flags.stylishHaskell = false;
      #             packages.haskell-language-server.flags.eval = false;
      #           }];
      #         };
      #       };
      #     };

      #     ptghci =
      #       let
      #         python3-ptghci = prev.python3.withPackages(ps: with ps; [
      #           ansicolors prompt-toolkit pygments pygtrie pyyaml pyzmq six
      #           wcwidth
      #         ]);
      #         flake = pkgs.ptghci-hs.flake { };
      #         executablePkg = flake.packages."ptghci:exe:ptghci";
      #         pybits = ./pybits;
      #       in executablePkg.overrideAttrs(old: {
      #         buildInputs = old.buildInputs ++ [ pkgs.makeWrapper ];
      #         # wrap the binary in a script where the appropriate env var is set
      #         postInstall = old.postInstall or "" + ''
      #           wrapProgram "$out/bin/ptghci" \
      #             --set PYTHONPATH "${python3-ptghci.sitePackages}:${pybits}"
      #         '';
      #       });
      #   })
      # ];
      pkgs = import nixpkgs { inherit system overlays; };

      ptghci =
        let drv = pkgs.haskellPackages.callCabal2nix "ptghci" ./.;
            python =
              (pkgs.python310.withPackages(ps: with ps; [
                ansicolors prompt-toolkit pygments pygtrie pyyaml pyzmq six wcwidth
              ])).override {
                makeWrapperArgs = ["--set PYTHONHOME $out"];
              };
            pybits = ./pybits;
        in (pkgs.callPackage drv { python3-embed = python; })
              .overrideAttrs (old: {
                doCheck = false;
                buildInputs = old.buildInputs ++ [ pkgs.makeWrapper ];
                postInstall = old.postInstall or "" + ''
                  wrapProgram "$out/bin/ptghci" \
                    --set PYTHONPATH "${python.out}/${python.sitePackages}:${pybits}"
                '';
              });


    # ptghci_builtins = (pkgs.haskellPackages.developPackage {
    #   name = "ptghci";
    #   root = ./.;
    #   modifier = drv: builtins.trace drv (
    #     let
    #       #drvPythonApplied = attrs: drv (attrs // {python3-embed = pkgs.python3;});
    #     in (drv { python3-embed = pkgs.python3;}).override { python3-embed = pkgs.python3; });
        #   drv' =
        #   pkgs.haskell.lib.addBuildTools drvPythonApplied (with pkgs.haskellPackages;
        #     [ cabal-install
        #       pkg-config
        #       pkgs.python310.withPackages(ps: with ps; [
        #         ansicolors prompt-toolkit pygments pygtrie pyyaml pyzmq six wcwidth zmq
        #       ])
        #     ]);
        # in (pkgs.haskell.lib.addExtraLibraries drv' (with pkgs.haskellPackages;
        #     [
        #       pkgs.python310.withPackages(ps: with ps; [
        #         ansicolors prompt-toolkit pygments pygtrie pyyaml pyzmq six wcwidth zmq
        #       ])
        #     ])));
    #});

  in {

    #daPkgs = pkgs;
    packages.x86_64-linux.ptghci = ptghci;

    packages.x86_64-linux.default = self.packages.x86_64-linux.ptghci;

  };
}
