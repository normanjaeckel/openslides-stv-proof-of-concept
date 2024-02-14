{ pkgs, ... }:

{
  # https://devenv.sh/

  packages = [
    pkgs.git
    pkgs.go-task
    pkgs.elmPackages.elm-test
   ];

  languages.elm.enable = true;

  # See full reference at https://devenv.sh/reference/options/
}
