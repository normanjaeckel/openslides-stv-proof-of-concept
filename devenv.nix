{ pkgs, ... }:

{
  # https://devenv.sh/

  packages = [
    pkgs.git
    pkgs.go-task
   ];

  languages.elm.enable = true;

  # See full reference at https://devenv.sh/reference/options/
}
