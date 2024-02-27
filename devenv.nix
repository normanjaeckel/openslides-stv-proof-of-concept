{ pkgs, ... }:

{
  # https://devenv.sh/

  languages.zig.enable = true;

  packages = [
    pkgs.git
    pkgs.go-task
   ];

  enterShell = ''
    # See https://github.com/roc-lang/roc/issues/5573
    # ROCDIR="$(pwd)/$(find . -type d -iname 'roc_nightly*' | head -n 1)"
    ROCDIR="$(pwd)/roc/target/release"
    PATH=$ROCDIR:$PATH
  '';

  # See full reference at https://devenv.sh/reference/options/
}
