{pkgs ? import ./nixpkgs.nix {}}:

with pkgs;

mkShell {
  # XXX: hack for macosX, this flags disable bazel usage of xcode
  # Note: this is set even for linux so any regression introduced by this flag
  # will be catched earlier
  # See: https://github.com/bazelbuild/bazel/issues/4231
  BAZEL_USE_CPP_ONLY_TOOLCHAIN=1;

  # Set UTF-8 local so that run-tests can parse GHC's unicode output.
  LANG="C.UTF-8";

  buildInputs = [
    bazel_4
    git
    gnused
    nix
    openjdk11
    python3
    which
    # For stack_install.
    stack
    # Needed to get correct locale for tests with encoding
    glibcLocales
    # to avoid CA certificate failures on macOS CI
    cacert
  ];
}
