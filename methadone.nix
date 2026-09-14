{
  lib,
  babashka-unwrapped,
  writers,
  writeText,
  clj-kondo,
  makeBinaryWrapper,
  runCommand,
  package,
  binary ? package.pname or package.name,
}:

let
  # The sources are many files but the artefact is one, because the
  # writer installs a single script and because a hand-installed copy
  # has to be something a symlink can point at. bb's own uberscript
  # concatenates the namespaces in dependency order and appends the call
  # to -main, which is why no namespace invokes itself.
  uberscript = runCommand "methadone.clj" { nativeBuildInputs = [ babashka-unwrapped ]; } ''
    export HOME="$TMPDIR"
    bb uberscript "$out" --classpath ${./src} -m methadone.main
  '';

  # clj-kondo checks the namespace against the file name and the
  # writer's output is an extension-less file named after whatever it is
  # wrapping -- now doubly so, the bundle holding every namespace at
  # once. The linter is otherwise worth keeping, so silence just that
  # one.
  #
  # Passed as a file because the check string is word-split by the
  # builder without quote removal, so no argument may contain a space,
  # and with --lint last because the script path is appended to it.
  #
  # Passed at all because writeBabashka's own default for check never
  # reaches makeScriptWriter, so omitting it lints nothing.
  kondo = writeText "kondo.edn" ''
    {:linters {:namespace-name-mismatch {:level :off}}}
  '';

  methadone = writers.writeBabashkaBin "methadone" {
    check = "${lib.getExe clj-kondo} --config ${kondo} --lint";
  } uberscript;
in
runCommand binary
  {
    nativeBuildInputs = [ makeBinaryWrapper ];
    meta.mainProgram = binary;
  }
  ''
    makeWrapper ${methadone}/bin/methadone $out/bin/${binary} \
      --set METHADONE_BINARY ${package}/bin/${binary}
  ''
