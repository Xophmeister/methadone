{
  lib,
  babashka-unwrapped,
  writers,
  writeText,
  clj-kondo,
  makeBinaryWrapper,
  runCommand,
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

  # NOTE The output binary's name must match the package's expectations
  # (see `methadone.process`), so that process discovery works.
  methadone-bin = "methadone";

  methadone = writers.writeBabashkaBin methadone-bin {
    check = "${lib.getExe clj-kondo} --config ${kondo} --lint";
  } uberscript;
in
{
  # Methadone standing in front of an agent, under that agent's name.
  # METHADONE_BINARY is what tells it there is something to supervise:
  # the wrapper always invokes the script under its own name, so the
  # name alone cannot distinguish this from a report.
  wrap =
    {
      package,
      binary ? package.pname or package.name,
    }:
    runCommand binary
      {
        nativeBuildInputs = [ makeBinaryWrapper ];
        meta.mainProgram = binary;
      }
      ''
        makeWrapper ${methadone}/bin/${methadone-bin} $out/bin/${binary} \
          --set METHADONE_BINARY ${package}/bin/${binary}
      '';

  # Methadone under its own name, reporting on what it has recorded.
  #
  # The variable is cleared rather than merely left unset: it is
  # inherited by everything a supervised agent runs, so a report asked
  # for from inside a session would otherwise find it, take itself for a
  # wrapper and launch a second agent. Methadone clears it for the
  # agent's children too, which covers a hand-installed symlink -- this
  # covers the case of a stale one already in the environment.
  stats =
    runCommand methadone-bin
      {
        nativeBuildInputs = [ makeBinaryWrapper ];
        meta.mainProgram = methadone-bin;
      }
      ''
        makeWrapper ${methadone}/bin/${methadone-bin} $out/bin/${methadone-bin} \
          --unset METHADONE_BINARY
      '';
}
