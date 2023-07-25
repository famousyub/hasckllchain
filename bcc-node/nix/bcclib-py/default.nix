{
  python3Packages
, bcc-cli
}:

python3Packages.buildPythonPackage {
  version = "1.0.0";
  pname = "bcc-lib-py";
  src = ./.;
  propagatedBuildInputs = [
    bcc-cli
  ];
}
