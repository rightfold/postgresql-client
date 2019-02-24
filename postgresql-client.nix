{mkDerivation, base, binary, bytestring, network, vector}:
mkDerivation {
    pname = "postgresql-client";
    version = "0.0.0.0";
    license = null;
    src = builtins.filterSource (p: t: p != toString ./dist &&
                                       p != toString ./result)
                                ./.;
    buildDepends = [
        base
        binary
        bytestring
        network
        vector
    ];
}
