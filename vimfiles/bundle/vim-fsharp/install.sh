if ! [ -e ./packages/FAKE/tools/Fake.exe ]
then
    mono paket.bootstrapper.exe
    exit_code=$?
    if [ $exit_code -ne 0 ]; then
        exit $exit_code
    fi

    mono paket.exe restore
    exit_code=$?
    if [ $exit_code -ne 0 ]; then
        exit $exit_code
    fi
fi

mono packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO install.fsx 
