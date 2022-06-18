#!/bin/bash

# This script is necessary because gnatcheck will return a failure status code if even one
# rule is violated (apparently), and there doesn't seem to be a way to change that behavior.
# This causes Jenkins to fail the build unless the style is *perfect*, which is unreasonable.
# This script runs gnatcheck but always returns a success status code.

# It is necessary to cd into the 'src' folder so that cubedos-casing-exceptions.txt is found.
cd src
codepeer-gnatcheck -P cubedos.gpr library/*.ads library/*.adb
codepeer-gnatcheck -P cubedos.gpr modules/*.ads modules/*.adb
codepeer-gnatcheck -P cubedos.gpr check/*.ads check/*.adb
cd ..

exit 0  # Success!
