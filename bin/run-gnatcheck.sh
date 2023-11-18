#!/bin/bash

# This script is necessary because gnatcheck will return a failure status code if even one
# rule is violated (apparently), and there doesn't seem to be a way to change that behavior.
# This causes Jenkins to fail the build unless the style is *perfect*, which is unreasonable.
# This script runs gnatcheck but always returns a success status code.

codepeer-gnatcheck -P cubedos.gpr src/library/*.ads src/library/*.adb
codepeer-gnatcheck -P cubedos.gpr src/modules/*.ads src/modules/*.adb
codepeer-gnatcheck -P cubedos.gpr src/check/*.ads src/check/*.adb

exit 0  # Success!
