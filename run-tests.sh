#!/bin/bash


Normal () { printf '\e[m'"$*"; }
Tred () { printf '\e[0;31m'"$*"'\e[m'; }
Tgreen () { printf '\e[0;32m'"$*"'\e[m'; }

test_out=/tmp/test_out
diff="git diff"

for test_file in tests/*.decaf; do
  Normal "> Running a test for "
  Tgreen $test_file "\n"
  ./parser_program.native $test_file > $test_out
  $diff ${test_file}.expected $test_out
  if [ $? -eq 0 ]; then
    Tgreen "OK\n"
  else
    Tred "FAIL\n"
  fi

  echo
done

