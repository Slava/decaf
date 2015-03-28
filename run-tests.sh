#!/bin/bash


Normal () { printf '\e[m'"$*"; }
Tred () { printf '\e[0;31m'"$*"'\e[m'; }
Tgreen () { printf '\e[0;32m'"$*"'\e[m'; }

test_out=/tmp/test_out
diff="git diff"

RunTest () {
  test_runner=$1
  test=$2
  Normal "> Running a test for "
  Tgreen $test "\n"
  $test_runner $test > $test_out
  if [ $? -ne 0 ]; then
    Tred "Comp Error\n\n"
    continue
  fi

  $diff ${test}.expected $test_out
  if [ $? -eq 0 ]; then
    Tgreen "OK\n"
  else
    Tred "FAIL\n"
  fi

  echo
}

for test_file in tests/parser/*.decaf; do
  RunTest "./parser_program.native" $test_file
done

