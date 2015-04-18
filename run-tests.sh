#!/bin/bash


Normal () { printf '\e[m'"$*"; }
Tred () { printf '\e[0;31m'"$*"'\e[m'; }
Tgreen () { printf '\e[0;32m'"$*"'\e[m'; }

test_out=/tmp/test_out
test_err=/tmp/test_err
diff="git diff"

RunTest () {
  test_runner=$1
  test=$2
  Normal "> Running a test for "
  Tgreen $test "\n"
  $test_runner $test > $test_out 2> $test_err
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
    if [ -f ${test}.failure ]; then
      $diff ${test}.failure $test_err
      if [ $? -eq 0 ]; then
        Tgreen "OK\n"
      else
        Tred "FAIL\n"
      fi
      continue
    fi

    Tred "Program Failed\n\n"
    echo "Exited with exit code:" $exit_code
    echo "STDERR: >>>"
    cat $test_err
    echo "STDERR: <<<"
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

for test_file in tests/lexer/*.decaf; do
  RunTest "./lexer_program.native" $test_file
done

for test_file in tests/analysis/*.decaf; do
  RunTest "./analysis_program.native" $test_file
done

