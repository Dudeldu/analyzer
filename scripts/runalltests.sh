#!/bin/bash
echo "******************************************************************************"
echo "                                 UNIT-TESTS                                   "
echo "******************************************************************************"
./goblint.unittest -verbose
echo "******************************************************************************"
echo "                              REGRESSION-TESTS                                "
echo "******************************************************************************"
cd tests/regression
for x in `find * -name "*.c" | sort`
do
  echo "Testing $x"
  ../../goblint --debug $x
  echo "------------------------------------------------------------------------------"
done
