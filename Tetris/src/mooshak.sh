#!/bin/bash
echo '(load "utilsaux.lisp")' > utl
cat  tetris.lisp utl ../tests/test01/input > out01
cat  tetris.lisp utl ../tests/test02/input > out02
cat  tetris.lisp utl ../tests/test03/input > out03
cat  tetris.lisp utl ../tests/test04/input > out04
cat  tetris.lisp utl ../tests/test05/input > out05
cat  tetris.lisp utl ../tests/test06/input > out06
cat  tetris.lisp utl ../tests/test07/input > out07
cat  tetris.lisp utl ../tests/test08/input > out08
cat  tetris.lisp utl ../tests/test09/input > out09
cat  tetris.lisp utl ../tests/test10/input > out10
cat  tetris.lisp utl ../tests/test11/input > out11
cat  tetris.lisp utl ../tests/test12/input > out12
cat  tetris.lisp utl ../tests/test14/input > out14
cat  tetris.lisp utl ../tests/test19/input > out19
cat  tetris.lisp utl ../tests/test22/input > out22
cat  tetris.lisp utl ../tests/test23/input > out23
cat  tetris.lisp utl ../tests/test24/input > out24

clisp < out01 > tst
echo 'TEST 1'
sed -n '/^T$/ { s///; :a; n; p; ba; }' tst > tst1
diff tst1 ../customtests2/test01/output

clisp < out02 > tst
echo 'TEST 2'
sed -n '/^T$/ { s///; :a; n; p; ba; }' tst > tst1
diff tst1 ../customtests2/test02/output

clisp < out03 > tst
echo 'TEST 3'
sed -n '/^T$/ { s///; :a; n; p; ba; }' tst > tst1
diff tst1 ../customtests2/test03/output

clisp < out04 > tst
echo 'TEST 4'
sed -n '/^T$/ { s///; :a; n; p; ba; }' tst > tst1
diff tst1 ../customtests2/test04/output

clisp < out05 > tst
echo 'TEST 5'
sed -n '/^T$/ { s///; :a; n; p; ba; }' tst > tst1
diff tst1 ../customtests2/test05/output

clisp < out06 > tst
echo 'TEST 6'
sed -n '/^T$/ { s///; :a; n; p; ba; }' tst > tst1
diff tst1 ../customtests2/test06/output

clisp < out07 > tst
echo 'TEST 7'
sed -n '/^T$/ { s///; :a; n; p; ba; }' tst > tst1
diff tst1 ../customtests2/test07/output

clisp < out08 > tst
echo 'TEST 8'
sed -n '/^T$/ { s///; :a; n; p; ba; }' tst > tst1
diff tst1 ../customtests2/test08/output

clisp < out09 > tst
echo 'TEST 9'
sed -n '/^T$/ { s///; :a; n; p; ba; }' tst > tst1
diff tst1 ../customtests2/test09/output

clisp < out10 > tst
echo 'TEST 10'
sed -n '/^T$/ { s///; :a; n; p; ba; }' tst > tst1
diff tst1 ../customtests2/test10/output

clisp < out11 > tst
echo 'TEST 11'
sed -n '/^T$/ { s///; :a; n; p; ba; }' tst > tst1
diff tst1 ../customtests2/test11/output

clisp < out12 > tst
echo 'TEST 12'
sed -n '/^T$/ { s///; :a; n; p; ba; }' tst > tst1
diff tst1 ../customtests2/test12/output

clisp < out14 > tst
echo 'TEST 14'
sed -n '/^T$/ { s///; :a; n; p; ba; }' tst > tst1
diff tst1 ../customtests2/test14/output

clisp < out19 > tst
echo 'TEST 19'
sed -n '/^T$/ { s///; :a; n; p; ba; }' tst > tst1
diff tst1 ../customtests2/test19/output

clisp < out22 > tst
echo 'TEST 22'
sed -n '/^T$/ { s///; :a; n; p; ba; }' tst > tst1
diff tst1 ../customtests2/test22/output

clisp < out23 > tst
echo 'TEST 23'
sed -n '/^T$/ { s///; :a; n; p; ba; }' tst > tst1
diff tst1 ../customtests2/test23/output

clisp < out24 > tst
echo 'TEST 24'
sed -n '/^T$/ { s///; :a; n; p; ba; }' tst > tst1
diff tst1 ../customtests2/test24/output

rm tst1 out* tst utl
