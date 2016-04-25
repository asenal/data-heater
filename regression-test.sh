## This is a BASH script to run regression test against data-heater module, by David.
## $1 will first split a input tab file into chunks with record count from  10000, 20000, .. 50x10000
## $1 will then call data-heater.test.lisp script to run the test , xargs will make sure the job queue scheduled.
## after all test cases return, $1 will collect performance information from each case's timer log, format into a table
## someone will do the dirty work, visualize the table as a graph.



##1. prepare the data for regression test
#for i in {1...50}; do head -n $(($i*10000)) /data/data12/zhengxin_pro/Heater/DATA/chezhu_info.csv > /tmp/chezhu_info.$(($i*10000)) ;done 

##2. wrap the test code in a cmdline script, which take one input datafile and start a AG connection to run the test;
##   use XARGS to manage the job queue, at most 10 tests are running at the same time.
ls /tmp/chezhu_info.*0000 |xargs -n 1 -P 10 -i ./data-heater.test.lisp {} {}.log {}.csv

#3. each test will make its own output, after 50 tests are done, we collect information from 50 output logs and format as a table
for i in /tmp/chezhu_info.*.log;do egrep '[0-9\., ]+?(Lines|sec|cons)' -o $i | sed 's/[ ,Linesconsec]//g' |tr '\n' '\t' && echo "null"; done |sort -n > regression-test.report
