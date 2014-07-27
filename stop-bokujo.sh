# kill screen and type the following
ps ax | grep bash | grep '?' | awk '{print "kill -9 " $1}' | sh
killall nodejs

