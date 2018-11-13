#!/bin/bash
USERBLANK="ca-user"

max=2
for i in `seq 1 $max`
do
        sudo su -c "userdel $USERBLANK$i"
done