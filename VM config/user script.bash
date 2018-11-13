#!/bin/bash
USERBLANK="ca-user"

max=2
for i in `seq 1 $max`
do
        sudo su -c "useradd $USERBLANK$i -s /bin/bash -m -g 100"
done

for i in `seq 1 $max`
do
    echo "$USERBLANK$i:iloveca" | sudo chpasswd
done
