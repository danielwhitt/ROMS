#!/bin/bash
stra='      1  0  1  1    0.01d0    2.0d0   '
strb='.d0   0.d0     0.0d0   0.0d0   0.0d0'
strgap='.d0  '
for j in `seq 80 150`;
do
for i in `seq 1 400`;
do
strout="$stra$i$strgap$j$strb"
echo $strout >> fltsbio400many.in
done
done
