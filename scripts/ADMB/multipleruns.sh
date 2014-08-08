#/bin/bash
tplname=$1
for f in *.dat ; do
B=$(basename "$f" .dat)
mkdir $B
cp $f $B
cp $tplname "$B/$B.tpl"
cd $B
admb $B
./$B
cd ..
done
# rm $tpname