RANGE=10000
for i in {1..10000}
do
    value=$RANDOM
    let "value = (value % $RANGE) + 1"
    echo "SET world:$i $value" | redis-cli > /dev/null
done

echo "DEL fortunes" | redis-cli
echo "RPUSH fortunes 'fortune: No such file or directory' \
\"A computer scientist is someone who fixes things that aren''t broken.\" \
'After enough decimal places, nobody gives a damn.' \
'A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1' \
'A computer program does what you tell it to do, not what you want it to do.' \
'Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen' \
'Any program that runs right is obsolete.' \
'A list is only as strong as its weakest link. — Donald Knuth' \
'Feature: A bug with seniority.' \
'Computers make very fast, very accurate mistakes.' \
'<script>alert(\"This should not be displayed in a browser alert box.\");</script>' \
'フレームワークのベンチマーク'" | redis-cli
