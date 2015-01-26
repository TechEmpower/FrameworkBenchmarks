RANGE=10000
for i in {1..10000}
do
    value=$RANDOM
    let "value = (value % $RANGE) + 1"
    echo "SET world:$i $value" | redis-cli > /dev/null
done

echo "DEL fortunes" | redis-cli

# Please don't compress these into one statement, TFB is not always
# run using Redis 2.4+ and we don't need optimization when 
# adding ~10 entries
echo "RPUSH fortunes \"fortune: No such file or directory\"" | redis-cli
echo "RPUSH fortunes \"A computer scientist is someone who fixes things that aren't broken.\"" | redis-cli
echo "RPUSH fortunes \"After enough decimal places, nobody gives a damn.\"" | redis-cli
echo "RPUSH fortunes \"A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1\"" | redis-cli
echo "RPUSH fortunes \"A computer program does what you tell it to do, not what you want it to do.\"" | redis-cli
echo "RPUSH fortunes \"Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen\"" | redis-cli
echo "RPUSH fortunes \"Any program that runs right is obsolete.\"" | redis-cli
echo "RPUSH fortunes \"A list is only as strong as its weakest link. — Donald Knuth\"" | redis-cli
echo "RPUSH fortunes \"Feature: A bug with seniority.\"" | redis-cli
echo "RPUSH fortunes \"Computers make very fast, very accurate mistakes.\"" | redis-cli
echo "RPUSH fortunes \"<script>alert(\\\"This should not be displayed in a browser alert box.\\\");</script>\"" | redis-cli
echo "RPUSH fortunes \"フレームワークのベンチマーク\"" | redis-cli
