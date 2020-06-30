<?php
/**
 * This file is part of webman.
 *
 * Licensed under The MIT License
 * For full copyright and license information, please see the MIT-LICENSE.txt
 * Redistributions of files must retain the above copyright notice.
 *
 * @author    walkor<walkor@workerman.net>
 * @copyright walkor<walkor@workerman.net>
 * @link      http://www.workerman.net/
 * @license   http://www.opensource.org/licenses/mit-license.php MIT License
 */
namespace support\bootstrap;

use Webman\Bootstrap;
use Illuminate\Redis\RedisManager;

/**
 * Class Redis
 * @package support
 *
 * Strings methods
 * @method static int append($key, $value)
 * @method static int bitCount($key)
 * @method static int decr($key, $value)
 * @method static int decrBy($key, $value)
 * @method static string|bool get($key)
 * @method static int getBit($key, $offset)
 * @method static string getRange($key, $start, $end)
 * @method static string getSet($key, $value)
 * @method static int incr($key, $value)
 * @method static int incrBy($key, $value)
 * @method static float incrByFloat($key, $value)
 * @method static array mGet(array $keys)
 * @method static array getMultiple(array $keys)
 * @method static bool mSet($pairs)
 * @method static bool mSetNx($pairs)
 * @method static bool set($key, $val, $timeout = null)
 * @method static bool setBit($key, $offset, $value)
 * @method static bool setEx($key, $ttl, $value)
 * @method static bool pSetEx($key, $ttl, $value)
 * @method static bool setNx($key, $value)
 * @method static string setRange($key, $offset, $value)
 * @method static int strLen($key)
 * Keys methods
 * @method static int del(...$keys)
 * @method static int unlink(...$keys)
 * @method static false|string dump($key)
 * @method static int exists(...$keys)
 * @method static bool expire($key, $ttl)
 * @method static bool pexpire($key, $ttl)
 * @method static bool expireAt($key, $timestamp)
 * @method static bool pexpireAt($key, $timestamp)
 * @method static array keys($pattern)
 * @method static bool|array scan($it)
 * @method static void migrate($host, $port, $keys, $dbIndex, $timeout, $copy = false, $replace = false)
 * @method static bool move($key, $dbIndex)
 * @method static string|int|bool object($information, $key)
 * @method static bool persist($key)
 * @method static string randomKey()
 * @method static bool rename($srcKey, $dstKey)
 * @method static bool renameNx($srcKey, $dstKey)
 * @method static string type($key)
 * @method static int|array sort($key, $options = [])
 * @method static int ttl($key)
 * @method static int pttl($key)
 * @method static void restore($key, $ttl, $value)
 * Hashes methods
 * @method static false|int hSet($key, $hashKey, $value)
 * @method static bool hSetNx($key, $hashKey, $value)
 * @method static false|string hGet($key, $hashKey)
 * @method static false|int hLen($key)
 * @method static false|int hDel($key, ...$hashKeys)
 * @method static array hKeys($key)
 * @method static array hVals($key)
 * @method static array hGetAll($key)
 * @method static bool hExists($key, $hashKey)
 * @method static int hIncrBy($key, $hashKey, $value)
 * @method static float hIncrByFloat($key, $hashKey, $value)
 * @method static bool hMSet($key, $members)
 * @method static array hMGet($key, $memberKeys)
 * @method static array hScan($key, $iterator, $pattern = '', $count = 0)
 * @method static int hStrLen($key, $hashKey)
 * Lists methods
 * @method static array blPop($keys, $timeout)
 * @method static array brPop($keys, $timeout)
 * @method static false|string bRPopLPush($srcKey, $dstKey, $timeout)
 * @method static false|string lIndex($key, $index)
 * @method static int lInsert($key, $position, $pivot, $value)
 * @method static false|string lPop($key)
 * @method static false|int lPush($key, ...$entries)
 * @method static false|int lPushx($key, $value)
 * @method static array lRange($key, $start, $end)
 * @method static false|int lRem($key, $value, $count)
 * @method static bool lSet($key, $index, $value)
 * @method static false|array lTrim($key, $start, $end)
 * @method static false|string rPop($key)
 * @method static false|string rPopLPush($srcKey, $dstKey)
 * @method static false|int rPush($key, ...$entries)
 * @method static false|int rPushX($key, $value)
 * @method static false|int lLen($key)
 * Sets methods
 * @method static int sAdd($key, $value)
 * @method static int sCard($key)
 * @method static array sDiff($keys)
 * @method static false|int sDiffStore($dst, $keys)
 * @method static false|array sInter($keys)
 * @method static false|int sInterStore($dst, $keys)
 * @method static bool sIsMember($key, $member)
 * @method static array sMembers($key)
 * @method static bool sMove($src, $dst, $member)
 * @method static false|string|array sPop($key, $count = 0)
 * @method static false|string|array sRandMember($key, $count = 0)
 * @method static int sRem($key, ...$members)
 * @method static array sUnion(...$keys)
 * @method static false|int sUnionStore($dst, ...$keys)
 * @method static false|array sScan($key, $iterator, $pattern = '', $count = 0)
 * Sorted sets methods
 * @method static array bzPopMin($keys, $timeout)
 * @method static array bzPopMax($keys, $timeout)
 * @method static int zAdd($key, $score, $value)
 * @method static int zCard($key)
 * @method static int zCount($key, $start, $end)
 * @method static double zIncrBy($key, $value, $member)
 * @method static int zinterstore($keyOutput, $arrayZSetKeys, $arrayWeights = [], $aggregateFunction = '')
 * @method static array zPopMin($key, $count)
 * @method static array zPopMax($key, $count)
 * @method static array zRange($key, $start, $end, $withScores = false)
 * @method static array zRangeByScore($key, $start, $end, $options = [])
 * @method static array zRevRangeByScore($key, $start, $end, $options = [])
 * @method static array zRangeByLex($key, $min, $max, $offset = 0, $limit = 0)
 * @method static int zRank($key, $member)
 * @method static int zRevRank($key, $member)
 * @method static int zRem($key, ...$members)
 * @method static int zRemRangeByRank($key, $start, $end)
 * @method static int zRemRangeByScore($key, $start, $end)
 * @method static array zRevRange($key, $start, $end, $withScores = false)
 * @method static double zScore($key, $member)
 * @method static int zunionstore($keyOutput, $arrayZSetKeys, $arrayWeights = [], $aggregateFunction = '')
 * @method static false|array zScan($key, $iterator, $pattern = '', $count = 0)
 * HyperLogLogs methods
 * @method static int pfAdd($key, $values)
 * @method static int pfCount($keys)
 * @method static bool pfMerge($dstKey, $srcKeys)
 * Geocoding methods
 * @method static int geoAdd($key, $longitude, $latitude, $member, ...$items)
 * @method static array geoHash($key, ...$members)
 * @method static array geoPos($key, ...$members)
 * @method static double geoDist($key, $members, $unit = '')
 * @method static int|array geoRadius($key, $longitude, $latitude, $radius, $unit, $options = [])
 * @method static array geoRadiusByMember($key, $member, $radius, $units, $options = [])
 * Streams methods
 * @method static int xAck($stream, $group, $arrMessages)
 * @method static string xAdd($strKey, $strId, $arrMessage, $iMaxLen = 0, $booApproximate = false)
 * @method static array xClaim($strKey, $strGroup, $strConsumer, $minIdleTime, $arrIds, $arrOptions = [])
 * @method static int xDel($strKey, $arrIds)
 * @method static mixed xGroup($command, $strKey, $strGroup, $strMsgId, $booMKStream = null)
 * @method static mixed xInfo($command, $strStream, $strGroup = null)
 * @method static int xLen($stream)
 * @method static array xPending($strStream, $strGroup, $strStart = 0, $strEnd = 0, $iCount = 0, $strConsumer = null)
 * @method static array xRange($strStream, $strStart, $strEnd, $iCount = 0)
 * @method static array xRead($arrStreams, $iCount = 0, $iBlock = null)
 * @method static array xReadGroup($strGroup, $strConsumer, $arrStreams, $iCount = 0, $iBlock = null)
 * @method static array xRevRange($strStream, $strEnd, $strStart, $iCount = 0)
 * @method static int xTrim($strStream, $iMaxLen, $booApproximate = null)
 * Pub/sub methods
 * @method static mixed pSubscribe($patterns, $callback)
 * @method static mixed publish($channel, $message)
 * @method static mixed subscribe($channels, $callback)
 * @method static mixed pubSub($keyword, $argument = null)
 * Generic methods
 * @method static mixed rawCommand(...$commandAndArgs)
 * Transactions methods
 * @method static \Redis multi()
 * @method static mixed exec()
 * @method static mixed discard()
 * @method static mixed watch($keys)
 * @method static mixed unwatch($keys)
 * Scripting methods
 * @method static mixed eval($script, $args = [], $numKeys = 0)
 * @method static mixed evalSha($sha, $args = [], $numKeys = 0)
 * @method static mixed script($command, ...$scripts)
 * @method static mixed client(...$args)
 * @method static null|string getLastError()
 * @method static bool clearLastError()
 * @method static mixed _prefix($value)
 * @method static mixed _serialize($value)
 * @method static mixed _unserialize($value)
 * Introspection methods
 * @method static bool isConnected()
 * @method static mixed getHost()
 * @method static mixed getPort()
 * @method static false|int getDbNum()
 * @method static false|double getTimeout()
 * @method static mixed getReadTimeout()
 * @method static mixed getPersistentID()
 * @method static mixed getAuth()
 */
class Redis implements Bootstrap {

    /**
     * @var RedisManager
     */
    protected static $_manager = null;

    /**
     * @param \Workerman\Worker $worker
     * @return void
     */
    public static function start($worker)
    {
        if (!class_exists('\Illuminate\Redis\RedisManager')) {
            return;
        }
        $config = config('redis');
        static::$_manager = new RedisManager('', 'phpredis', $config);
    }

    /**
     * @param string $name
     * @return \Illuminate\Redis\Connections\Connection
     */
    public static function connection($name = 'default') {
        return static::$_manager->connection($name);
    }

    /**
     * @param $name
     * @param $arguments
     * @return mixed
     */
    public static function __callStatic($name, $arguments)
    {
        return static::$_manager->connection('default')->{$name}(... $arguments);
    }
}