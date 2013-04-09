<?php
/*
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * This software consists of voluntary contributions made by many individuals
 * and is licensed under the LGPL. For more information, see
 * <http://www.doctrine-project.org>.
 */

/**
 * Install PHP extensions required for testing by Travis CI.
 *
 * @author Victor Berchet <victor@suumit.com>
 * @since 2.2
 */
$installer = new PhpExtensions();

if (isset($argv[1]) && 'APC' === strtoupper($argv[1])) {
    $installer->install('apc');
} else {
    $installer->install('xcache');
}

$installer->install('memcache');
$installer->install('memcached');

class PhpExtensions
{
    protected $extensions;
    protected $phpVersion;
    protected $iniPath;

    public function __construct()
    {
        $this->phpVersion = phpversion();
        $this->iniPath = php_ini_loaded_file();
        $this->extensions = array(
        'memcache' => array(
            'url'        => 'http://pecl.php.net/get/memcache-2.2.6.tgz',
            'php_version' => array(),
            'cfg'         => array('--enable-memcache'),
            'ini'         => array('extension=memcache.so'),
        ),
        'memcached' => array(
            'url'        => 'http://pecl.php.net/get/memcached-1.0.2.tgz',
            'php_version' => array(
                // memcached 1.0.2 does not build on PHP 5.4
                array('<', '5.4'),
            ),
            'cfg'         => array(),
            'ini'         => array('extension=memcached.so'),
        ),
        'apc' => array(
            'url'        => 'http://pecl.php.net/get/APC-3.1.9.tgz',
            'php_version' => array(
                // apc 3.1.9 causes a segfault on PHP 5.4
                array('<', '5.4'),
            ),
            'cfg'         => array(),
            'ini'         => array(
                'extension=apc.so',
                'apc.enabled=1',
                'apc.enable_cli=1'
            ),
        ),
        'xcache' => array(
            'url'        => 'http://xcache.lighttpd.net/pub/Releases/1.2.2/xcache-1.2.2.tar.gz',
            'php_version' => array(
                // xcache does not build with Travis CI (as of 2012-01-09)
                array('<', '5'),
            ),
            'cfg'         => array('--enable-xcache'),
            'ini'         => array(
                'extension=xcache.so',
                'xcache.cacher=false',
                'xcache.admin.enable_auth=0',
                'xcache.var_size=1M',
            ),
        ),
    );
    }

    public function install($name)
    {
        if (array_key_exists($name, $this->extensions)) {
            $extension = $this->extensions[$name];


            echo "== extension: $name ==\n";

            foreach ($extension['php_version'] as $version) {
                if (!version_compare($this->phpVersion, $version[1], $version[0])) {
                    printf(
                        "=> not installed, requires a PHP version %s %s (%s installed)\n",
                        $version[0],
                        $version[1],
                        $this->phpVersion
                    );

                    return;
                }
            }

            $this->system(sprintf("wget %s > /dev/null 2>&1", $extension['url']));
            $file = basename($extension['url']);
            $this->system(sprintf("tar -xzf %s > /dev/null 2>&1", $file));
            $folder = basename($file, ".tgz");
            $folder = basename($folder, ".tar.gz");
            $this->system(sprintf(
                'sh -c "cd %s && phpize && ./configure %s && make && sudo make install" > /dev/null 2>&1',
                $folder,
                implode(' ', $extension['cfg'])
            ));
            foreach ($extension['ini'] as $ini) {
                $this->system(sprintf("echo %s >> %s", $ini, $this->iniPath));
            }
            printf("=> installed (%s)\n", $folder);
        }
    }

    private function system($cmd)
    {
        $ret = 0;
        system($cmd, $ret);
        if (0 !== $ret) {
            printf("=> Command '%s' failed !", $cmd);
            
            exit($ret);
        }
    }
}
