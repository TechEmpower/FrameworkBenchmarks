echo off
echo -
echo #下载源代码
echo mvn dependency:sources
echo -

echo #下载源代码jar。 -DdownloadJavadocs=true 下载javadoc包
echo -DdownloadSources=true
echo -
echo -



echo #将jar解压出来
echo mvn dependency:unpack-dependencies
echo -

echo #将jar拷贝到某一目录中(所有jar在同一目录中)
echo mvn dependency:copy-dependencies -Dmdep.useRepositoryLayout=false
echo -

echo #将jar按仓库目录拷贝出来()
echo mvn dependency:copy-dependencies -Dmdep.useRepositoryLayout=true -Dmdep.copyPom=true
echo -
echo -



echo #检查版本更新
echo mvn versions:display-dependency-updates
echo -

echo #版本变更
echo mvn versions:set -DnewVersion=4.0.0-talent-999
echo -


call cmd