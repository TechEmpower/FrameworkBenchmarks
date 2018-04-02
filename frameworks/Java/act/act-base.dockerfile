FROM techempower/maven-java8:0.1

RUN apt-get install -y unzip
ADD ./ /act
WORKDIR /act

#
# This part isn't strictly necessary.  Its purpose is to download everything
# actframework might need from Maven ONCE rather than repeating the downloads
# for each actframework permutation.
#
# An even more ideal solution would be to share the .m2 directory with the host
# machine across all frameworks, but this doesn't seem to be possible right now.
# See: https://github.com/moby/moby/issues/14080
#
RUN mvn package clean -P mongo,ebean_mysql,hibernate_mysql,eclipselink_mysql,ebean_pgsql,hibernate_pgsql,eclipselink_pgsql
