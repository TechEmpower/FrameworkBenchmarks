# Creating a New Application

[!!] The following examples assume that your web server is already set up, and you are going to create a new application at <http://localhost/gitorial/>.

Using your console, change to the empty directory `gitorial` and run `git init`. This will create the bare structure for a new git repository.

Next, we will create a [submodule](http://www.kernel.org/pub/software/scm/git/docs/git-submodule.html) for the `system` directory. Go to <http://github.com/kohana/core> and copy the "Clone URL":

![Github Clone URL](http://img.skitch.com/20091019-rud5mmqbf776jwua6hx9nm1n.png)

Now use the URL to create the submodule for `system`:

    git submodule add git://github.com/kohana/core.git system

[!!] This will create a link to the current development version of the next stable release. The development version should almost always be safe to use, have the same API as the current stable download with bugfixes applied.

Now add whatever submodules you need. For example, if you need the [Database] module:

    git submodule add git://github.com/kohana/database.git modules/database

After submodules are added, they must be initialized:

    git submodule init

Now that the submodules are added, you can commit them:

    git commit -m 'Added initial submodules'

Next, create the application directory structure. This is the bare minimum required:

    mkdir -p application/classes/{Controller,Model}
    mkdir -p application/{config,views}
    mkdir -m 0777 -p application/{cache,logs}

If you run `find application` you should see this:

    application
    application/cache
    application/config
    application/classes
    application/classes/Controller
    application/classes/Model
    application/logs
    application/views

We don't want git to track log or cache files, so add a `.gitignore` file to each of the directories. This will ignore all non-hidden files:

    echo '[^.]*' > application/{logs,cache}/.gitignore

[!!] Git ignores empty directories, so adding a `.gitignore` file also makes sure that git will track the directory, but not the files within it.

Now we need the `index.php` and `bootstrap.php` files:

    wget https://github.com/kohana/kohana/raw/3.3/master/index.php --no-check-certificate
    wget https://github.com/kohana/kohana/raw/3.3/master/application/bootstrap.php --no-check-certificate -O application/bootstrap.php

Commit these changes too:

    git add application
    git commit -m 'Added initial directory structure'

That's all there is to it. You now have an application that is using Git for versioning.

## Adding Submodules
To add a new submodule complete the following steps:

1. run the following code - git submodule add repository path for each new submodule e.g.:

        git submodule add git://github.com/shadowhand/sprig.git modules/sprig

2. then init and update the submodules:

        git submodule init
        git submodule update

## Updating Submodules

At some point you will probably also want to upgrade your submodules. To update all of your submodules to the latest `HEAD` version:

    git submodule foreach 'git checkout 3.3/master && git pull origin 3.3/master'

To update a single submodule, for example, `system`:

    cd system
    git checkout 3.3/master
    git pull origin 3.3/master
    cd ..
    git add system
    git commit -m 'Updated system to latest version'

If you want to update a single submodule to a specific commit:

    cd modules/database
    git pull origin 3.3/master
    git checkout fbfdea919028b951c23c3d99d2bc1f5bbeda0c0b
    cd ../..
    git add database
    git commit -m 'Updated database module'

Note that you can also check out the commit at a tagged official release point, for example:

    git checkout v3.3.0

Simply run `git tag` without arguments to get a list of all tags.

## Removing Submodules
To remove a submodule that is no longer needed complete the following steps:

1. open .gitmodules and remove the reference to the to submodule
    It will look something like this:

        [submodule "modules/auth"]
        path = modules/auth
        url = git://github.com/kohana/auth.git

2. open .git/config and remove the reference to the to submodule\\

        [submodule "modules/auth"]
        url = git://github.com/kohana/auth.git

3. run git rm --cached path/to/submodule, e.g.

        git rm --cached modules/auth

**Note:** Do not put a trailing slash at the end of path. If you put a trailing slash at the end of the command, it will fail.

## Updating Remote Repository URL

During the development of a project, the source of a submodule may change for any reason (you've created your own fork, the server URL changed, the repository name or path changed, etc...) and you'll have to update those changes. To do so, you'll need to perform the following steps:

1. edit the .gitmodules file, and change the URL for the submodules which changed.

2. in your source tree's root run:

		git submodule sync

3. run `git init` to update the project's repository configuration with the new URLs:

		git submodule init

And it's done, now you can continue pushing and pulling your submodules with no problems.

Source: http://jtrancas.wordpress.com/2011/02/06/git-submodule-location/