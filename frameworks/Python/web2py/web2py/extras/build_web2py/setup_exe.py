#!/usr/bin/env python
# -*- coding: utf-8 -*-

#Adapted from http://bazaar.launchpad.net/~flavour/sahana-eden/trunk/view/head:/static/scripts/tools/standalone_exe.py

USAGE = """
Usage:
    Copy this and setup_exe.conf to web2py root folder
    To build with py2exe:
        Install py2exe: http://sourceforge.net/projects/py2exe/files/
        run python setup_exe.py py2exe
    To build with bbfreeze:
        Install bbfreeze: https://pypi.python.org/pypi/bbfreeze/
        run python setup_exe.py bbfreeze
"""

from distutils.core import setup
from gluon.import_all import base_modules, contributed_modules
from gluon.fileutils import readlines_file
from glob import glob
import fnmatch
import os
import shutil
import sys
import re
import zipfile

if len(sys.argv) != 2 or not os.path.isfile('web2py.py'):
    print USAGE
    sys.exit(1)
BUILD_MODE = sys.argv[1]
if not BUILD_MODE in ('py2exe', 'bbfreeze'):
    print USAGE
    sys.exit(1)

def unzip(source_filename, dest_dir):
    with zipfile.ZipFile(source_filename) as zf:
        zf.extractall(dest_dir)

#borrowed from http://bytes.com/topic/python/answers/851018-how-zip-directory-python-using-zipfile
def recursive_zip(zipf, directory, folder=""):
    for item in os.listdir(directory):
        if os.path.isfile(os.path.join(directory, item)):
            zipf.write(os.path.join(directory, item), folder + os.sep + item)
        elif os.path.isdir(os.path.join(directory, item)):
            recursive_zip(
                zipf, os.path.join(directory, item), folder + os.sep + item)


#read web2py version from VERSION file
web2py_version_line = readlines_file('VERSION')[0]
#use regular expression to get just the version number
v_re = re.compile('[0-9]+\.[0-9]+\.[0-9]+')
web2py_version = v_re.search(web2py_version_line).group(0)

#pull in preferences from config file
import ConfigParser
Config = ConfigParser.ConfigParser()
Config.read('setup_exe.conf')
remove_msft_dlls = Config.getboolean("Setup", "remove_microsoft_dlls")
copy_apps = Config.getboolean("Setup", "copy_apps")
copy_site_packages = Config.getboolean("Setup", "copy_site_packages")
copy_scripts = Config.getboolean("Setup", "copy_scripts")
make_zip = Config.getboolean("Setup", "make_zip")
zip_filename = Config.get("Setup", "zip_filename")
remove_build_files = Config.getboolean("Setup", "remove_build_files")
include_gevent = Config.getboolean("Setup", "include_gevent")

# Python base version
python_version = sys.version_info[:3]



if BUILD_MODE == 'py2exe':
    import py2exe

    setup(
        console=[{'script':'web2py.py',
                  'icon_resources': [(0, 'extras/icons/web2py.ico')]
                  }],
        windows=[{'script':'web2py.py',
                  'icon_resources': [(1, 'extras/icons/web2py.ico')],
                  'dest_base':'web2py_no_console'  # MUST NOT be just 'web2py' otherwise it overrides the standard web2py.exe
                  }],
        name="web2py",
        version=web2py_version,
        description="web2py web framework",
        author="Massimo DiPierro",
        license="LGPL v3",
        data_files=[
            'ABOUT',
            'LICENSE',
            'VERSION'
        ],
        options={'py2exe': {
                 'packages': contributed_modules,
                 'includes': base_modules,
                 }},
    )
    #py2exe packages lots of duplicates in the library.zip, let's save some space
    library_temp_dir = os.path.join('dist', 'library_temp')
    library_zip_archive = os.path.join('dist', 'library.zip')
    os.makedirs(library_temp_dir)
    unzip(library_zip_archive, library_temp_dir)
    os.unlink(library_zip_archive)
    zipl = zipfile.ZipFile(library_zip_archive, "w", compression=zipfile.ZIP_DEFLATED)
    recursive_zip(zipl, library_temp_dir)
    zipl.close()
    shutil.rmtree(library_temp_dir)
    print "web2py binary successfully built"

elif BUILD_MODE == 'bbfreeze':
    modules = base_modules + contributed_modules
    from bbfreeze import Freezer
    f = Freezer(distdir="dist", includes=(modules))
    f.addScript("web2py.py")
    #to make executable without GUI we need this trick
    shutil.copy("web2py.py", "web2py_no_console.py")
    f.addScript("web2py_no_console.py", gui_only=True)
    if include_gevent:
        #fetch the gevented webserver script and copy to root
        gevented_webserver = os.path.join("handlers", "web2py_on_gevent.py")
        shutil.copy(gevented_webserver, "web2py_on_gevent.py")
        f.addScript("web2py_on_gevent.py")
    f.setIcon('extras/icons/web2py.ico')
    f()    # starts the freezing process
    os.unlink("web2py_no_console.py")
    if include_gevent:
        os.unlink("web2py_on_gevent.py")
    #add data_files
    for req in ['ABOUT', 'LICENSE', 'VERSION']:
        shutil.copy(req, os.path.join('dist', req))
    print "web2py binary successfully built"

try:
    os.unlink('storage.sqlite')
except:
    pass

#This need to happen after bbfreeze is run because Freezer() deletes distdir before starting!
if python_version > (2,5):
    # Python26 compatibility: http://www.py2exe.org/index.cgi/Tutorial#Step52
    try:
        shutil.copytree('C:\Bin\Microsoft.VC90.CRT', 'dist/Microsoft.VC90.CRT/')
    except:
        print "You MUST copy Microsoft.VC90.CRT folder into the archive"

def copy_folders(source, destination):
    """Copy files & folders from source to destination (within dist/)"""
    if os.path.exists(os.path.join('dist', destination)):
        shutil.rmtree(os.path.join('dist', destination))
    shutil.copytree(os.path.join(source), os.path.join('dist', destination))

#should we remove Windows OS dlls user is unlikely to be able to distribute
if remove_msft_dlls:
    print "Deleted Microsoft files not licensed for open source distribution"
    print "You are still responsible for making sure you have the rights to distribute any other included files!"
    #delete the API-MS-Win-Core DLLs
    for f in glob('dist/API-MS-Win-*.dll'):
        os.unlink(f)
    #then delete some other files belonging to Microsoft
    other_ms_files = ['KERNELBASE.dll', 'MPR.dll', 'MSWSOCK.dll',
                      'POWRPROF.dll']
    for f in other_ms_files:
        try:
            os.unlink(os.path.join('dist', f))
        except:
            print "unable to delete dist/" + f

#Should we include applications?
if copy_apps:
    copy_folders('applications', 'applications')
    print "Your application(s) have been added"
else:
    #only copy web2py's default applications
    copy_folders('applications/admin', 'applications/admin')
    copy_folders('applications/welcome', 'applications/welcome')
    copy_folders('applications/examples', 'applications/examples')
    print "Only web2py's admin, examples & welcome applications have been added"

copy_folders('extras', 'extras')
copy_folders('examples', 'examples')
copy_folders('handlers', 'handlers')


#should we copy project's site-packages into dist/site-packages
if copy_site_packages:
    #copy site-packages
    copy_folders('site-packages', 'site-packages')
else:
    #no worries, web2py will create the (empty) folder first run
    print "Skipping site-packages"

#should we copy project's scripts into dist/scripts
if copy_scripts:
    #copy scripts
    copy_folders('scripts', 'scripts')
else:
    #no worries, web2py will create the (empty) folder first run
    print "Skipping scripts"

#should we create a zip file of the build?
if make_zip:
    #create a web2py folder & copy dist's files into it
    shutil.copytree('dist', 'zip_temp/web2py')
    #create zip file
    zipf = zipfile.ZipFile(zip_filename + ".zip",
                            "w", compression=zipfile.ZIP_DEFLATED)
    # just temp so the web2py directory is included in our zip file
    path = 'zip_temp'
    # leave the first folder as None, as path is root.
    recursive_zip(zipf, path)
    zipf.close()
    shutil.rmtree('zip_temp')
    print "Your Windows binary version of web2py can be found in " + \
        zip_filename + ".zip"
    print "You may extract the archive anywhere and then run web2py/web2py.exe"

#should py2exe build files be removed?
if remove_build_files:
    if BUILD_MODE == 'py2exe':
        shutil.rmtree('build')
    shutil.rmtree('deposit')
    shutil.rmtree('dist')
    print "build files removed"

#final info
if not make_zip and not remove_build_files:
    print "Your Windows binary & associated files can also be found in /dist"

print "Finished!"
print "Enjoy web2py " + web2py_version_line
