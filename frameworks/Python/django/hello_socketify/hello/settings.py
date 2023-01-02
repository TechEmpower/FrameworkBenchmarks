import os

DEBUG = False

SECRET_KEY = '_7mb6#v4yf@qhc(r(zbyh&amp;z_iby-na*7wz&amp;-v6pohsul-d#y5f'
ADMINS = ()

MANAGERS = ADMINS

DATABASES = {}

TIME_ZONE = 'America/Chicago'
LANGUAGE_CODE = 'en-us'
USE_I18N = False
USE_L10N = False
USE_TZ = False

MEDIA_ROOT = ''
MEDIA_URL = ''
STATIC_ROOT = ''
STATIC_URL = '/static/'
STATICFILES_DIRS = ()
STATICFILES_FINDERS = ()
MIDDLEWARE = ()

ROOT_URLCONF = 'hello_socketify.hello.urls'
WSGI_APPLICATION = 'hello_socketify.hello.wsgi.application'

TEMPLATES = [
    {
        'BACKEND': 'django.template.backends.django.DjangoTemplates',
        'DIRS': [],
        'APP_DIRS': True,
        'OPTIONS': {},
    },
]

INSTALLED_APPS = (
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'hello_socketify.world',
)

LOGGING = {
    'version': 1,
    'disable_existing_loggers': True,
    'handlers': {},
    'loggers': {},

}

ALLOWED_HOSTS = ['*']
