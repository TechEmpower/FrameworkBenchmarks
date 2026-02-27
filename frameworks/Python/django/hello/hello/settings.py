import os

DEBUG = False

SECRET_KEY = "_7mb6#v4yf@qhc(r(zbyh&amp;z_iby-na*7wz&amp;-v6pohsul-d#y5f"
ADMINS = ()

MANAGERS = ADMINS

_django_db = os.getenv("DJANGO_DB", "")

DATABASES = {
    "default": {
        "ENGINE": "django.db.backends." + _django_db,
        "NAME": "hello_world",
        "USER": "benchmarkdbuser",
        "PASSWORD": "benchmarkdbpass",
        "HOST": "tfb-database",
        "PORT": "",
        "CONN_MAX_AGE": None,  # Persistent connections per worker
    }
}

if not _django_db:
    DATABASES = {}

TIME_ZONE = "America/Chicago"
LANGUAGE_CODE = "en-us"
USE_I18N = False
USE_L10N = False
USE_TZ = False

MEDIA_ROOT = ""
MEDIA_URL = ""
STATIC_ROOT = ""
STATIC_URL = "/static/"
STATICFILES_DIRS = ()
STATICFILES_FINDERS = ()
MIDDLEWARE = ()

ROOT_URLCONF = "hello.urls"
WSGI_APPLICATION = "hello.wsgi.application"

TEMPLATES = [
    {
        "BACKEND": "django.template.backends.django.DjangoTemplates",
        "DIRS": [],
        "APP_DIRS": False,
        "OPTIONS": {
            "loaders": [
                ("django.template.loaders.cached.Loader", [
                    "django.template.loaders.app_directories.Loader",
                ]),
            ],
        },
    },
]

INSTALLED_APPS = (
    "world",
)

LOGGING = {
    "version": 1,
    "disable_existing_loggers": True,
    "handlers": {},
    "loggers": {},
}

ALLOWED_HOSTS = ["*"]
