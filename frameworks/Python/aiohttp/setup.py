from setuptools import setup, Extension
from Cython.Build import cythonize

setup(
    ext_modules=cythonize([
        Extension(
            "app.random_utils",
            ["app/random_utils.pyx"],
            extra_compile_args=["-O3"]
        ),
    ])
)
