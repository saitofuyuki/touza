import setuptools
setuptools.setup(
    packages=setuptools.find_packages(where='zbt'),
    package_dir={"": "zbt"},
    package_data={"samples": ["demo*.png"], },
)
