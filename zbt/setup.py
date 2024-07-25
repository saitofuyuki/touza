import subprocess
from setuptools import setup
from setuptools.command.sdist import sdist

class CustomSdist(sdist):
    def run(self):
        subprocess.call(["pwd"])
        subprocess.call(["./param.sh",
                         "-o", "zbt/param.py",
                         "ParamTouzaNio", "../nio/touza_nio_param.h", ])
        super().run()

setup(
   packages=[],
   cmdclass={'sdist': CustomSdist, },
)
