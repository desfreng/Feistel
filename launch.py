import argparse
import os
import shutil

_DUNE = shutil.which("dune")

if _DUNE is None:
    raise RuntimeError("'dune' must be installed")

parser = argparse.ArgumentParser(
    description='Run and Build dune project via VSCode')
group = parser.add_mutually_exclusive_group()
group.add_argument('--build', action='store_true',
                   help='Build Dune project')
group.add_argument('--watch', action='store_true',
                   help='Watch Build Dune project')
group.add_argument('--utop', action='store_true',
                   help='run "utop" in project')
group.add_argument('--exec', type=str,
                   help='Exec dune target')

args = parser.parse_args()

if args.build:
    os.execvp(_DUNE, (_DUNE, "build"))

elif args.utop:
    if os.path.exists("./lib/"):
        os.execvp(_DUNE, (_DUNE, "utop", "lib", "--", "-implicit-bindings"))

elif args.watch:
    os.execvp(_DUNE, (_DUNE, "build", "--watch"))

elif args.exec is not None:
    os.execvp(_DUNE, (_DUNE, "exec", args.exec))

else:
    os.execvp(_DUNE, (_DUNE, "build"))
