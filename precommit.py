#!/usr/bin/env python3
"""Run precommit checks on the repository."""
import pathlib
import subprocess


def main() -> None:
    """"Execute the main routine."""

    root_dir = pathlib.Path(__file__).parent
    source_dir = root_dir / "src"

    if subprocess.call(['which', 'elm-format'], stdout=subprocess.DEVNULL) != 0:
        raise FileNotFoundError("elm-format (https://github.com/avh4/elm-format) not found.")
    if subprocess.call(['which', 'elm-analyse'], stdout=subprocess.DEVNULL) != 0:
        raise FileNotFoundError("elm-analyse (https://stil4m.github.io/elm-analyse/) not found.")
    if subprocess.call(['which', 'elm-docstyle'], stdout=subprocess.DEVNULL) != 0:
        raise FileNotFoundError("elm-docstyle (https://www.npmjs.com/package/elm-docstyle) not found.")
    if subprocess.call(['which', 'elm-doc-test'], stdout=subprocess.DEVNULL) != 0:
        raise FileNotFoundError("elm-doc-test (https://www.npmjs.com/package/elm-doc-test) not found.")
    if subprocess.call(['which', 'elm-test'], stdout=subprocess.DEVNULL) != 0:
        raise FileNotFoundError("elm-test (https://www.npmjs.com/package/elm-test) not found.")

    print("elm-format'ing...")
    subprocess.check_call(['elm-format', '--elm-version=0.18', source_dir.as_posix(), '--yes'])

    print("elm-analyse'ing...")
    subprocess.check_call(['elm-analyse'], cwd=root_dir.as_posix())

    print("elm-docstyle'ing...")
    subprocess.check_call(['elm-docstyle', '.',
                          '--config_path', 'elm-docstyle.json',
                          '--check_all'], cwd=root_dir.as_posix())

    print("elm-doc-test generating...")
    subprocess.check_call(['elm-doc-test'], cwd=root_dir.as_posix())

    print("elm-test'ing...")
    subprocess.check_call(['elm-test'], cwd=root_dir.as_posix())

    print("All precommit checks succeeded.")


if __name__ == "__main__":
    main()
