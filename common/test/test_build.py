import pytest

import os
import shutil
import subprocess
import tempfile

faw_dir = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

@pytest.fixture(scope='module')
def faw_in_empty_parent():
    d = tempfile.mkdtemp()
    shutil.copytree(faw_dir, os.path.join(d, 'faw'),
            ignore = lambda src, names: [n for n in names if n == '.git'])
    yield os.path.join(d, 'faw')
    shutil.rmtree(d)


def test_build(faw_in_empty_parent):
    """Test a build of the basic PDF distribution.
    """
    subprocess.check_call(['python3', 'workbench.py', 'pdf', 'build/pdf'],
            cwd=faw_in_empty_parent)


def test_build_parent(faw_in_empty_parent):
    """Test a build of the PDF distribution, copied into a parent folder. One
    level of parent folders are allowed for e.g. private distributions.
    """
    cfg_dir = os.path.join(os.path.dirname(faw_in_empty_parent), 'bleep')
    shutil.copytree(os.path.join(faw_in_empty_parent, 'pdf'), cfg_dir)
    with open(os.path.join(cfg_dir, 'config.json5')) as f:
        contents = f.read()
    s = "name: 'galois-workbench-pdf'"
    assert s in contents, 'Needs contents to test: ' + s
    contents2 = contents.replace(s, "name: 'galois-test-bleep'")
    with open(os.path.join(cfg_dir, 'config.json5'), 'w') as f:
        f.write(contents2)

    subprocess.check_call(['python3', 'workbench.py', cfg_dir, 'build/pdf'],
            cwd=faw_in_empty_parent)


def test_build_parent_parent(faw_in_empty_parent):
    """Test a build two folders up, which should error."""
    grandparent = os.path.dirname(os.path.dirname(faw_in_empty_parent))
    shutil.copy2(os.path.join(faw_in_empty_parent, 'pdf', 'config.json5'),
            os.path.join(grandparent, 'config.json5'))
    p = subprocess.Popen(['python3', 'workbench.py', grandparent, 'build/pdf'],
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            cwd=faw_in_empty_parent)
    stdout, stderr = p.communicate()
    assert p.wait() != 0
    ex = b'Only one parent directory allowed to reach distribution folder'
    assert ex in stdout

