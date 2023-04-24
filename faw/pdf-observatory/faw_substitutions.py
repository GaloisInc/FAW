"""Argument substitutions for parsers and plugins,
and artifact interdependency inference.
"""
import dataclasses
import functools
import json
import os
import pathlib
import shlex
import tempfile
from typing import Callable, Iterable, Optional, Set, Dict, List, Any, Sequence, Tuple, Union

_ARTIFACT_INPUT_PREFIX = 'artifactInParserDirs'
_ARTIFACT_OUTPUT_DIR_PREFIX = 'artifactOutDir'
_ARTIFACT_OUTPUT_FILE_PREFIX = 'artifactOutFile'


@dataclasses.dataclass(frozen=True)
class ArtifactTypes:
    input_artifact_types: Set[str]
    output_artifact_types: Set[str]


@dataclasses.dataclass(frozen=True)
class _ArgumentSubstitution:
    """Defines a substitution of ``<key [parameters...]>`` for arbitrary text."""
    key: str
    substitute: Callable[..., str]
    """Function from (str) substitution parameters to result.

    Number of arguments must match param count.
    """
    param_count: Tuple[int, ...] = (0,)
    """All valid parameter counts."""


def artifact_types(command: Iterable[str]) -> ArtifactTypes:
    """Return all artifact types a parser requires/outputs"""
    input_artifact_types: Set[str] = set()
    output_artifact_types: Set[str] = set()
    for argument in command:
        if argument.startswith('<') and argument.endswith('>'):
            elements = shlex.split(argument[1:-1])
            if elements[0] == _ARTIFACT_INPUT_PREFIX:
                input_artifact_types.add(elements[1])
            elif elements[0] == _ARTIFACT_OUTPUT_DIR_PREFIX:
                output_artifact_types.add(elements[1])
            elif elements[0] == _ARTIFACT_OUTPUT_FILE_PREFIX:
                output_artifact_types.add(elements[1])
    return ArtifactTypes(
        input_artifact_types=input_artifact_types,
        output_artifact_types=output_artifact_types,
    )


def subsitute_arguments(
    command: Iterable[str],
    substitutions: Iterable[_ArgumentSubstitution],
) -> Iterable[str]:
    substitutions_by_key = {sub.key: sub for sub in substitutions}
    command_substituted: List[str] = []
    for arg in command:
        if arg.startswith('<') and arg.endswith('>'):
            elements = shlex.split(arg[1:-1])
            if elements and (substitution := substitutions_by_key.get(elements[0])) is not None:
                if len(elements) - 1 in substitution.param_count:
                    try:
                        command_substituted.append(substitution.substitute(*elements[1:]))
                    except TypeError:
                        print(substitution, elements, flush=True)
                        raise
                else:
                    raise ValueError(
                        'Wrong number of parameters to substitution '
                        f'{elements[0]} (expected any of {substitution.param_count}; '
                        f'was {len(elements) - 1})'
                    )
            else:
                raise ValueError(
                    f'Unrecognized or invalid argument substitution key: {arg}'
                )
        else:
            command_substituted.append(arg)
    return command_substituted


def common_substitutions(
    api_info: Dict[str, Any],
    temp_root: str,
) -> Sequence[_ArgumentSubstitution]:
    """Substitutions common to all tools.

    <apiInfo>
    <tempFile [...]>
    <tempPrefix [...]>
    <tempDir [...]>
    """
    return [
        _ArgumentSubstitution(
            key='apiInfo',
            substitute=lambda: json.dumps(api_info),
        ),
        _ArgumentSubstitution(
            key='tempFile',
            param_count=(0, 1),
            substitute=functools.partial(
                _temp_file_substitution,
                temp_root=temp_root,
            ),
        ),
        _ArgumentSubstitution(
            key='tempPrefix',
            param_count=(0, 1),
            substitute=functools.partial(
                _temp_file_substitution,  # tempPrefix is functionally identical to tempFile
                temp_root=temp_root,
            ),
        ),
        _ArgumentSubstitution(
            key='tempDir',
            param_count=(0, 1),
            substitute=functools.partial(
                _temp_dir_substitution,
                temp_root=temp_root,
            ),
        ),
    ]


def _temp_file_substitution(suffix: str = None, *, temp_root: str) -> str:
    temp_file = tempfile.NamedTemporaryFile(
        suffix=suffix, delete=False, dir=temp_root
    )
    temp_file.close()
    return temp_file.name


def _temp_dir_substitution(suffix: str = None, *, temp_root: str) -> str:
    return tempfile.mkdtemp(suffix=suffix, dir=temp_root)


def parser_or_file_plugin_substitutions(
    filename: str,
    artifacts_root_dir: pathlib.Path,
) -> Iterable[_ArgumentSubstitution]:
    """File-specific substitutions for parsers and file plugins.

    <inputFile>
    <artifactInParserDirs ...>
    """
    return [
        _ArgumentSubstitution(
            key='inputFile',
            substitute=lambda: filename,
        ),
        _ArgumentSubstitution(
            key=_ARTIFACT_INPUT_PREFIX,
            param_count=(1,),
            substitute=functools.partial(
                _substitute_artifact_input,
                artifacts_root_dir=artifacts_root_dir
            ),
        ),
    ]


def parser_substitutions(
    artifacts_root_dir: pathlib.Path,
    parser_name: str,
) -> Iterable[_ArgumentSubstitution]:
    """Substitutions for parsers only.

    <artifactOutDir ...>
    <artifactOutFile ... [...]>
    """
    return [
        _ArgumentSubstitution(
            key=_ARTIFACT_OUTPUT_DIR_PREFIX,
            param_count=(1,),
            substitute=functools.partial(
                _substitute_artifact_output_dir,
                artifacts_root_dir=artifacts_root_dir,
                parser_name=parser_name,
            ),
        ),
        _ArgumentSubstitution(
            key=_ARTIFACT_OUTPUT_FILE_PREFIX,
            param_count=(1, 2),
            substitute=functools.partial(
                _substitute_artifact_output_file,
                artifacts_root_dir=artifacts_root_dir,
                parser_name=parser_name,
            ),
        ),
    ]


def _substitute_artifact_input(
    artifact_type: str, *, artifacts_root_dir: pathlib.Path
) -> str:
    artifact_dir = artifacts_root_dir / artifact_type
    artifact_dir.mkdir(exist_ok=True)
    return os.fspath(artifact_dir)


def _substitute_artifact_output_dir(
    artifact_type: str, *, artifacts_root_dir: pathlib.Path, parser_name: str
) -> str:
    parser_artifact_subdir = artifacts_root_dir / artifact_type / parser_name
    parser_artifact_subdir.mkdir(parents=True, exist_ok=True)
    return os.fspath(parser_artifact_subdir)


def _substitute_artifact_output_file(
    artifact_type: str, artifact_filename: Optional[str] = None,
    *, artifacts_root_dir: pathlib.Path, parser_name: str
) -> str:
    parser_artifact_subdir = artifacts_root_dir / artifact_type / parser_name
    parser_artifact_subdir.mkdir(parents=True, exist_ok=True)
    return os.fspath(
        parser_artifact_subdir
        / (artifact_filename if artifact_filename is not None else parser_name)
    )


def plugin_substitutions(
    *,
    json_args: Dict[str, Any],
    get_output_html_filename: Callable[[], str],
    files_path: str,
    mongodb_url: str,
    workbench_api_url: str,
) -> Sequence[_ArgumentSubstitution]:
    """Substitutions common to all plugins.

    <jsonArguments>
    <outputHtml>
    <filesPath>
    <mongo>
    <workbenchApiUrl>
    """
    return [
        _ArgumentSubstitution(
            key='jsonArguments',
            substitute=lambda: json.dumps(json_args),
        ),
        _ArgumentSubstitution(
            key='outputHtml',
            substitute=get_output_html_filename,
        ),
        _ArgumentSubstitution(
            key='filesPath',
            substitute=lambda: files_path,
        ),
        _ArgumentSubstitution(
            key='mongo',
            substitute=lambda: mongodb_url,
        ),
        _ArgumentSubstitution(
            key='workbenchApiUrl',
            substitute=lambda: workbench_api_url,
        ),
    ]
