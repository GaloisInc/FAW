import abc
import dataclasses
from typing import Optional, Sequence

import numpy as np
import numpy.typing as npt


@dataclasses.dataclass
class PartitioningSettings:
    min_feature_samples: int = 1  # TODO don't need to consider this here since it's covered in the main file
    restrict_to_target: bool = False


@dataclasses.dataclass
class Dialect:
    feature_index: int
    negated: bool


@dataclasses.dataclass
class CompressedArray:
    array: npt.NDArray[np.bool_]
    row_copies: npt.NDArray[np.int_]
    """Counts/weights of each row in compressed array"""
    col_copies: npt.NDArray[np.int_]
    """Counts/weights of each col in compressed array"""
    row_reconstruct_indices: npt.NDArray[np.int_]
    """Indices to reconstruct original rows from compressed"""
    col_reconstruct_indices: npt.NDArray[np.int_]
    """Indices to reconstruct original cols from compressed"""


class PartitioningAlgorithm(metaclass=abc.ABCMeta):

    @staticmethod
    @abc.abstractmethod
    def dialects_of(
        *,
        target_feature_indices: Optional[Sequence[int]],
        file_features: npt.NDArray[np.bool_],  # features x files
        settings: PartitioningSettings,
    ) -> Sequence[Sequence[Dialect]]:  # return type doesn't include info like entropy, since we can compute that later
        pass

    @staticmethod
    def compress_array(file_features: npt.NDArray[np.bool_]) -> CompressedArray:
        compressed_rows, row_reconstruct_indices, row_copies = np.unique(
            file_features, axis=0, return_inverse=True, return_counts=True
        )
        compressed_both, col_reconstruct_indices, col_copies = np.unique(
            compressed_rows, axis=1, return_inverse=True, return_counts=True
        )
        return CompressedArray(
            compressed_both,
            row_copies=row_copies,
            col_copies=col_copies,
            row_reconstruct_indices=row_reconstruct_indices,
            col_reconstruct_indices=col_reconstruct_indices,
        )

