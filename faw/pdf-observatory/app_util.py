
import json


def config_load(config_path):
    """Load config and munge in pipeline plugins (like parsers) into the core
    config.
    """
    cfg = json.load(open(config_path))
    # Pipelines now integrated dynamically as part of various API endpoints.
    # Was necessary due to analysis sets.
    return cfg

